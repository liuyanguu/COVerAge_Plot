# script for `taskscheduleR`
# need to `setwd`

suppressPackageStartupMessages({
  library("data.table")
  library("ggplot2")
  library("readr")
  library("cowplot")
  library("osfr")
})
setwd("C:/Users/lyhel/Dropbox/UNICEF_Work_Project/200421_COVID19/COVerAGE-DB-Plot")
invisible(lapply(list.files("R", full.names = TRUE), source))

dtJHU <- get.JHU.daily()
dtJHU2 <- process_dtJHU(dtJHU) # dtJHU2 is not agg by country yet

dt5_ori <- load_fresh(backup = TRUE)
dt5 <- clean_outputDB(dt5_ori) # dt5 has only latest day 
countries_w_new_update <- dt5[Date>"2020-10-01", unique(Country)]
all_countries <- get_cnames(dt5) # all countries in the datasets
# range of `max_date`
dt_date <- unique(dt5[,.(Country, max_date)])
  
# make dt5_country (MPIDR by country and age and sex) for Tableau 
dt5_country <- get_dt5_pool_sex(dt5) # dt5_country has all the countries, pooled sex into total and remove NA value
dt5_country[, New:=ifelse(Country%in%countries_w_new_update, 1, 0)]
dt5_country <- join_region(dt5_country) # joined to region 

dt5_country <- join_wpp_pop_dth(dt5_country)
# dt5_country[, update_date:= format.Date(dt5_ori$update_date[1], "%d %B %Y")]
dt5_country[, update_date:= as.Date(dt5_ori$update_date[1], format = "%d %B %Y")]

dt5_country_colorder <- readRDS("data/dt5_country_colorder.rds")
setcolorder(dt5_country, dt5_country_colorder)
dt5_country[,.N]
# 3/1: 8419

dt5_country[Country%like%"United States" & Measure=="Cases", sum(Value)]

# agg country and age get total
# dt5_total <- get_sum_country_age(dt5_country)
dt5_summary_simple <- get_summary_table(dt5_country)
dt5_summary_simple[, Pcent_0_9:= (`Age 0-4` + `Age 5-9`) / Sum0_19 * 100] # 0-10 % in 0-20
dt5_summary_simple[, Pcent_10_19:= (`Age 10-14` + `Age 15-19`) / Sum0_19 * 100]
dt5_summary_simple

# agg by WB region
dt5_summary_region <- get_summary_table_region(dt5_country, dtJHU2)
dt5_summary_region[1:6,]
# 
# aggregate at country level with JHU country data 
dt5_summary <- get_summary_table_region2(dt5_country, dtJHU2)
dt5_summary[,.N]
# 744
fwrite(dt5_summary_simple, (paste0("data_backup/dt5_summary_simple_", Sys.Date() ,".csv")))
fwrite(dt5_summary, (paste0("data_backup/dt5_summary_", Sys.Date() ,".csv")))
fwrite(dt5_country, (paste0("data_backup/dt5_country_", Sys.Date() ,".csv")))


# write to google sheets
library("googlesheets4")
gs4_auth(email = "yang.liu.uu@gmail.com")

# country agg w JHU
ss1 <- "https://docs.google.com/spreadsheets/d/12F9gRkMNCD-TIydtMmn4f_1zZaJfnuB2kFjxI4IEjbU/edit#gid=0"
googlesheets4::sheet_write(dt5_summary, ss = ss1, sheet = "Sheet1")

# by country and age and sex
ss2 <- "https://docs.google.com/spreadsheets/d/1a3VNCzpmjQ4_d3R_6yVdIpChO8IOG_ETKOyudlWsaIo/edit#gid=0"
googlesheets4::sheet_write(dt5_country, ss = ss2, sheet = "Sheet1")

dir_dropbox_output <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/UNICEF Work/COVID-19/COVerAge-DB-Plot")
fwrite(dt5_summary, file.path(dir_dropbox_output, "output/MPIDR_dt5_country_summary.csv"))
fwrite(dt5_country, file.path(dir_dropbox_output, "output/MPIDR_dt5_country.csv"))
