# using the harmonized output dataset

suppressPackageStartupMessages({
  library("data.table")
  library("ggplot2")
  library("readr")
  library("cowplot")
  library("osfr")
})
invisible(lapply(list.files(here::here("R"), full.names = TRUE), source))

# download input.DB as zip file and read in 
dt5_ori <- download_covid(data = "Output_5", temp = TRUE, 
                          verbose = FALSE, progress = FALSE, return = "data.table") 
dt5 <- clean_outputDB(dt5_ori)


# country-specific plot ---- 
all_countries <- get_cnames(dt5)
cname0 <- all_countries[1]
cname0 <- "Nepal"
p1 <- make_country_plot(cname0 = cname0, dt1 = dt5)
if(!dir.exists("fig_harmonized5/country/")) dir.create("fig_harmonized5/country", recursive = TRUE)
ggsave(p1, filename = paste0("fig_harmonized5/country/", cname0, ".png"), width = 11, height = 4)

# a wrapped function to save a group of countries by supplying country names, results will be saved as one pdf or png file  
save_country_plot_in_one(cnames = all_countries, png_or_pdf = "pdf", dt1 = dt5,
                         file_name = "fig_harmonized5/MPIDR_Countries_wCFR")

# aggregated plots for all countries ---- 
data_total1 <- rbindlist(lapply(all_countries, get_dt_for_total, 
                                data = dt5,
                                target_interval = seq(0, 90, by = 5),
                                get_f_m = FALSE))
# a three-panel plot (Case, Death, CFR) for a specific given interval
g1 <- plot_aggregated_total(data_total1)
g1
# interval by 5
g_list1 <- plot_aggregated_total_wrap(
  data = dt5, 
  max_interval = 90, 
  by_interval = 5,
  one_row = FALSE, # 2 rows if FALSE, 1st row is total, 2nd row is sex-specific
  folder = "fig_harmonized5/aggregated")

