# functions for rmd file 



# pooled by 
get_dt5_pool_sex <- function(dt5, remove_single_sex = TRUE){
  dt5_country_1 <- pool_sex(dt5)
  dt5_country_2 <- pool_sex(dt5, sex_specific = TRUE)
  dt5_country <- rbindlist(list(dt5_country_1, dt5_country_2), use.names = TRUE)
  # adjust country names now:
  # dt5_country$Country_new <- revise_map_name(dt5_country$Country)
  # N answers for how many countries have cases/deaths
  dt5_country[, Sex2:= ifelse(Sex=="Both", "Both", "Sex-specific")]
  dt5_country[, Measure2 := paste(sort(unique(Measure)), collapse = "_"), by = .(Sex2, Country)]
  dt5_country[, nc:= uniqueN(Country), by = .(Measure, Sex2)] 
  
  if(remove_single_sex){
    dt5_country[Sex!="Both", nsex:= uniqueN(Sex), by = .(Country, Measure)]
    message("Remove single-sex part of country with only female or male:",
    paste(dt5_country[nsex==1, unique(Country)], collapse = ", ")
    )
    dt5_country <- dt5_country[nsex!=1|is.na(nsex)]
    dt5_country[, nsex:=NULL]
    
  }
  return(dt5_country)
}

#' sum all age and all country 
#' @return dataset by measure, sex 
get_sum_country_age <- function(dt5_country){
  dtt <- copy(dt5_country)
  dtt[, Value:= sum(Value), by = .(Measure, Sex)]
  dtt <- unique(dtt[,.(Measure, Sex, Value)])
  dtt <- factor.Measure(dtt)
  dtt <- factor.sex(dtt)
  return(dtt)
}

#' sum all country, 
#' @return dataset by age, measure, sex 
get_sum_country <- function(dt5_total0, age_max = 90){
  dt5_total <- copy(dt5_total0)[!is.na(Value)]
  dt5_total[, Age:=as.numeric(as.character(Age))]
  dt5_total[Age>=age_max, Age:=age_max]
  dt5_total[, Age:=ordered(Age, levels = as.character(seq(0, 90, 5)))]
  
  dt5_total[, Value:= sum(Value), by = .(Measure, Sex, Age)]
  dt5_total <- unique(dt5_total[,.(Measure, Sex, Age, Value)])
  dt5_total[, pcnt:= round(Value/sum(Value, na.rm = TRUE)*100, 1), by = .(Measure, Sex)]
  dt5_total[, label:=paste0(prettyNum(Value, big.mark = ","), "\n(", pcnt, "%)")]
  dt5_total
}

# sum all country by region
get_sum_country_region <- function(dt5_total0, 
                                   region_name = "WBRegion4",
                                   age_max = 90){
  dt5_total <- copy(dt5_total0)
  dt5_total[, Age:=as.numeric(as.character(Age))]
  dt5_total[Age>=age_max, Age:=age_max]
  dt5_total[, Age:=ordered(Age, levels = as.character(seq(0, 90, 5)))]
  
  dt5_total[, Value:= sum(Value), by = c(region_name, "Measure", "Sex", "Age")]
  dt5_total <- unique(dt5_total[, c(region_name, "Measure", "Sex", "Age", "Value"), with = FALSE])
  dt5_total[, pcnt:= round(Value/sum(Value, na.rm = TRUE)*100, 1), by = c(region_name, "Measure", "Sex")]
  dt5_total[, label:=paste0(prettyNum(Value, big.mark = ","), "\n(", pcnt, "%)")]
  setnames(dt5_total, region_name, "Region")
}

#' make summary table
#' 
get_summary_table <- function(dt5_country, 
                              cname = NULL){
  dtJHU0 <- copy(dtJHU)
  dt5_country <- copy(dt5_country)[!is.na(Measure)]
  if(!is.null(cname)){
    if(length(cname)>1){
      message("Only take one country as input")
      cname = cname[1]
    }
    dt_sum <- dt5_country[Country%in%cname]
    setnames(dt_sum, "Value", "MPIDR")
  } else {
    dt_sum <- dt5_country[, .(MPIDR = sum(Value)), by = .(Measure, Sex, Age)]
    
  }
  # dcast Age to wide
  dt_sum_w <- dcast.data.table(dt_sum, Measure + Sex ~ Age, value.var = "MPIDR")
  dt_sum_w[, MPIDR:= rowSums(.SD), .SDcols = as.character(seq(0, 90, 5))]
  dt_sum_w[, Sum0_4 := `0`] # round to .1
  dt_sum_w[, Pcnt0_4 := round(`0`/MPIDR*100, 1)] # round to .1
  dt_sum_w[, Sum0_19:= rowSums(.SD), .SDcols = as.character(seq(0, 15, 5))]
  dt_sum_w[, Pcnt0_19 := round(Sum0_19/MPIDR*100, 1)]
  dt_sum_w[Measure == "Cases", `Age_0_4` := paste0(prettyNum(Sum0_4, big.mark = ","),
                                              " (", round(Pcnt0_4), "%)")] # round to .
  dt_sum_w[Measure == "Deaths", `Age_0_4` := paste0(prettyNum(Sum0_4, big.mark = ","),
                                               " (", Pcnt0_4, "%)")]
  dt_sum_w[Measure == "Cases", `Age_0_19` := paste0(prettyNum(Sum0_19, big.mark = ","),
                                                    " (", round(Pcnt0_19), "%)")] # round to .
  dt_sum_w[Measure == "Deaths", `Age_0_19` := paste0(prettyNum(Sum0_19, big.mark = ","),
                                                     " (", Pcnt0_19, "%)")]
  if(!is.null(cname)) dtJHU0 <- dtJHU0[Country_Region == cname]
  dt_sum_w[Measure=="Cases" & Sex=="Both", `JHU`:=sum(dtJHU0$Confirmed, na.rm = TRUE)]
  dt_sum_w[Measure=="Deaths" & Sex=="Both", `JHU`:=sum(dtJHU0$Deaths, na.rm = TRUE)]
  dt_sum_w[!is.na(JHU), `MPIDR_JHU`:= round(`MPIDR`/`JHU`*100)]
  dt_sum_w[!is.na(JHU), `MPIDR_JHU`:= round(`MPIDR`/`JHU`*100)]
  setnames(dt_sum_w, as.character(seq(0, 15, 5)), paste("Age", c("0-4", "5-9", "10-14", "15-19")))
  vars_wanted <- c("Measure", "Sex", "JHU", "MPIDR", "MPIDR_JHU", 
                   paste("Age", c("0-4", "5-9", "10-14", "15-19")),
                   "Age_0_4", "Age_0_19", # This is Age with (%)
                   "Sum0_4", "Pcnt0_4", "Sum0_19", "Pcnt0_19")
  dtt <- dt_sum_w[,..vars_wanted]
  # Table Cases
  # cols1 <- c("JHU", "MPIDR", paste("Age", c("0-4", "5-9", "10-14", "15-19")))
  # dtt[, (cols1) := lapply(.SD, prettyNum, big.mark = ","), .SDcols = cols1]
  dtt[JHU=="NA", JHU:=""]
  dtt[Sex=="Both", Sex := "Total"]
  setnames(dtt, "MPIDR", "COVerAGE-DB")
  setnames(dtt, "MPIDR_JHU", "COVerAGE-DB/JHU(%)")
  return(dtt)
}

# aggregate by WB region
get_summary_table_region <- function(dt5_country, dtJHU2){
  
  dt_sum <- copy(dt5_country)[!is.na(Measure), .(MPIDR = sum(Value)), by = .(WBRegion4, Measure, Sex, Age, nc_MPIDR_WB)]
  # dcast Age to wide
  dt_sum_w <- dcast.data.table(dt_sum, WBRegion4 + nc_MPIDR_WB + Measure + Sex ~ Age, value.var = "MPIDR")
  dt_sum_w[, MPIDR:= rowSums(.SD), .SDcols = as.character(seq(0, 90, 5))]
  dt_sum_w[, Sum0_4 := `0`] # round to .1
  dt_sum_w[, Pcnt0_4 := round(`0`/MPIDR*100, 1)] # round to .1
  dt_sum_w[, Sum0_19:= rowSums(.SD), .SDcols = as.character(seq(0, 15, 5))]
  dt_sum_w[, Pcnt0_19 := round(Sum0_19/MPIDR*100, 1)]
  setkey(dt_sum_w, WBRegion4, Sex, Measure)
  
  # sum JHU by region
  dtJHU_sum <- copy(dtJHU2)[, `:=`(Cases = sum(Confirmed, na.rm = TRUE), 
                             Deaths = sum(Deaths, na.rm = TRUE)), by = .(WBRegion4)]
  dtJHU_sum <- unique(dtJHU_sum[,.(WBRegion4, nc_WB, Cases, Deaths)])
  dtJHU_sum2 <- melt.data.table(dtJHU_sum, measure.vars = c("Cases","Deaths"),
                     variable.name = "Measure", value.name = "JHU", variable.factor = FALSE)
  dtJHU_sum2[, Sex:= "Both"]
  setkey(dtJHU_sum2, WBRegion4, Sex, Measure)
  
  # sum JHU corresponding countries (available in MPIDR)  
  dtJHU3 <- copy(dtJHU2)[ISO3Code%in%dt5_country$ISO3Code,]
  dtJHU_sum_c <- dtJHU3[, `:=`(Cases = sum(Confirmed, na.rm = TRUE), 
                             Deaths = sum(Deaths, na.rm = TRUE)), by = .(WBRegion4)]
  dtJHU_sum_c <- unique(dtJHU_sum_c[,.(WBRegion4, Cases, Deaths)])
  dtJHU_sum_c2 <- melt.data.table(dtJHU_sum_c, measure.vars = c("Cases","Deaths"),
                                variable.name = "Measure", value.name = "JHU_matched", variable.factor = FALSE)
  dtJHU_sum_c2[, Sex:= "Both"]
  setkey(dtJHU_sum_c2, WBRegion4, Sex, Measure)
  
  # merge into one
  dt_sum_w <- dtJHU_sum2[dtJHU_sum_c2][dt_sum_w]
  
  dt_sum_w[!is.na(JHU), `MPIDR_JHU`:= round(`MPIDR`/`JHU`*100)]
  setnames(dt_sum_w, as.character(seq(0, 15, 5)), paste("Age", c("0-4", "5-9", "10-14", "15-19")))
  vars_wanted <- c("WBRegion4", "nc_WB", "nc_MPIDR_WB", "Measure", "Sex", 
                   "JHU", "MPIDR",  "MPIDR_JHU",
                   paste("Age", c("0-4", "5-9", "10-14", "15-19")),
                   "Sum0_4", "Pcnt0_4", "Sum0_19", "Pcnt0_19")
  dtt <- dt_sum_w[,..vars_wanted]
  dtt[Sex=="Both", Sex := "Total"]
  setnames(dtt, "MPIDR", "COVerAGE-DB")
  return(dtt)
}

#' aggregate table by country, merged to JHU results, with WB region for Tableau 
#'
#' @param dt5_country 
#' @param dtJHU2 
#' @param combine_sex use Sex2 (only Both and Sex-specific) if TRUE
#'
get_summary_table_region2 <- function(dt5_country, dtJHU2, combine_sex = TRUE){
  
  # dcast Age to wide, aggregate f/m into sex-specific so I can still compare to JHU
  dt5_country_1 <- copy(dt5_country)[!is.na(Value) & !is.na(Measure)]
  if(combine_sex){
    dt5_country_1[, Value:= sum(Value), by = .(ISO3Code, Measure, Sex2, Age)]
    dt5_country_1 <- unique(dt5_country_1[,.(ISO3Code, Measure, Sex2, Age, Value)])
    setnames(dt5_country_1, "Sex2", "Sex")
    
  } else {
    dt5_country_1[, Value:= sum(Value), by = .(ISO3Code, Measure, Sex, Age)]
    dt5_country_1 <- unique(dt5_country_1[,.(ISO3Code, Measure, Sex, Age, Value)])
  }
  
  dt_sum_w <- dcast.data.table(dt5_country_1, ISO3Code  + Measure + Sex ~ Age, value.var = "Value")
  dt_sum_w[, MPIDR:= rowSums(.SD, na.rm = TRUE), .SDcols = as.character(seq(0, 90, 5))]
  dt_sum_w[, Sum0_4 := `0`] # round to .1
  dt_sum_w[, Pcnt0_4 := (`0`/MPIDR)] 
  dt_sum_w[, Sum0_19:= rowSums(.SD, na.rm = TRUE), .SDcols = as.character(seq(0, 15, 5))]
  dt_sum_w[, Pcnt0_19 := (Sum0_19/MPIDR)]
  setkey(dt_sum_w, ISO3Code, Measure, Sex)
  
  # add JHU  
  dtJHU_long <- get_JHU_long(dtJHU2, combine_sex = combine_sex)
  setkey(dtJHU_long, ISO3Code, Measure, Sex)
  dt_sum_w <- dt_sum_w[dtJHU_long]
  # dt_sum_w[, sum(MPIDR, na.rm = T), by = Measure]
  
  dt_sum_w[, `MPIDR_JHU`:= round(`MPIDR`/`JHU`*100)]
  setnames(dt_sum_w, as.character(seq(0, 15, 5)), paste("Age", c("0-4", "5-9", "10-14", "15-19")))
  setnames(dt_sum_w, "MPIDR", "COVerAGE-DB")
  
  setkey(dt_sum_w, ISO3Code)
  country_info <- readRDS("data/dt_info.rds")
  country_info[, UNICEF_region:= ifelse(UNICEFReportRegion2 == "", UNICEFReportRegion1, UNICEFReportRegion2)]
  country_info[, SDG_region:= ifelse(SDGSimpleRegion1 != "Oceania", SDGSimpleRegion1, SDGSimpleRegion2)]
  setkey(country_info, ISO3Code)
  # use the official name 
  country_info[, Country:=OfficialName]
  
  dt_sum_w <- dt_sum_w[country_info[,.(ISO3Code, Country, WBRegion4, UNICEF_region)]]
  
  return(dt_sum_w[!is.na(Measure)])
}

# compare with the last dataset (country summary )
get_dt5_summary_compare <- function(dt5_summary, dt5_country){
  dt5_summary_old <- fread(get_dir_latest_file("data_backup", "dt5_summary"))[,.(ISO3Code, Measure, Sex, `COVerAGE-DB`, JHU)]
  suppressMessages({
  date_old <- gsub("dt5_summary_|.csv", "", get_dir_latest_file("data_backup", "dt5_summary", short = TRUE))
  })
  date_old <- as.Date(date_old)
  setkey(dt5_summary_old, ISO3Code, Measure, Sex)
  setnames(dt5_summary_old, c("COVerAGE-DB", "JHU"), paste0(c("COVerAGE-DB", "JHU"), "old"))
  setkey(dt5_summary, ISO3Code, Measure, Sex)
  setkey(dt5_summary_old, ISO3Code, Measure, Sex)
  dt_compare <- dt5_summary[,.(ISO3Code, Country, Measure, Sex, `COVerAGE-DB`, JHU)][dt5_summary_old]
  dt_compare[, `:=`(diff = `COVerAGE-DB` - `COVerAGE-DBold`)]
  dt_compare[, `:=`(pcnt = (diff)/`COVerAGE-DBold`*100)]
  dt_compare[, Date_old_data:= date_old]
  dt_compare <- dt_compare[!is.na(`COVerAGE-DB`)]
  dt5_country2 <- copy(dt5_country)
  dt5_country2[Sex!="Both", Sex:= "Sex-specific"]
  dt_compare <- merge(dt_compare, unique(dt5_country2[,.(ISO3Code, Measure, Sex, max_date)]))
  setorder(dt_compare, Sex, pcnt, Country)
  fwrite(dt_compare, paste0("data_backup/check/dt5_summary_compare_", format(Sys.Date(), "%Y%m%d"),
                            "_vs_", date_old))
  return(dt_compare)
}

# 
print_table <- function(data){
  DT::datatable(data, rownames = FALSE, 
                colnames = colnames(data),
                options = list(dom = 't')
  )
}
print_table_pdf <- function(dt){
  kbl(dt, booktabs = TRUE) %>%
    kable_styling(latex_options = "striped")
}

# data processing 
# the data is cumulative by date, so sometimes subset the latest date
subset.latest.date <- function(dt){
  dt[, max_date:= max(Date), by = Country]
  dt <- dt[Date==max_date]
  dt[, max_date:=NULL]
  return(dt)  
}

#' Load MPIDR output 5-year interval
#' the new function to load data for the dashboard
load_fresh <- function(backup = FALSE){
  dt5_ori <- download_covid(data = "Output_5", temp = TRUE,
                            verbose = FALSE, progress = FALSE, return = "data.table")
  dt5_ori[, update_date:= format(Sys.Date(), format="%d %B %Y")]
  if(backup){
    if(!dir.exists("data_backup")) dir.create("data_backup")
    fwrite(dt5_ori, "data_backup/dt5_ori.csv")
    fwrite(dt5_ori, paste0("data_backup/Output_5_", Sys.Date() ,".csv"))
  }
  return(dt5_ori)
}


#' JHU time series, get both cases and deaths 
#' @return long-format data 
get.JHU.ts <- function(){
  url_dth <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  url_case <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  dt_JUH_ts <- rbindlist(list(fread(url_dth)[, Measure := "Deaths"], fread(url_case)[, Measure := "Cases"]))
  dt_JUH_ts[, `Province/State`:=NULL][, Lat:=NULL][, Long:=NULL]
  setnames(dt_JUH_ts, "Country/Region", "Country_Region")
  dt_JUH_ts_l <- melt.data.table(dt_JUH_ts, id.vars = c("Country_Region", "Measure"), variable.name = "Date",
                                 variable.factor = FALSE)
  dt_JUH_ts_l[, Date:= lubridate::mdy(Date)]
  setorder(dt_JUH_ts_l, Country_Region, Measure)
  dt_JUH_ts_l[, value2:= shift(value, n=1, fill=NA, type="lag"), by = .(Country_Region, Measure)]
  dt_JUH_ts_l[, value_daily := value - value2]
  dt_JUH_ts_l <- dt_JUH_ts_l[!is.na(value_daily)]
  return(dt_JUH_ts_l)
}

plot_country_JHU <- function(dtJHU = dtJHU_country, iso3){
  if(!iso3%in%unique(dtJHU$ISO3Code)) stop("ISO not found.")
  g <- ggplot(dtJHU[ISO3Code == iso3], aes(x = Date, y = value_daily)) +
    geom_bar(stat="identity", width=0.5, show.legend = FALSE, color = "#0058AB") + 
    labs(x = "", y = "") + 
    scale_x_date(date_labels = "%Y-%m") +
    scale_y_continuous(expand = c(0,0),
                       breaks = scales::pretty_breaks()) +
    facet_wrap(~Measure, scales = "free_y", ncol = 1) + 
    theme_classic() + 
    theme(legend.position='none')
  return(g)
}

plot_latest_date <- function(dt_date = dt_date){
  dt_date[Sex!="Both", Sex:= "Sex-specific"]
  ggplot(dt_date, aes(x = Month)) +
    geom_bar(stat="count", width=0.5, show.legend = FALSE, color = "#0058AB") + 
    geom_text(stat='count', aes(label=..count..), vjust=-0.2) + 
    labs(x = "", y = "") + 
    # scale_x_date(date_labels = "%Y-%m") +
    facet_wrap(~ Sex,  ncol = 1) + 
    theme_classic() + 
    theme(legend.position='none')
}


# get.JHU.daily -----------------------------------------------------------
#' Get JHU world daily report
#' If URL unavailable, load latest backup dataset
get.JHU.daily <- function(){
  # date0 <- format(Sys.Date(), format = "%m-%d-%Y")
  date1 <- format(Sys.Date()-1, format = "%m-%d-%Y")
  url0 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
  url_daily_report <- paste0(url0, as.character(date1) ,".csv")
  
  if(!dir.exists("data_backup/JHU")) dir.create("data_backup/JHU", recursive = TRUE)
  
  tryCatch({
    dt_JHU <- fread(input = url_daily_report);
    fwrite(dt_JHU, paste0("data_backup/JHU/JHU_daily_report_", Sys.Date()-1,".csv"))
  },
    error = function(e){
          message("URL does not exist, load saved JHU dataset")
          return(dt_JHU <- fread(get_dir_latest_file("data_backup/JHU", "JHU")))
        }
      )
  
  return(dt_JHU)
}


# get latest file
get_dir_latest_file <- function(dir0, pattern0, short = FALSE){
  files_full <- get.file.name(dir_file = dir0, pattern0 = pattern0)
  files <- get.file.name(dir_file = dir0, pattern0 = pattern0, full_name = FALSE)
  file_selected <- if(short)files[find_latest_date(files)] else files_full[find_latest_date(files)]
  if(length(file_selected)!=0){
    message(paste(pattern0, "dataset chosen: \n", file_selected))
    return(file_selected)
  } else {
    message("No dataset found in: \n ", dir0)
    return(NULL)
  }
}

find_latest_date <- function(files){
  remove_string <- c("dt5_country_|.csv|dt5_summary_|Output_5_|JHU_daily_report_")
  dates <- gsub(remove_string, "", files)
  # screen for valid date string:
  # dates <- c("2015", "20200804", "2020-08-01")
  # return which.max e.g. 2L
  get.max.date(dates)
}

get.max.date <- function(mydate) {
  align.date <- function(mydate){
    if(!is.na(as.Date(mydate, "%Y-%m-%d"))){
      mydate <- as.Date(mydate, "%Y-%m-%d")
    } else if (!is.na(as.Date(mydate, "%Y%m%d"))){
      mydate <- as.Date(mydate, "%Y%m%d")
    } else {
      mydate <- NA
    }
    return(mydate)
  }
  out <- sapply(mydate, align.date)
  return(which.max(out))
}

get.file.name <- function(dir_file,
                          pattern0,
                          full_name = TRUE){
  
  if(is.null(dir_file))message("dir_file is NULL. Please double check.")
  if(!dir.exists(dir_file))message("Check if dir_file exists: ", dir_file)
  files <- list.files(dir_file)
  files_full <- list.files(dir_file, full.names = TRUE)
  return(if(full_name)files_full[which(grepl(pattern0, files))] else files[which(grepl(pattern0, files))])
}
#' add ISO3Code and region from info.cme to JHU 
process_dtJHU <- function(dtJHU){
  # dtJHU$ISO3Code <- countrycode::countrycode(dtJHU$Country_Region, origin = "country.name", destination = "iso3c")
  dtJHU_ISO3 <- fread(here::here("data/JHU_ISO3Code_book.csv"))
  dtJHU <- merge(dtJHU, dtJHU_ISO3)
  country_info <- readRDS("data/dt_info.rds")
  vars_wanted <- c("CountryName", "ISO3Code", "WBRegion4", "UNICEFReportRegion1")
  dt_info <- country_info[,..vars_wanted]
  dtJHU2 <- merge.data.table(dt_info, dtJHU, by = "ISO3Code", all.x = TRUE)
  dtJHU2[, nc_WB := uniqueN(ISO3Code), by = WBRegion4]   
  
  return(dtJHU2)
}


# Turn JHU dataset into long, insert Sex == "Sex-specific"
get_JHU_long <- function(dtJHU2, combine_sex = TRUE){
  dtJHU_long <- melt.data.table(dtJHU2, measure.vars = c("Confirmed", "Deaths"),
                                id.vars = "ISO3Code",
                                variable.name = "Measure", value.name = "JHU", variable.factor = FALSE)
  dtJHU_long[Measure=="Confirmed", Measure:="Cases"]
  dtJHU_long[, JHU:= sum(JHU), by = .(ISO3Code, Measure)]
  dtJHU_long <- unique(dtJHU_long)[!is.na(JHU)]
  dtJHU_long1 <- copy(dtJHU_long)[ ,Sex:="Both"]
  if(combine_sex){
    # for Tableau
    dtJHU_long2 <- copy(dtJHU_long)[,Sex:="Sex-specific"]
    return(rbindlist(list(dtJHU_long1, dtJHU_long2)))
  } else {
    # for showing both female and male 
    dtJHU_long2 <- copy(dtJHU_long)[,Sex:="Female"][, JHU:= NA]
    dtJHU_long3 <- copy(dtJHU_long)[,Sex:="Male"][, JHU:= NA]
    return(rbindlist(list(dtJHU_long1, dtJHU_long2, dtJHU_long3)))
  }
}

get_JHU_long_keepNA <- function(dtJHU2){
  dtJHU_long <- melt.data.table(dtJHU2, measure.vars = c("Confirmed", "Deaths"),
                                id.vars = "ISO3Code",
                                variable.name = "Measure", value.name = "JHU", variable.factor = FALSE)
  dtJHU_long[Measure=="Confirmed", Measure:="Cases"]
  dtJHU_long[, JHU:= sum(JHU), by = .(ISO3Code, Measure)]
  dtJHU_long <- unique(dtJHU_long)[!is.na(JHU)]
  dtJHU_long1 <- copy(dtJHU_long)[,Sex:="Both"]
  dtJHU_long2 <- copy(dtJHU_long)[,Sex:="Sex-specific"]
  return(rbindlist(list(dtJHU_long1, dtJHU_long2)))
}


#' pool f and m into Both sex, remove NA Value
#' 
pool_sex <- function(dt5, sex_specific = FALSE){
  dt5 <- copy(dt5)[Region == "All" & Age!="TOT" & !is.na(Value)]
  if(sex_specific) return(dt5[Sex!="Both",.(Country, Measure, Age, Sex, Value, max_date)])
  # measure cases
  dt_cases <- dt5[Measure == "Cases"]
  cnames_case <- dt_cases[Sex=="Both", unique(Country)]
  data_sex_1 <- rbind(dt_cases[Country%in%cnames_case & Sex == "Both"],
                         dt_cases[!Country%in%cnames_case])
  # deaths
  dt_dth <- dt5[Measure == "Deaths"]
  cnames_dth <- dt_dth[Sex=="Both", unique(Country)]
  data_sex_2 <- rbind(dt_dth[Country%in%cnames_dth & Sex == "Both"],
                      dt_dth[!Country%in%cnames_dth])
  
  data_sex <- rbindlist(list(data_sex_1, data_sex_2))
  data_sex2 <- data_sex[, Value:= sum(Value), by = .(Country, Measure, Age)]
  data_sex2 <- unique(data_sex2[,.(Country, Measure, Age, Value, max_date)])
  data_sex2[, Sex:= "Both"]
  return(data_sex2)
}

#' adjust age group
pool_age <- function(
  dt5, 
  target_interval = seq(0, 100, by = 10)
  ){
  Int_Diff <- diff(target_interval)[1]
  suppressWarnings(dt5[, Age := as.numeric(Age)])
  dt5[Age>=max(target_interval), Age:=max(target_interval)]
  
  # sum to 5 year interval if AgeInt is all 1
  dt5_poolAge <- copy(dt5)
  dt5_poolAge[, Age := Age - Age%%Int_Diff]
  dt5_poolAge <- unique(dt5_poolAge[, Value := sum(Value), by = .(Country, Measure, Sex, Age)])
  return(dt5_poolAge)
}

#' get total country
#'
pool_country <- function(dt5){
  dt5_2 <- dt5[, Value:= sum(Value), by = .(Measure, Age)]
  dt5_2 <- unique(dt5_2[,.(Measure, Age, Sex, Value)])
  return(dt5_2)
}

# read in country_info
get_country_info <- function(){
  file1 =  "Dropbox/UN IGME data/2020 Round Estimation/Code/input/country.info.CME.csv"
  # file2 =  "Dropbox/UN IGME data/2020 Round Estimation/Code/input/data_livebirths.csv"
  dc <- fread(file.path(Sys.getenv("USERPROFILE"), file1))
  # dt_lb <- fread(file.path(Sys.getenv("USERPROFILE"), file2))
  dc[, UNICEF_region:= ifelse(UNICEFReportRegion2 == "", UNICEFReportRegion1, UNICEFReportRegion2)]
  dc[, SDG_region:= ifelse(SDGSimpleRegion1 != "Oceania", SDGSimpleRegion1, SDGSimpleRegion2)]
  dc
}


# join region + revised_country_names ---------------------------------------------------


revised_country_names <- list(
  # match output dt to country.info.CME
  "Czechia" = "Czech Republic", 
  "England" = "United Kingdom",           # more updated 2021/1/16
  "England and Wales" = "United Kingdom", # same as United Kingdom: updated on 2021/1/1
  "Northern Ireland"  = "United Kingdom", # no sex
  "Scotland"          = "United Kingdom", # 2021/1/20
  "Palestine" = "State of Palestine", 
  "Eswatini" = "Swaziland", 
  "Gambia" = "Gambia The",
  "Northern Ireland",
  "Scotland",
  "South Korea" = "Korea Rep",
  "UK" = "United Kingdom",  # Case number doesn't look right 
  "USA" = "United States of America"
)

join_region <- function(dt1_ava = dt5_country){
  country_info <- readRDS("data/dt_info.rds")
  country_info[, UNICEF_region:= ifelse(UNICEFReportRegion2 == "", UNICEFReportRegion1, UNICEFReportRegion2)]
  country_info[, SDG_region:= ifelse(SDGSimpleRegion1 != "Oceania", SDGSimpleRegion1, SDGSimpleRegion2)]
  
  vars_wanted <- c("CountryName", "OfficialName", "ISO3Code", "WBRegion4", "UNICEF_region")
  dt_info <- country_info[,..vars_wanted]
  setkey(dt_info, CountryName)
  
  # use United Kingdom's subregions as UK has problem with cases 
  # 2021/1/27
  dt1_ava <- dt1_ava[!Country%in%c("England", "United Kingdom", "UK", "Taiwan")]
  dt1_ava$Country <- get.match(dt1_ava$Country, new_list = revised_country_names)
  dt1_ava[Country=="United Kingdom", New:= 1]
  dt1_ava[Country=="United Kingdom", Measure2:= "Cases_Deaths"]
  
  
  name_missed <- unique(dt1_ava$Country[!dt1_ava$Country%in%dt_info$CountryName])
  if(length(name_missed)>0) message("Check regions from dataset not matched to country_info: ", paste(name_missed, collapse = "; "))
  setkey(dt1_ava, Country)
  dt2 <- dt_info[dt1_ava, nomatch = 0]
  dt2[,.N]
  dt2[,uniqueN(CountryName)]
  
  # countries without data by sex
  dt2_miss1 <- dt_info[!CountryName%in%dt1_ava[Sex2=="Both", unique(Country)],
                       .(CountryName, OfficialName, ISO3Code, WBRegion4, UNICEF_region)]
  dt2_miss1[, Measure2:="No data"]; dt2_miss1[, Sex2:= "Both"]
  dt2_miss2 <- dt_info[!CountryName%in%dt1_ava[Sex2=="Sex-specific", unique(Country)],
                       .(CountryName, OfficialName, ISO3Code, WBRegion4, UNICEF_region)] 
  dt2_miss2[, Measure2:="No data"]; dt2_miss2[, Sex2:= "Sex-specific"]
  dt2 <- rbindlist(list(dt2, dt2_miss1, dt2_miss2), fill = TRUE)

  dt2[, Country:=OfficialName]
  dt2[, OfficialName:= NULL] # keep col order so Tableau can read 
  # setnames(dt2, "OfficialName", "Country") # Country is the official label 
  
  # combine old age group 
  dt2[, Age:=as.numeric(Age)]
  dt2[Age>=90, Age:=90]
  dt2[, Age:=ordered(Age, levels = as.character(seq(0, 90, 5)))]
  
  # sum those combined countries and combined age groups 
  dt2[, Value := sum(Value), by = .(CountryName, Measure, Sex, Age)]
  dt2[, max_date := max(max_date), by = .(CountryName, Measure, Sex, Age)]
  dt2 <- unique(dt2)
  dt2[, CountryName:=NULL]
  # reorder
  dt2$WBRegion4 <- as.factor(dt2$WBRegion4)
  dt2$WBRegion4 <- factor(dt2$WBRegion4, levels = c("Low income",
                                                    "Lower middle income",
                                                    "Upper middle income",
                                                    "High income"))
  dt2[!is.na(Measure2), nc_MPIDR_WB:=uniqueN(Country), by = .(Measure, Sex2, WBRegion4)]
  # 

  
  return(dt2)
}


# join WPP ----------------------------------------------------------------

get_wpp_pop_dth <- function(){
  dt_wpp <- fread(here::here("data/dt_wpp_5_year_2020_Pop.csv"))
  # age
  dt_wpp[, Age:=as.numeric(Age)]
  dt_wpp[Age>=90, Age:=90]
  dt_wpp[, Age:=ordered(Age, levels = as.character(seq(0, 90, 5)))]
  dt_wpp[, Pop2020:= sum(Pop2020), by = .(ISO3Code, Sex, Age)]
  dt_wpp <- unique(dt_wpp)
  setkey(dt_wpp, ISO3Code, Age, Sex)
  
  dt_wpp_dth <- fread(here::here("data/dt_wpp_5_year_dth.csv"))
  # age
  dt_wpp_dth[, Age:=as.numeric(Age)]
  dt_wpp_dth[Age>=90, Age:=90]
  dt_wpp_dth[, Age:=ordered(Age, levels = as.character(seq(0, 90, 5)))]
  dt_wpp_dth[, Dth2020:= sum(Dth2020), by = .(ISO3Code, Sex, Age)]
  dt_wpp_dth <- unique(dt_wpp_dth)
  setkey(dt_wpp_dth, ISO3Code, Age, Sex)
  
  dt_wpp <- dt_wpp[dt_wpp_dth]
  # still want to add country name 
  country_info <- readRDS("data/dt_info.rds")
  dt_wpp <- merge.data.table(dt_wpp, unique(country_info[,.(ISO3Code, CountryName)]),
                             by = "ISO3Code")
  return(dt_wpp)
}

join_wpp_pop_dth <- function(dt5_country){
  dt_wpp <- get_wpp_pop_dth()
  setkey(dt_wpp, ISO3Code, Age, Sex)
  setkey(dt5_country, ISO3Code, Age, Sex)
  dt5_country2 <- dt_wpp[dt5_country]
  dt5_country2
}



#' a label function to relabel certain variable names
#'
#' You can provide a __new_list__ to define the labels. If any label is not
#' provided, the function will just return the original value
#'
#' @param x a element or a vector
#' @param new_list if you supply a new list the function will use instead of the
#'   default_labels
#' @param no_line_break to remove linebreak from the string
#' @export get.match
#' @return updated labels as character vector
get.match <- function(x,
                      new_list = NULL,
                      no_line_break = FALSE){
  if(is.null(new_list)){
    labs <- default_label_1
  } else {
    if(is.list(new_list)){
      labs <- new_list
    } else {
      message("new_list must be a list. Still use the default list.")
      labs <- default_label_1
    }
  }
  if(!is.character(x)){
    message("Coerse input into character.")
    x <- as.character(x)
  }
  out <- rep(NA, length(x))
  for (i in 1:length(x)){
    if (is.null(labs[[ x[i] ]])){
      out[i] <- x[i]
    }else{
      out[i] <- labs[[ x[i] ]]
    }
  }
  return(if(no_line_break)gsub("\n", "", out) else out)
}


# functions for map ---- 

#' revise names to match NE (Natural earth) map
revise_map_name <- function(cnames){
  new_name <- list(
    "England" = "United Kingdom",
    "England and Wales" = "United Kingdom",
    "Northern Ireland"  = "United Kingdom",
    "Scotland"          = "United Kingdom",
    "Swaziland" = "eSwatini",
    "Eswatini" = "eSwatini",
    "United States" = "United States of America",
    "USA" = "United States of America",
    "Dominican Republic" = "Dominican Rep.",
    "Central African Republic" = "Central African Rep.",
    "Equatorial Guinea" = "Eq. Guinea",
    
    # revert back from revised country name to match maps
    "Czech Republic" = "Czechia",
    "Gambia The" = "Gambia",
    "Korea Rep" = "South Korea",
    "State of Palestine" = "Palestine",
    
    "Bolivia (Plurinational State of)" = "Bolivia",
    "Republic of Korea" = "South Korea",
    "Venezuela (Bolivarian Republic of)" = "Venezuela",
    "Republic of Moldova" = "Moldova",
    "Viet Nam" = "Vietnam"
  )
  cnames <- get.match(cnames, new_list = new_name)
  cnames_miss <- sort(unique(cnames[!cnames%in%WorldRobinson$NAME]))
  if(length(cnames_miss)>0)message("Check country names not matched in the NE map: ", 
                                   paste(cnames_miss, collapse = ", "))
  return(cnames)
}


#' make coverage map
plot_ava_map <- function(highlight_cnames){
  
  highlight_cnames <- highlight_cnames[highlight_cnames%in%WorldRobinson$NAME]
  World_Region <- WorldRobinson[WorldRobinson$NAME %in% highlight_cnames,]
  if(length(World_Region)==0) stop("World_Region has length 0 for plotting")
  message("World_Region has " , length(World_Region), " countries")
  
  World_Region_sp <- spTransform(World_Region, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  gWorld <- ggplot() +
    geom_polygon(data = WorldRobinson, aes(x = long, y = lat, group = group), fill="lightgray", colour = "white") +
    geom_polygon(data = World_Region_sp, aes(x = long, y = lat, group = group), fill="#0058AB", colour = "white") +
    # ggrepel::geom_label_repel(data = subnational_centroids, 
    #                           aes(x=long, y=lat, label = name),
    #                           force = 3,
    #                           bg.r = 0.15,
    #                           bg.color = "white",
    #                           inherit.aes = FALSE) + 
    ggthemes::theme_map() 
  return(gWorld)
}

plot_ava_map2 <- function(dt_ava2){
  dt_ava2$Country_new <- revise_map_name(dt_ava2$Country)
  highlight_cnames <- dt_ava2$Country_new
  highlight_cnames <- highlight_cnames[highlight_cnames%in%WorldRobinson$NAME]
  World_Region <- WorldRobinson[WorldRobinson$NAME %in% highlight_cnames,]
  if(length(World_Region)==0) stop("World_Region has length 0 for plotting")
  message("World_Region has " , length(World_Region), " countries")
  
  geo2 <- dplyr::left_join(geo_df,  dt_ava2, by = c("id" = "Country_new"))
  geo2$Measure[is.na(geo2$Measure)] <- "NA"
  cols_data <- c("Cases_Deaths" = "#0058AB",  "Cases" = "#1CABE2", 
                 "Deaths" = "#69DBFF", "NA" = "grey")
  geo2$Measure <- factor(geo2$Measure, levels = names(cols_data))
  
  gWorld <- ggplot() +
    geom_polygon(data = WorldRobinson, aes(x = long, y = lat, group = group), fill="lightgray", colour = "white") +
    geom_polygon(data = geo2[!is.na(geo2$Measure),], aes(x = long, y = lat, group = group, fill = Measure), colour = "white") +
    scale_fill_manual(values = cols_data,
                      labels = c("Cases and deaths", "Cases", "Deaths", "No data"))  + 
    ggthemes::theme_map() + 
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.justification = c("bottom"),
          legend.title = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size=10))
  return(gWorld)
}


# functions for plots -----------------------------------------------------
# barchart compared to JHU
make_barchart1 <- function(dt1){
  dt1$`COVerAGE-DB` <- as.integer(dt1$`COVerAGE-DB`)
  dt11 <- factor.Measure(melt.data.table(dt1, id.vars = "Measure", 
                                         measure.vars = c("JHU", "COVerAGE-DB")), 
                         target_level = c("Cases", "Deaths"))
  bar_compare <- ggplot(dt11, aes(fill = variable, x = Measure, y = value)) + 
    geom_bar(position="dodge", width=0.8, stat="identity") + 
    labs(x = "", y = "", fill = "") +
    scale_fill_manual(values = c("JHU" = "#1CABE2", "COVerAGE-DB" = "#0058AB")) + 
    scale_y_continuous(expand = c(0,0), 
                       labels = scales::comma) + 
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.line.x = element_line(colour = "black"))  
  bar_compare
}

# total for sex 
make_barchart1sex <- function(dt2){
  dt22 <- factor.Measure(melt.data.table(dt2, id.vars = c("Measure",  "Sex"),
                                         measure.vars = c("JHU", "COVerAGE-DB")), 
                         target_level = c("Cases", "Deaths"))
  bar_compare <- ggplot(dt22, aes(fill = Sex, x = Measure, y = value)) + 
    geom_bar(position="dodge", width=0.8, stat="identity") + 
    labs(x = "", y = "", fill = "") +
    scale_fill_manual(values = c("Male" = "#1CABE2", "Female" = "#F26A21")) + 
    scale_y_continuous(expand = c(0,0), 
                       labels = scales::comma) + 
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.line.x = element_line(colour = "black"))  
  bar_compare
}

# the total cases/deaths barcharts
make_barchart2 <- function(dt5_total, Measure0 = NULL, label = TRUE){
  rcs <- c("Low income",
           "Lower middle income",
           "Upper middle income",
           "High income")
  eg <- expand.grid(rcs, c("Cases", "Deaths"))
  if("Region"%in%colnames(dt5_total)) {
    dt5_total[, R_M := paste(Region, Measure)]
    dt5_total[, R_M:= factor(R_M, levels = paste(eg$Var1, eg$Var2))]
  } else {
    if(!is.null(Measure0)) dt5_total <- dt5_total[Measure==Measure0]
    max_y <- max(dt5_total$Value)
  }
  g <- ggplot(data = dt5_total, aes(x = Age, y = Value)) +
    geom_bar(stat="identity", width=0.8, fill = "#1CABE2") + 
    labs( y = "") +

    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 8), 
      axis.text.y = element_text(size = 8), 
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.line.x = element_line(colour = "black"),
      plot.caption = element_text(hjust = 0))  
  
  if(label) g <- g + geom_text(aes(label=label), vjust=-0.3, size=2)
  if("Region"%in%colnames(dt5_total)){
    g <- g + scale_y_continuous(expand = c(0,0), 
                                    labels = scales::comma) +
      facet_wrap(~ R_M, nrow = 4, scales = "free")
      
  } else {
    # g <- g + facet_wrap(~ Measure, nrow = 1, scales = "free") 
    g <- g + scale_y_continuous(expand = c(0,0), 
                                limits = c(0, max_y*1.1),
                                    labels = scales::comma) 
  }
  return(g)
  
  
}

