# data processing 
# the data is cumulative by date, so sometimes subset the latest date
subset.latest.date <- function(dt){
  dt[, max_date:= max(Date), by = Country]
  dt <- dt[Date==max_date]
  dt[, max_date:=NULL]
  return(dt)  
}

#' world daily report
#' 
get.JHU.daily <- function(){
  date1 <- format(Sys.Date()-1, format = "%m-%d-%Y")
  date2 <- format(Sys.Date()-2, format = "%m-%d-%Y")
  url0 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
  url_daily_report <- paste0(url0, as.character(date1) ,".csv")
  if(RCurl::url.exists(url_daily_report)){
    dt_JUH <- fread(url_daily_report)  
  } else {
    dt_JUH <- fread(paste0(url0, as.character(date2) ,".csv"))  
  }
  return(dt_JUH)
}

#' pool f and m into Both sex
pool_sex <- function(dt5, sex_specific = FALSE){
  dt5 <- copy(dt5)[Region == "All" & Age!="TOT" & !is.na(Value)]
  if(sex_specific) return(dt5[Sex!="Both",.(Country, Measure, Age, Sex, Value)])
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
  data_sex2 <- unique(data_sex2[,.(Country, Measure, Age, Value)])
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


revised_country_names <- list(
  "Czechia" = "Czech Republic", 
  "England", 
  "Eswatini" = "Swaziland", 
  "Gambia" = "Gambia The",
  "Northern Ireland",
  "Scotland",
  "South Korea" = "Korea Rep",
  "UK" = "United Kingdom",
  "USA" = "United States of America"
)

join_region <- function(dt1_ava, get_pop = FALSE){
  country_info <- get_country_info()
  vars_wanted <- c("CountryName", "ISO3Code", "WBRegion3", "UNICEFReportRegion1")
  if(get_pop) vars_wanted <- c("CountryName", "ISO3Code",
                               "WBRegion3", "UNICEFReportRegion1",
                               "pop02019", "pop1to42019")
  dt_info <- country_info[,..vars_wanted]
  dt_info[, nc:=.N, by = UNICEFReportRegion1]
  dt_info[, nc_income:=.N, by = WBRegion3]
  setkey(dt_info, CountryName)
  dt1_ava$Country <- get.match(dt1_ava$Country, new_list = revised_country_names)
  name_missed <- unique(dt1_ava$Country[!dt1_ava$Country%in%dt_info$CountryName])
  message("Check regions from dataset not matched to country_info: ", paste(name_missed, collapse = "; "))
  setkey(dt1_ava, Country)
  dt2 <- dt_info[dt1_ava, nomatch = 0]
  dt2[, Country:= CountryName]
  return(dt2)
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
