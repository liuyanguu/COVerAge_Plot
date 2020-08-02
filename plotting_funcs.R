# Functions to make descriptive plots for COVerAge dataset
suppressPackageStartupMessages({
  library("data.table")
  library("ggplot2")
  library("readr")
  library("cowplot")
})


# 1. Data Source: MPIDR - Tim Riffe ---- 
# Max Planck Institute for Demographic Research (MPIDR)
# https://github.com/timriffe/covid_age
#' Get Data host on OSF: https://osf.io/mpwjq/
get_MPIDR_inputDB <- function(){
  # the raw data on OSF: https://osf.io/mpwjq/
  dt_age <- data.table::fread("https://osf.io/8uk9n/?action=download") # link updated 7/1/2020
}
get_MPIDR_output_5 <- function(){
  dt_age <- data.table::fread("https://osf.io/5hyvs/?action=download")
}
get_MPIDR_output_10 <- function(){
  dt_age <- data.table::fread("https://osf.io/wu5ve/?action=download")
}

refresh_data <- function(save_locally = TRUE){
  # refresh raw dataset
  message("Hold on, the file is over 130MB. This is going to take a while ... ")
  # download the online data
  dt0 <- get_MPIDR_inputDB()
  dt0$Download_Date <- format(Sys.Date(), "%d-%m-%Y")
  if(save_locally){
    if(!dir.exists("data")) dir.create("data")
    fwrite(dt0, "data/MPIDR.input.csv")
  }
  return(dt0)
}




# 2. Dataset preparation ----------------------------------------------------

#' rough cleaning inputDB
#' 1. subset latest date by Measure, Country, Sex
#' 2. apply timriffe's step 1 of Processing steps: If Metric is "Fraction", distribute totals to produce counts by age and/or sex.
# Ref: https://timriffe.github.io/covid_age/DataSteps.html
#' 3. subset to Region = "All", Metric = "Count"
#' 4. Remove multiple Code if any (e.g. UK)
#' @param inputDB the dataset downloaded using \code{\link{refresh_data()}}
#' 
clean_inputDB <- function(inputDB){
  # latest date by Measure, Country, Sex
  inputDB[, max_date:= max(Date), by = .(Measure, Country, Sex)]
  inputDB <- inputDB[Date==max_date]
  inputDB[, max_date:=NULL]
  # apply timriffe's step 1 of Processing steps: If Metric is "Fraction", distribute totals to produce counts by age and/or sex.
  # Ref: https://timriffe.github.io/covid_age/DataSteps.html
  dt1 <- convert_to_count(inputDB)
  setDT(dt1)
  dt1 <- dt1[Metric == "Count" & Measure%in%c("ASCFR", "Cases", "Deaths") & Region == "All"]
  dt1[, Value:=round(Value)] # since the calculated count could have decimal
  # latest date by Measure, Country, Sex
  dt1[, max_date:= max(Date), by = .(Measure, Country, Sex)]
  dt1 <- dt1[Date==max_date]
  dt1[, max_date:=NULL]
  # UK has multiple Code, remove duplication
  dt1[,Code1 := Code[1], by = .(Measure, Country, Sex)]
  dt1 <- dt1[Code==Code1]
  dt1[, Code1:= NULL]
  dt1$Sex <- as.factor(dt1$Sex)
  levels(dt1$Sex) <- c("Both", "Female", "Male", "Unknown")
  # as a reference: available measure and sex by country
  dt1[, avail_measure:= paste(sort(unique(Measure)), collapse = ","), by = Country]
  dt1[, avail_sex:= paste(sort(unique(Sex)), collapse = ","), by = Country]
  return(dt1)
}

#' get all country names, remove some regions if needed 
get_cnames <- function(dt1){
  cnames <- sort(unique(dt1$Country))
  cnames <- cnames[!cnames%in%c("Taiwan")]
  return(cnames)
}



# 3. Core plotting funcs. ------------------------------------------------------
#' Core plot function to make one plot
#' reorder age intervals in the right way
#' NA age group will be coded into "Unknown"
plot.measure <- function(data, Measure0){
  
  data_sex <- copy(data[Measure==Measure0])
  levels0 <- unique(sort(data_sex$Age))
  n_levels <- length(levels0)
  data_sex$Age <- factor(data_sex$Age, levels = levels0) # levels adjusted in order
  data_sex[is.na(Age), Age:= "Unknown"]
  # adjust group names to e.g. 0-10,..., 60+
  levels1 <- levels(data_sex$Age) # levels to be renamed 
  levels2 <- shift(levels1, -1)
  levels3 <- paste0(levels1, "-", levels2)
  levels3[n_levels] <- paste0(levels1[n_levels], "+")
  levels3[grep("Unknown", levels3)] <- "Unknown"
  levels(data_sex$Age) <- levels3
  # levels1[is.na(levels1)] <- "+"
  # levels2 <- paste0(levels0,"-", levels1)
  
  ngroup <- uniqueN(data_sex$Age)
  max_value <- max(data_sex$Value, na.rm = TRUE) * 1.2
  # put labels on the two sides of the bars:
  data_sex[, label_position := ifelse(Sex == "Male", -Value-max_value*0.09,
                                      Value + max_value*0.09)]  
  
  g <- ggplot(data = data_sex,
              mapping = aes(x = ifelse(Sex == "Male", yes = -Value, no = Value), 
                            y = Age, fill = Sex, label = Value)
  ) +
    geom_col(width = 0.8) +
    # ggrepel::geom_text_repel(seed = 123,
    #                          direction = "x", 
    #                          hjust = "outward",
    #                          bg.r = 0.15, bg.color = "white",
    #                          size = if(ngroup>10) 2 else 2.5,
    #                          force_pull = 2, force = 1,
    #                          segment.size = 0.2, segment.color = "grey"
    # ) +
    geom_text(aes(x = label_position), size = if(ngroup>10) 2 else 2.5, 
              hjust = 0.5
    )+
    labs(x = "", y ="", fill = "", 
         subtitle = paste(Measure0))  + 
    theme_minimal() + 
    theme(axis.text.x = element_text(size=ifelse(max_value>1E4, 6, 8)),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(colour = "black"))
  # g
  # If it is a sex plot --- add color 
  avail_sex <- unique(data_sex$Sex)
  if(any(grepl("Female|Male", avail_sex))){
    g <- g+
      scale_fill_manual(values = c("Female" = "pink2", "Male" = "skyblue"))
  }
  # revise axis
  scientific_10 <- function(x) {
    parse(text=gsub("e", " %*%10^", scales::scientific_format()(x)))
    # scales::scientific_format()(x)
  }
  scientific_10_abs <- function(x) {
    parse(text=gsub("e", " %*%10^", scales::scientific_format()(abs(x))))
    # scales::scientific_format()(abs(x))
  }
  
  # if(length(avail_sex)>1){
  #   g <-  g + scale_x_continuous(labels = abs,
  #                                limits = max_value * c(-1,1))       
  # }
  if(length(avail_sex)>1){
    # have both Male and Female
    if(max_value>1E4){
      g <-  g + scale_x_continuous(labels = scientific_10_abs,
                                   limits = max_value * c(-1,1))
    } else {
      g <-  g + scale_x_continuous(labels = abs, limits = max_value * c(-1,1))
    }
  } else {
    # have only Both
    if(max_value>1E4){
      g <-  g + scale_x_continuous(labels =scientific_10, limits = c(0, max_value))}
  }
  return(g)
} 

# Calculate CFR --- Case Fatality Rate ---- 
get.data.CFR <- function(data_both, Exact_Match = FALSE){
  # check if it is possible to calculate CFR:
  avail_mea <- sort(unique(data_both[Value!=0, Measure]))
  if(!("Cases"%in%avail_mea & "Deaths"%in%avail_mea)) return(NULL)
  # no CFR for the Unknown
  data_both <- copy(data_both)[Age!="Unknown"]
  n_sex <- uniqueN(data_both$Sex)
  if(!n_sex%in%c(1L,2L,3L)) stop("Check sex in CFR")
  # Diff age interval 
  if(n_sex %in% c(1L,3L)){ # Both + Female + Male or Both + Both
    data_both[, Value:= sum(Value, na.rm = TRUE), by = .(Measure, Age)]
    data_both <- unique(data_both[,.(Age, Measure, Value)])
    data_both_w <- dcast(data_both, Age ~ Measure, value.var = "Value")
    data_both_w[, `:=`(Sex = "Both", Measure = "CFR")] # add marker
  }  else {
    # Both have F/M, Keep Sex
    data_both_w <- dcast(data_both, Age + Sex ~ Measure, value.var = "Value")
    data_both_w[, `:=`(Measure = "CFR")] # add marker
  }
  # check if Case > Death
  if(nrow(data_both_w[Deaths > Cases])>0) return(NULL)
  
  # Aggregate by common age group
  data_both_w[!is.na(Cases) & !is.na(Deaths), Common_Age:= Age]
  # request exact match of intervals
  if(Exact_Match){
    if (any(is.na(data_both_w$Common_Age))) return(NULL)
  }
  # rolling forward to fill in all the gaps in age intervals for aggregating
  data_both_w$Common_Age <- zoo::na.locf(data_both_w$Common_Age) 
  data_both_w[, Cases:= sum(Cases, na.rm = TRUE), by = .(Sex, Common_Age)]
  data_both_w[, Deaths:= sum(Deaths, na.rm = TRUE), by = .(Sex, Common_Age)]
  data_both_w <- data_both_w[!is.na(Common_Age)]
  data_both_w[, Value:= round(Deaths/Cases*100, 2)] # Calculate CFR
  data_both_w <- unique(data_both_w[,.(Common_Age, Sex, Measure, Value)])
  setnames(data_both_w, "Common_Age", "Age")
  if(nrow(data_both_w)==0 | uniqueN(data_both_w$Age)<3) return(NULL)
  return(data_both_w)
}


# 4.1 Country plot -----------------------------------
#' main country plot
#' country-specific plot, showing Cases, Deaths, and CFR 
#' whichever is available, showing sex-specific if available
#'
#' @param cname0 country name
#' @param CFR_alone option to make CFR plot alone
#' @param return_CFR_data 
make_country_plot <- function(
  cname0, 
  CFR_alone = FALSE, 
  return_CFR_data = FALSE
  ){
  dt2 <- dt1[Country==cname0 & Age!="TOT"]
  latest.date <- max(dt2$Date, na.rm = TRUE)
  avail_mea <- sort(unique(dt2[Value!=0, Measure]))
  if(length(avail_mea)==0) {
    cat(cname0, "-Measures-", "All values are 0, not plotted", "\n")
    return(NULL)
  }
  cat(cname0, "-Measures-", paste(avail_mea, collapse = ", "), "\n")
  # get datasets by measure
  get.dt.measure <- function(Measure0){
    data_measure <- dt2[Measure==Measure0]
    avail_sex <- unique(data_measure$Sex)  
    cat(cname0, Measure0, "-Sexes-", paste(avail_sex, collapse = ", "), "\n")
    if(any(grepl("Female|Male", avail_sex))) {
      data_sex <- data_measure[Sex%in%c("Female", "Male")]
    } else {
      data_sex <- data_measure[Sex%in%c("Both")]
    }
    
    suppressWarnings(data_sex[, Age := as.numeric(Age)])
    data_sex[Age>95, Age:=95]
    
    if(median(data_sex$AgeInt, na.rm = TRUE)<2){
      # sum to 5 year interval if AgeInt is all 1
      data_sex[, Age := Age - Age%%5, by = Sex]
    }
    data_sex[, Value:= sum(Value), by = .(Sex, Age)]
    data_sex <- unique(data_sex[,.(Age, Sex, Value, Measure)])
    data_sex
  }
  data_total <- rbindlist(lapply(avail_mea, get.dt.measure))
  data_both_w <- get.data.CFR(data_total, Exact_Match = FALSE)
  
  if(return_CFR_data) {
    if(!is.null(data_both_w)){
      data_both_w[, Country:=cname0]
      data_both_w[, Age:= as.numeric(as.character(Age))]
      data_both_w[, Int:= Age - shift(Age), by = Sex]
    }
    return(data_both_w)
  }
  
  title <- cowplot::ggdraw() + 
    draw_label(paste0(cname0," (", latest.date, ")"),
               fontface = 'bold', x = 0, hjust = 0) + 
    theme(plot.margin = margin(0, 0, 0, 7))
  
  if(CFR_alone){
    if(!is.null(data_both_w)){
      p <- plot.measure(data_both_w, Measure0 = "CFR") +
        labs(subtitle = paste0(cname0," (", latest.date, ")"))
      return(p)
    } else {
      return(NA)
    }
  }
  
  if(!is.null(data_both_w)) data_total <- rbindlist(list(data_total, data_both_w), fill = TRUE, use.names = TRUE)
  plots_by_measure <- lapply(unique(data_total$Measure), plot.measure, data = data_total)
  nplot <- length(plots_by_measure)
  
  # an extra plots as placeholder
  Grob0 <- grid::rectGrob(gp=grid::gpar(col="white"))
  
  if(nplot==1){
    gg <- cowplot::plot_grid(plots_by_measure[[1]], Grob0, Grob0 ,nrow = 1)
  } else if (nplot==2){
    gg <- cowplot::plot_grid(plots_by_measure[[1]],plots_by_measure[[2]], Grob0, nrow = 1)
  } else {
    gg <- cowplot::plot_grid(plotlist = plots_by_measure, nrow = 1)    
  }
  gg <- cowplot::plot_grid(title, gg, ncol = 1,
                           rel_heights = c(0.1, 1))
  return(gg)  
}


#' save several countries in one plot using `ggsave
#' a simple function for user's convenience
save_country_plot_in_one <- function(
  cnames, # a vector of countries
  n_col0 = 2, # two countries per row
  file_name = "fig/MPIDR_Countries_wCFR",
  png_or_pdf = "pdf"
){
  require("cowplot")
  g_country_list <- lapply(cnames, make_country_plot)
  n_col0 = n_col0
  g_grid <- cowplot::plot_grid(plotlist = g_country_list, ncol = n_col0)
  # height = 4 for each row 
  ggsave(filename = paste0(file_name, ".", png_or_pdf), 
         g_grid, 
         width = 22, 
         height = 4/n_col0*length(g_country_list), 
         limitsize = FALSE)
}


# 4.2 Aggregated plot -----------------------------------------------------


#' get data by country for the total plot
#'
#' @param cname0 one country name
#' @param target_interval target interval to plot
#' @param get_f_m  # If TRUE, only use those with sex-specific data and make sex-specific plot; If FALSE, make plot for Both sex
get_dt_for_total <- function(
  data,
  cname0, 
  target_interval = seq(0, 60, by = 10),
  get_f_m # plot sex-specific?
){
  data_measure <- data[Age!="TOT" & Country==cname0]
  avail_mea <- sort(unique(data_measure[Value!=0, Measure]))
  # must have both Cases and Deaths to get CFR
  if(!("Cases"%in%avail_mea & "Deaths"%in%avail_mea)) return(NULL)
  
  get.sex <- function(Measure0){
    avail_sex <- unique(data_measure[Measure == Measure0, Sex])
    # message(cname0, "-", Measure0, "-Sex: ", paste(avail_sex, collapse = ", "))
    if(any(grepl("Both", avail_sex))) {
      data_sex <- data_measure[Measure == Measure0 & Sex%in%c("Both")]
    } else {
      data_sex <- data_measure[Measure == Measure0 & Sex%in%c("Female", "Male")]
    }
    data_sex
  }
  # if to get sex-specific, Must both have Female and Male 
  if(get_f_m){
    # force both F&M exist in the dataset for both measures
    data_sex <- data_measure[Sex%in%c("Female", "Male")]
    avail_mea <- unique(data_sex[Value!=0, Measure])
    if(!all(c("Cases", "Deaths")%in%avail_mea)) return(NULL)
  } else {
    # Use Both if Both is available and if there is F&M, later F&M will be combined into Both
    data_sex <- rbindlist(lapply(c("Cases", "Deaths"), get.sex))
  }
  
  has_na <- "UNK"%in% data_sex$Age
  suppressWarnings(data_sex[, Age := as.numeric(Age)])
  data_sex[Age>=max(target_interval), Age:=max(target_interval)]
  Age1 <- unique(sort(data_sex[Measure == "Cases", Age])) # These intervals must be there
  Age2 <- unique(sort(data_sex[Measure == "Deaths", Age]))
  if(!(all(target_interval%in%Age1) & all(target_interval%in%Age2))) return(NULL)
  
  Int_Diff <- diff(target_interval)[1]
  # sum to 5 year interval if AgeInt is all 1
  data_sex[, Age := Age - Age%%Int_Diff]
  data_sex[, id:= paste(Measure, Sex, Age)]
  # if ask for both, pool sex
  if(get_f_m){
    # get_f and m
    data_sex2 <- data_sex[, Value:= sum(Value), by = .(Measure, Sex, Age)]
    data_sex2 <- unique(data_sex2[,.(Measure, Sex, Age, Value)])
  } else {
    data_sex2 <- data_sex[, Value:= sum(Value), by = .(Measure, Age)]
    data_sex2 <- unique(data_sex2[,.(Measure, Age, Value)])
    data_sex2[, Sex:= "Both"]
  }
  # data_sex2 <- data_sex2[!is.na(Age)]
  data_sex2[, Country:= cname0]
  # 
  return(data_sex2)
}

# revise the total dataset 
revise.data.total <- function(data_total){
  n_country <- uniqueN(data_total$Country)
  # data_total <- copy(data[Age!="Unknown"])
  data_total[, Value:= sum(Value), by = .(Measure, Sex, Age)]
  data_total <- unique(data_total[,.(Measure, Sex, Age, Value)])
  data_CFR <- get.data.CFR(data_total)
  data_total <- rbindlist(list(data_total, data_CFR), use.names = TRUE)
  data_total[, n_country:= n_country]
  return(data_total)
}

plot_aggregated_total <- function(data_total){
  # further aggregated by country 
  data_total <- revise.data.total(data_total)
  total_title <- paste0("Aggregated Results of ", data_total$n_country[1], " Countries")
  title <- cowplot::ggdraw() + 
    draw_label(total_title, fontface = 'bold', x = 0, hjust = 0) + 
    theme(plot.margin = margin(0, 0, 0, 7))
  plots_by_measure <- lapply(unique(data_total$Measure), plot.measure, data = data_total)
  gg <- cowplot::plot_grid(plotlist = plots_by_measure, nrow = 1)    
  gg <- cowplot::plot_grid(title, gg, ncol = 1,
                           rel_heights = c(0.1, 1))
  return(gg)
}


# aggregated plots wrapped into one function
# 
plot_aggregated_total_wrap <- function(
  data = dt1, 
  max_interval = 60, 
  by_interval = 10,
  one_row = TRUE,
  folder = "fig/aggregated" # where to save
){
  # target_interval = seq(0, 60, by = 10)
  cnames <- unique(data$Country)
  data_total1 <- rbindlist(lapply(cnames, get_dt_for_total, data = data, 
                                  target_interval = seq(0, max_interval, by = by_interval),
                                  get_f_m = FALSE))
  data_total2 <- rbindlist(lapply(cnames, get_dt_for_total, data = data, 
                                  target_interval = seq(0, max_interval, by = by_interval),
                                  get_f_m = TRUE))
  g_list <- lapply(list(data_total1, data_total2), plot_aggregated_total)
  n_col0 <- ifelse(one_row, 2, 1)
  g_grid <- cowplot::plot_grid(plotlist = g_list, ncol = n_col0)
  # height = 4 for each row 
  if(!dir.exists(folder)) dir.create(folder, recursive = TRUE)
  ggsave(filename = file.path(folder, 
                              paste0("Aggregated_plot_0to", max_interval, "_by", by_interval,"_", n_col0, "rows", ".png")),
        g_grid, 
         # save each 3-panel plot as 11*4
         width = 11*n_col0, 
         height = 4/n_col0*length(g_list), 
         limitsize = FALSE)
  return(g_grid)
}

