# make MPIDR country-specific and aggregated plot
# load data ---- 
# 
suppressPackageStartupMessages({
  library("data.table")
  library("ggplot2")
  library("readr")
  library("cowplot")
  library("osfr")
})
source("plotting_funcs.R")
source("00_Functions_convert_to_count.R")

# download input.DB as zip file and read in 
inputDB <- refresh_data() # re-download and calculate fraction into numbers 
# 
inputDB[, table(Metric)]
inputDB[, table(Measure)] # ASCFR  Cases Deaths  Tests
dt1 <- clean_inputDB(inputDB = inputDB)
all_countries <- get_cnames(dt1)

# country-specific plot ---- 
# three plots for each country showing Cases, Deaths, and CFR (Death/Case) 
# whichever is available, showing sex-specific if available
cname0 <- all_countries[1]
p1 <- make_country_plot(cname0)

if(!dir.exists("fig/country/")) dir.create("fig/country", recursive = TRUE)
ggsave(p1, filename = paste0("fig/country/", cname0, ".png"), width = 11, height = 4)

# a wrapped function to save a group of countries by supplying country names, results will be saved as one pdf or png file  
save_country_plot_in_one(cnames = all_countries, png_or_pdf = "pdf")
save_country_plot_in_one(cnames = all_countries, png_or_pdf = "png")

# If needed, can save CFR (Death/Case) alone
# p2 <- make_country_plot(cname0, CFR_alone = TRUE)

# aggregated plots for all countries ---- 
# only choose those that has both case and death data and can calculate CFR
# by assigning a common age interval, only those countries whose age interval are adaptable are used (i.e. through combining intervals into a wider one, 0-5 & 5-10 -> 0-10) 
# Fewer countries will be included if set a stricter interval, or set `get_f_m = TRUE` to ask for sex-specific data
data_total1 <- rbindlist(lapply(all_countries, get_dt_for_total, 
                                data = dt1,
                                target_interval = seq(0, 60, by = 10),
                                get_f_m = TRUE))
# a three-panel plot (Case, Death, CFR) for a specific given interval
g1 <- plot_aggregated_total(data_total1)

# a wrapped function to plot and save:
# return list of figures, and save using ggsave in the same time 
g_list <- plot_aggregated_total_wrap(
  data = dt1, 
  max_interval = 60, 
  by_interval = 10,
  one_row = FALSE, # 2 rows if FALSE, 1st row is total, 2nd row is sex-specific
  folder = "fig/aggregated")

# save plots for all aggregated ones ---- 
# Total and sex-specific in two rows
g_total <- Map(plot_aggregated_total_wrap,
               max_interval = c(60, 60, 80, 80), by_interval = c(10, 20, 10, 20),
               one_row = FALSE)
# Total and sex-specific in one row 
g_total <- Map(plot_aggregated_total_wrap, 
               max_interval = c(60, 60, 80, 80), by_interval = c(10, 20, 10, 20),
               one_row = TRUE)

# all the aggregated plots together
g_grid1 <- cowplot::plot_grid(plotlist = g_total, ncol = 1)
Ref0 <- paste0("\nReference: Riffe, T., Acosta, E, et. al. (2020, July 21). COVerAGE-DB: A database of COVID-19 cases and deaths by age. https://doi.org/10.17605/OSF.IO/MPWJQ. Dataset downloaded on ", dt1$Download_Date[1])
Reference <- cowplot::ggdraw() + 
  draw_label(Ref0, x = 0, hjust = 0) + 
  theme(plot.margin = margin(1, 1, 1, 7))
g_grid_wref <- cowplot::plot_grid(g_grid1, Reference, ncol = 1,
                                  rel_heights = c(1, 0.05))
ggsave(paste0("fig/MPIDR_aggregated",".png"), g_grid_wref, width = 24, height = 4*length(g_total), limitsize = FALSE)
ggsave(paste0("fig/MPIDR_aggregated",".pdf"), g_grid_wref, width = 24, height = 4*length(g_total), limitsize = FALSE)
