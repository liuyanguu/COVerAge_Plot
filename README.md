# COVerAge_Plot : Age Pyramid for the COVerAGE-DB Dataset

COVerAGE-DB dataset: [https://github.com/timriffe/covid_age](https://github.com/timriffe/covid_age)

The `main_script.R` walks through the process to make all the plots below

## Download raw data from OSF: https://osf.io/mpwjq/
```{r}
source("plotting_funcs.R") # all the plotting functions
source("00_Functions_convert_to_count.R") # borrow to distribute total if Metric is "Fraction"
inputDB <- refresh_data(save_locally = TRUE) # download and save the data locally
```

## Country-specific plot 
* Three-plots age pyramid for each country showing Cases, Deaths, and CFR (Death/Case), whichever is available
* Showing sex-specific if available
```{r}
dt1 <- clean_inputDB(inputDB = inputDB)
all_countries <- get_cnames(dt1)
cname0 <- all_countries[1]
p1 <- make_country_plot(cname0)
```
![Afghanistan](https://user-images.githubusercontent.com/11966330/89129390-1d1b4f00-d4cb-11ea-8ee5-fcaded6596ef.png)


## Aggregated plots for all countries 
* Only choose those that has both case and death data and can calculate CFR
* By assigning a common age interval, only those countries whose age interval are adaptable are used (i.e. through combining intervals into a wider one, 0-5 & 5-10 -> 0-10) 
* Fewer countries will be included if set a stricter interval, or set `get_f_m = TRUE` to ask for sex-specific data
```{r}
# this example shows the countries available in 0 to 60 by 10 years age pyramid
data_total1 <- rbindlist(lapply(all_countries, get_dt_for_total, 
                                data = dt1,
                                target_interval = seq(0, 60, by = 10),
                                get_f_m = TRUE))
# a three-panel plot (Case, Death, CFR) for a specific given interval
g1 <- plot_aggregated_total(data_total1)
```
* A wrapped function to plot and save the aggregated plot:  
Return list of figures, and save using ggsave in the same time   
```{r}
g_list <- plot_aggregated_total_wrap(
  data = dt1, 
  max_interval = 60, 
  by_interval = 10,
  one_row = FALSE, # 2 rows if FALSE, 1st row is total, 2nd row is sex-specific
  folder = "fig/aggregated")

# Make and save different combinations of desired intervals

g_total <- Map(plot_aggregated_total_wrap, 
               max_interval = c(60, 60, 80, 80), by_interval = c(10, 20, 10, 20),
               one_row = FALSE)
```
![Aggregated_plot_0to60_by10_1rows](https://user-images.githubusercontent.com/11966330/89129397-345a3c80-d4cb-11ea-81bf-59d6a16f8b0e.png)

## All together (aggregated + country-specific)
Code in `main_script.R`

### Aggregated results using different combinations  
![MPIDR_aggregated](https://user-images.githubusercontent.com/11966330/89129409-42a85880-d4cb-11ea-8ffe-133faf1c88d7.png)

### Every country in the dataset  
![MPIDR_Countries_wCFR](https://user-images.githubusercontent.com/11966330/89129411-450ab280-d4cb-11ea-850a-ccfd50f1bdcb.png)
