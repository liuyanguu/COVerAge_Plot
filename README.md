# COVerAge_Plot : Age Pyramid for the COVerAGE-DB Dataset

[COVerAGE-DB dataset project page](https://github.com/timriffe/covid_age)

## Plots using the harmonized dataset (`Output_5.zip`)

The [Github page](https://liuyanguu.github.io/COVerAge_Plot/index.html) (updated in October 2021) shows the plots based on the 5-year age interval harmonized datasets from COVerAGE-DB
The pages are rendered from the `.Rmd` files.


## Plots using the raw input dataset (`inputDB.zip`)
Notice that it's harder to work on the input database directly now since it is quite large (\~1.5GB). The cleaning step might take an hour.

The script `plot_raw_dataset.R` walks through the process 

Download raw data from OSF: <https://osf.io/mpwjq/>

```{r}
source("plotting_funcs.R") # all the plotting functions
source("00_Functions_convert_to_count.R") # borrow to distribute total if Metric is "Fraction"
inputDB <- refresh_data() # re-download and calculate fraction into numbers 
```

This cleaning step might take an hour:
```{r}
dt1 <- clean_inputDB(inputDB = inputDB)
```

### Country-specific plot

-   Age pyramid for each country showing Cases, Deaths, and CFR (Death/Case), whichever is available
-   Showing sex-specific if available
-   Showing in title the latest date used as it is different for each country

```{r}
all_countries <- get_cnames(dt1)
cname0 <- all_countries[1]
p1 <- make_country_plot(cname0)
```

![Afghanistan](fig/country/Afghanistan.png)

### Aggregated plots for all countries

-   Only choose those with both case and death data with matching age intervals, thus can calculate CFR
-   By assigning a common age interval, only those countries whose age interval are adaptable are used (i.e. through combining intervals into a wider one, 0-5 & 5-10 -\> 0-10)
-   Fewer countries will be included if set a stricter/narrower interval, or set `get_f_m = TRUE` to ask for sex-specific data

```{r}
# this example shows the countries available with 0 to 60 by 10 years age intervals
data_total1 <- rbindlist(lapply(all_countries, get_dt_for_total, 
                                data = dt1,
                                target_interval = seq(0, 60, by = 10),
                                get_f_m = TRUE))
# a three-panel plot (Case, Death, CFR) for a specific given interval
g1 <- plot_aggregated_total(data_total1)
```
