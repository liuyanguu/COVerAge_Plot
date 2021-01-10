# functions for map

#' revise names to match NE map
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
    "Central African Republic" = "Central African Rep."
    )
  cnames <- get.match(cnames, new_list = new_name)
  cnames_miss <- sort(unique(cnames[!cnames%in%WorldRobinson$NAME]))
  if(length(cnames_miss)>0)message("Check country names not in the map: ", 
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
  highlight_cnames <- dt_ava2$Country_new
  highlight_cnames <- highlight_cnames[highlight_cnames%in%WorldRobinson$NAME]
  World_Region <- WorldRobinson[WorldRobinson$NAME %in% highlight_cnames,]
  if(length(World_Region)==0) stop("World_Region has length 0 for plotting")
  message("World_Region has " , length(World_Region), " countries")
  
  geo2 <- dplyr::left_join(geo_df,  dt_ava2, by = c("id" = "Country_new"))
  cols_data <- c("Cases_Deaths" = "#0058AB",  "Cases" = "#1CABE2", "Deaths" = "#69DBFF")
  geo2$nMeasure <- factor(geo2$nMeasure, levels = names(cols_data))

  gWorld <- ggplot() +
    geom_polygon(data = WorldRobinson, aes(x = long, y = lat, group = group), fill="lightgray", colour = "white") +
    geom_polygon(data = geo2[!is.na(geo2$nMeasure),], aes(x = long, y = lat, group = group, fill = nMeasure), colour = "white") +
    scale_fill_manual(values = cols_data,
                      labels = c("Cases and Deaths", "Cases alone", "Deaths alone"))  + 
    ggthemes::theme_map() + 
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.justification = c("bottom"),
          legend.title = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size=10))
  return(gWorld)
}