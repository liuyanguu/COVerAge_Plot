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
    ggthemes::theme_map() + 
    theme(legend.position = c(0.6, 0.2),
          legend.direction = "horizontal",
          legend.justification = c("bottom"),
          legend.title = element_text(size=18),  # font size of the legend 
          legend.text = element_text(size=16))
  return(gWorld)
}