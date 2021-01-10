# make `data/NEWorldRobinson.rds` from Nature Earth map
library(ggplot2)
library(grid)

# get data
dir.create("NaturalEarth")
download.file(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip", 
              "NaturalEarth/ne_110m_admin_0_countries.zip", "auto")
unzip("NaturalEarth/ne_110m_admin_0_countries.zip", exdir = "NaturalEarth")

# read shape file using rgdal library
library(rgdal)
ogrInfo("NaturalEarth", "ne_110m_admin_0_countries")
World <- readOGR("NaturalEarth", "ne_110m_admin_0_countries")
World <- World[!World$NAME == "Antarctica",]

#
WorldRobinson <- sp::spTransform(World, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
saveRDS(WorldRobinson, "data/NEWorldRobinson.rds")

# df
WorldRobinson_df <- ggplot2::fortify(WorldRobinson, region = "NAME")
saveRDS(WorldRobinson_df, "data/WorldRobinson_df.rds")


# for labelling -- country names 
geo1 <- ggplot2::fortify(WorldRobinson, region = "NAME")
geo2 <- by(geo_df, geo_df$id, function(x) {sp::Polygon(x[c('long', 'lat')])@labpt})
World_centroids <- stats::setNames(do.call("rbind.data.frame", geo2), c('long', 'lat'))
World_centroids$name <- names(geo2)
saveRDS(World_centroids, "data/NEWorld_centroids.rds")


# IDs = sapply(World@polygons, function(x) x@ID)

# direct plot
summary(WorldRobinson)  
plot(WorldRobinson, col = "grey")  
