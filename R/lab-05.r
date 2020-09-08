library(raster) # Raster Data handling
library(tidyverse) # Data Manipulation
library(getlandsat) # keyless Landsat data (2013-2017)
library(sf) # Vector data processing
library(mapview) # Rapid Interactive visualization



# Question 2
bbwgs = aoi %>%
  st_transform(4326)

bb = st_bbox(bbwgs)


scenes = getlandsat::lsat_scenes()


aoi_sc = scenes %>%
  filter(min_lat <= bb$ymin, max_lat >= bb$ymax,
         min_lon <= bb$xmin, max_lon >= bb$xmax,
         as.Date(acquisitionDate) == as.Date("2016-09-26"))

write.csv(aoi_sc, file = "data/palo-flood.csv", row.names = FALSE)




