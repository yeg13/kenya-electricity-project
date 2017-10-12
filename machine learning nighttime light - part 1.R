
### Anecdotal evidence suggests that illumination plays an important role in education, as extended lighting could translate
### into longer study hours and potentially better educational outcomes. This piece explores this assumption in Kenya using
### nighttime light index collected by the National Oceanic and Atmospheric Administration (NOAA) as a proxy of illumination.
### Two systems with different resolutions have been used to collect the nighttime light data in 2008/09 and 2014. Using the
### crossover year 2013 as a reference, I used machine learning techniques to normalize data from two different systems.
### This part is all about data extraction.


## Load packages and set working directory
packages <- c("sp", "maptools", "raster", "rasterVis", "RCurl", "rgeos", "rgdal", "readstata13", "ggplot", "caret", "car")
lapply(packages, require, character.only = TRUE)

setwd("P:Thesis\\Nightlight VIIR\\2013\\rasters")

## Extract coordinates for all household clusters
# read shapefiles
cluster <- read.dta13("C:/Users/guyuye2.UOFI/Downloads/County/Cluster_GPS.dta")
cluster <- cluster[, c("LATNUM", "LONGNUM", "DHSYEAR", "hv001")]
cluster <- subset(cluster, !(is.na(LATNUM)))
cluster <- subset(cluster, LONGNUM != "0")
cluster$ID <- paste0(cluster$DHSYEAR, cluster$hv001)
cluster <- cluster[, c(5, 1:4)]
coords <- cluster[, c("LONGNUM", "LATNUM")]
cluster_sp <- SpatialPoints(coords)

kenya <- readOGR("C:/Users/guyuye2.UOFI/Downloads/County/County.shp")
#hhcluster <- readOGR("C:/Users/guyuye2.UOFI/Downloads/County/hhcluster_sp.shp")
crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs.proj <- CRS("+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 
                +datum=WGS84 +units=m +no_defs")

cluster_sp@proj4string <- crs.geo
cluster_proj <- spTransform(cluster_sp, crs.proj)

## Extract monthly data within a 2,000m buffer collected using VIIR in 2013
## The VIIRS system breaks the world down into 6 tiles, and Kenya belongs to two of them (Tile 5 and Tile 2).
cluster_buffer <- gBuffer(cluster_proj, width = 2000, byid = TRUE)

# Crop the shape of Kenya from two tiles, and then extract nighttime light data using the cropped raster file.
#_Jan_#
T5_01 <- raster("T5_01.avg_rade9.tif")
T5_01 <- crop(T5_01, extent(kenya))
T2_01 <- raster("T2_01.avg_rade9.tif")
T2_01 <- crop(T2_01, extent(kenya))
T01 <- merge(T5_01, T2_01)

T01.proj <- projectRaster(T01, crs = crs.proj)
T01.mask_proj <- mask(T01.proj, cluster_buffer)
ex1 <- extract(T01.mask_proj, cluster_buffer, mean)

#_Feb_#
T5_02 <- raster("T5_02.avg_rade9.tif")
T5_02 <- crop(T5_02, extent(kenya))
#T5_01 <- mask(T5_01, kenya)
T2_02 <- raster("T2_02.avg_rade9.tif")
T2_02 <- crop(T2_02, extent(kenya))
T02 <- merge(T5_02, T2_02)

T02.proj <- projectRaster(T02, crs = crs.proj)
T02.mask_proj <- mask(T02.proj, cluster_buffer)
ex2 <- extract(T02.mask_proj, cluster_buffer, mean)

#_Mar_#
T5_03 <- raster("T5_03.avg_rade9.tif")
T5_03 <- crop(T5_03, extent(kenya))
#T5_01 <- mask(T5_01, kenya)
T2_03 <- raster("T2_03.avg_rade9.tif")
T2_03 <- crop(T2_03, extent(kenya))
T03 <- merge(T5_03, T2_03)

T03.proj <- projectRaster(T03, crs = crs.proj)
T03.mask_proj <- mask(T03.proj, cluster_buffer)
ex3 <- extract(T03.mask_proj, cluster_buffer, mean)

#_Apr_#
T5_04 <- raster("T5_04.avg_rade9.tif")
T5_04 <- crop(T5_04, extent(kenya))
#T5_01 <- mask(T5_01, kenya)
T2_04 <- raster("T2_04.avg_rade9.tif")
T2_04 <- crop(T2_04, extent(kenya))
T04 <- merge(T5_04, T2_04)

T04.proj <- projectRaster(T04, crs = crs.proj)
T04.mask_proj <- mask(T04.proj, cluster_buffer)
ex4 <- extract(T04.mask_proj, cluster_buffer, mean)

#_May_#
T5_05 <- raster("T5_05.avg_rade9.tif")
T5_05 <- crop(T5_05, extent(kenya))
#T5_01 <- mask(T5_01, kenya)
T2_05 <- raster("T2_05.avg_rade9.tif")
T2_05 <- crop(T2_05, extent(kenya))
T05 <- merge(T5_05, T2_05)

T05.proj <- projectRaster(T05, crs = crs.proj)
T05.mask_proj <- mask(T05.proj, cluster_buffer)
ex5 <- extract(T05.mask_proj, cluster_buffer, mean)

#_Jun_#
T5_06 <- raster("T5_06.avg_rade9.tif")
T5_06 <- crop(T5_06, extent(kenya))
#T5_01 <- mask(T5_01, kenya)
T2_06 <- raster("T2_06.avg_rade9.tif")
T2_06 <- crop(T2_06, extent(kenya))
T06 <- merge(T5_06, T2_06)

T06.proj <- projectRaster(T06, crs = crs.proj)
T06.mask_proj <- mask(T06.proj, cluster_buffer)
ex6 <- extract(T06.mask_proj, cluster_buffer, mean)

#_Jul_#
T5_07 <- raster("T5_07.avg_rade9.tif")
T5_07 <- crop(T5_07, extent(kenya))
#T5_01 <- mask(T5_01, kenya)
T2_07 <- raster("T2_07.avg_rade9.tif")
T2_07 <- crop(T2_07, extent(kenya))
T07 <- merge(T5_07, T2_07)

T07.proj <- projectRaster(T07, crs = crs.proj)
T07.mask_proj <- mask(T07.proj, cluster_buffer)
ex7 <- extract(T07.mask_proj, cluster_buffer, mean)

#_Aug_#
T5_08 <- raster("T5_08.avg_rade9.tif")
T5_08 <- crop(T5_08, extent(kenya))
#T5_01 <- mask(T5_01, kenya)
T2_08 <- raster("T2_08.avg_rade9.tif")
T2_08 <- crop(T2_08, extent(kenya))
T08 <- merge(T5_08, T2_08)

T08.proj <- projectRaster(T08, crs = crs.proj)
T08.mask_proj <- mask(T08.proj, cluster_buffer)
ex8 <- extract(T08.mask_proj, cluster_buffer, mean)

#_Sep_#
T5_09 <- raster("T5_09.avg_rade9.tif")
T5_09 <- crop(T5_09, extent(kenya))
#T5_01 <- mask(T5_01, kenya)
T2_09 <- raster("T2_09.avg_rade9.tif")
T2_09 <- crop(T2_09, extent(kenya))
T09 <- merge(T5_09, T2_09)

T09.proj <- projectRaster(T09, crs = crs.proj)
T09.mask_proj <- mask(T09.proj, cluster_buffer)
ex9 <- extract(T09.mask_proj, cluster_buffer, mean)

#_Oct_#
T5_10 <- raster("T5_10.avg_rade9.tif")
T5_10 <- crop(T5_10, extent(kenya))
#T5_01 <- mask(T5_01, kenya)
T2_10 <- raster("T2_10.avg_rade9.tif")
T2_10 <- crop(T2_10, extent(kenya))
T10 <- merge(T5_10, T2_10)

T10.proj <- projectRaster(T10, crs = crs.proj)
T10.mask_proj <- mask(T10.proj, cluster_buffer)
ex10 <- extract(T10.mask_proj, cluster_buffer, mean)

#_Nov_#
T5_11 <- raster("T5_11.avg_rade9.tif")
T5_11 <- crop(T5_11, extent(kenya))
#T5_01 <- mask(T5_01, kenya)
T2_11 <- raster("T2_11.avg_rade9.tif")
T2_11 <- crop(T2_11, extent(kenya))
T11 <- merge(T5_11, T2_11)

T11.proj <- projectRaster(T11, crs = crs.proj)
T11.mask_proj <- mask(T11.proj, cluster_buffer)
ex11 <- extract(T11.mask_proj, cluster_buffer, mean)

#_Dec_#
T5_12 <- raster("T5_12.avg_rade9.tif")
T5_12 <- crop(T5_12, extent(kenya))
#T5_01 <- mask(T5_01, kenya)
T2_12 <- raster("T2_12.avg_rade9.tif")
T2_12 <- crop(T2_12, extent(kenya))
T12 <- merge(T5_12, T2_12)

T12.proj <- projectRaster(T12, crs = crs.proj)
T12.mask_proj <- mask(T12.proj, cluster_buffer)
ex12 <- extract(T12.mask_proj, cluster_buffer, mean)


## Putting extracted nighttime indices collected using VIIR to one data frame

ex_viir <- as.data.frame(cbind(ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10, ex11, ex12))
View(ex_viir)
ex_viir$ex<- apply(ex_viir, 1, mean,na.rm = F)

## Annual data collected using DSMP System
dsmp <- raster("P:\\Thesis\\Nightlight DMSP\\F182013.v4c_web.stable_lights.avg_vis.tif")
dsmp <- crop(dsmp, extent(kenya))
dsmp.proj <- projectRaster(dsmp, crs = crs.proj)
dsmp.mask_proj <- mask(dsmp.proj, cluster_buffer)
ex_dsmp <- extract(dsmp.mask_proj, cluster_buffer, mean)

## Combine data collected using both VIIR and DSMP
ex_2013 <- cbind(ex_viir, ex_dsmp)
save(ex_2013, file = "ex_2013.Rda")

