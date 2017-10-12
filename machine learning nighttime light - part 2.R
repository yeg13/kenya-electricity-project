
## This part extracts nighttime light data collected in 2008 and 2014. Data is extracted within a 2000-meter buffer
## for rural areas, and a 5000-meter buffer for urban areas to potentially offset the controlled deviation by USAID.


## set working directory and read shapefiles
setwd("P:Thesis\\Nightlight VIIR\\2014")
urban_cluster <- readOGR("P:Thesis\\County\\cluster_urban_14.shp")
rural_cluster <- readOGR("P:Thesis\\County\\cluster_rural_14.shp")
cluster_all <- rbind(urban_cluster, rural_cluster)

## Extract nighttime light indices in VIIRS (2014 data)
# Crop the shape of Kenya from two tiles, and then extract nighttime light data using the cropped raster file.
# Data for urban and rural clusters are extracted separately.
#_Jan_#
T5_01 <- raster("T5_01.avg_rade9.tif")
T5_01 <- crop(T5_01, extent(kenya))
#T5_01 <- mask(T5_01, kenya)
T2_01 <- raster("T2_01.avg_rade9.tif")
T2_01 <- crop(T2_01, extent(kenya))
T01 <- merge(T5_01, T2_01)

T01.proj <- projectRaster(T01, crs = crs.proj)
T01.mask_proj <- mask(T01.proj, cluster_all)
ex1_r <- extract(T01.mask_proj, rural_cluster, mean)
ex1_u <- extract(T01.mask_proj, urban_cluster, mean)

#_Feb_#
T5_02 <- raster("T5_02.avg_rade9.tif")
T5_02 <- crop(T5_02, extent(kenya))
#T5_01 <- mask(T5_02, kenya)
T2_02 <- raster("T2_02.avg_rade9.tif")
T2_02 <- crop(T2_02, extent(kenya))
T02 <- merge(T5_02, T2_02)

T02.proj <- projectRaster(T02, crs = crs.proj)
T02.mask_proj <- mask(T02.proj, cluster_all)
ex2_r <- extract(T02.mask_proj, rural_cluster, mean)
ex2_u <- extract(T02.mask_proj, urban_cluster, mean)

#_ missing data for March and April_#

#_May_#
T5_05 <- raster("T5_05.avg_rade9.tif")
T5_05 <- crop(T5_05, extent(kenya))
#T5_01 <- mask(T5_03, kenya)
T2_05 <- raster("T2_05.avg_rade9.tif")
T2_05 <- crop(T2_05, extent(kenya))
T05 <- merge(T5_05, T2_05)
plot(T05)

T05.proj <- projectRaster(T05, crs = crs.proj)
T05.mask_proj <- mask(T05.proj, cluster_all)
ex5_r <- extract(T05.mask_proj, rural_cluster, mean)
ex5_u <- extract(T05.mask_proj, urban_cluster, mean)

#_Jun_#
T5_06 <- raster("T5_06.avg_rade9.tif")
T5_06 <- crop(T5_06, extent(kenya))
#T5_01 <- mask(T5_06, kenya)
T2_06 <- raster("T2_06.avg_rade9.tif")
T2_06 <- crop(T2_06, extent(kenya))
T06 <- merge(T5_06, T2_06)
plot(T06)

T06.proj <- projectRaster(T06, crs = crs.proj)
T06.mask_proj <- mask(T06.proj, cluster_all)
ex6_r <- extract(T06.mask_proj, rural_cluster, mean)
ex6_u <- extract(T06.mask_proj, urban_cluster, mean)

#_Jul_#
T5_07 <- raster("T5_07.avg_rade9.tif")
T5_07 <- crop(T5_07, extent(kenya))
#T5_01 <- mask(T5_07, kenya)
T2_07 <- raster("T2_07.avg_rade9.tif")
T2_07 <- crop(T2_07, extent(kenya))
T07 <- merge(T5_07, T2_07)
plot(T07)

T07.proj <- projectRaster(T07, crs = crs.proj)
T07.mask_proj <- mask(T07.proj, cluster_all)
ex7_r <- extract(T07.mask_proj, rural_cluster, mean)
ex7_u <- extract(T07.mask_proj, urban_cluster, mean)

#_Aug_#
T5_08 <- raster("T5_08.avg_rade9.tif")
T5_08 <- crop(T5_08, extent(kenya))
#T5_01 <- mask(T5_07, kenya)
T2_08 <- raster("T2_08.avg_rade9.tif")
T2_08 <- crop(T2_08, extent(kenya))
T08 <- merge(T5_08, T2_08)
plot(T08)

T08.proj <- projectRaster(T08, crs = crs.proj)
T08.mask_proj <- mask(T08.proj, cluster_all)
ex8_r <- extract(T08.mask_proj, rural_cluster, mean)
ex8_u <- extract(T08.mask_proj, urban_cluster, mean)

#_Sep_#
T5_09 <- raster("T5_09.avg_rade9.tif")
T5_09 <- crop(T5_09, extent(kenya))
#T5_01 <- mask(T5_07, kenya)
T2_09 <- raster("T2_09.avg_rade9.tif")
T2_09 <- crop(T2_09, extent(kenya))
T09 <- merge(T5_09, T2_09)
plot(T09)

T09.proj <- projectRaster(T09, crs = crs.proj)
T09.mask_proj <- mask(T09.proj, cluster_all)
ex9_r <- extract(T09.mask_proj, rural_cluster, mean)
ex9_u <- extract(T09.mask_proj, urban_cluster, mean)

#_Oct_#
T5_10 <- raster("T5_10.avg_rade9.tif")
T5_10 <- crop(T5_10, extent(kenya))
#T5_01 <- mask(T5_10, kenya)
T2_10 <- raster("T2_10.avg_rade9.tif")
T2_10 <- crop(T2_10, extent(kenya))
T10 <- merge(T5_10, T2_10)
plot(T10)

T10.proj <- projectRaster(T10, crs = crs.proj)
T10.mask_proj <- mask(T10.proj, cluster_all)
ex10_r <- extract(T10.mask_proj, rural_cluster, mean)
ex10_u <- extract(T10.mask_proj, urban_cluster, mean)

#_Nov_#
T5_11 <- raster("T5_11.avg_rade9.tif")
T5_11 <- crop(T5_11, extent(kenya))
#T5_01 <- mask(T5_10, kenya)
T2_11 <- raster("T2_11.avg_rade9.tif")
T2_11 <- crop(T2_11, extent(kenya))
T11 <- merge(T5_11, T2_11)
plot(T11)

T11.proj <- projectRaster(T11, crs = crs.proj)
T11.mask_proj <- mask(T11.proj, cluster_all)
ex11_r <- extract(T11.mask_proj, rural_cluster, mean)
ex11_u <- extract(T11.mask_proj, urban_cluster, mean)

#_Dec_#
T5_12 <- raster("T5_12.avg_rade9.tif")
T5_12 <- crop(T5_12, extent(kenya))
#T5_01 <- mask(T5_10, kenya)
T2_12 <- raster("T2_12.avg_rade9.tif")
T2_12 <- crop(T2_12, extent(kenya))
T12 <- merge(T5_12, T2_12)
plot(T12)

T12.proj <- projectRaster(T12, crs = crs.proj)
T12.mask_proj <- mask(T12.proj, cluster_all)
ex12_r <- extract(T12.mask_proj, rural_cluster, mean)
ex12_u <- extract(T12.mask_proj, urban_cluster, mean)

# Combine data from rural clusters and urban clusters 
ex_r <- as.data.frame(cbind(ex1_r, ex2_r, ex5_r, ex6_r, ex7_r, ex8_r, ex9_r, ex10_r, ex11_r, ex12_r, rural_cluster$Cluster))
View(ex_r)
ex_r$ex<- apply(ex_r[1:10], 1, mean,na.rm = F)

ex_u <- as.data.frame(cbind(ex1_u, ex2_u, ex5_u, ex6_u, ex7_u, ex8_u, ex9_u, ex10_u, ex11_u, ex12_u, urban_cluster$Cluster))
View(ex_u)
ex_u$ex<- apply(ex_u[1:10], 1, mean,na.rm = F)

ex_2014 <- rbind(ex_r, ex_u)
save(ex_2014, file = "ex_2014.Rda")

## Extract nighttime light indices in DSMP (2008 data)

urban_cluster_08 <- readOGR("P:Thesis\\cluster_rural_08\\cluster_urban_08.shp")
rural_cluster_08 <- readOGR("P:Thesis\\cluster_rural_08\\cluster_rural_08.shp")
cluster_all_08 <- rbind(urban_cluster_08, rural_cluster_08)
cluster_08.proj <- spTransform(cluster_all_08, crs.proj)

# Annual nighttime light data is available for 2008, so only need to extract one raster file here
raster09 <- raster("P:\\Thesis\\F162008.v4\\F162008.v4b_web.stable_lights.avg_vis.tif\\F162008.v4b_web.stable_lights.avg_vis.tif")
raster09 <- crop(raster09, extent(kenya))
raster09.proj <- projectRaster(raster09, crs = crs.proj)
#raster09.mask_proj <- mask(raster09.proj, cluster_all_08, mean)
raster_08_r <- extract(raster09.proj, rural_cluster_08, mean)
raster_08_r <- cbind(raster_08_r, rural_cluster_08$Cluster)
raster_08_u <- extract(raster09.proj, urban_cluster_08, mean)
raster_08_u <- cbind(raster_08_u, urban_cluster_08$Cluster)
results08 <- rbind(raster_08_r, raster_08_u)