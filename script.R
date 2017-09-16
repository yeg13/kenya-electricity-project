
### This script reads and projects shapefiles, calculates distance to grid, collapses DHS data, and linkes shapefiles to DHS data

## Load required packages
## Set working directory
packages <- c("readstata13","geosphere", "sp", "rgdal", "maptools","mapproj", "gdistance","rgeos","dplyr", "lattice",
              "car", "foreign", "gplots","ggplot2", "spdep", "plm", "sphet", "zoo", "pglm", "plyr", "doBy", "PBSmapping", "data.table")
lapply(packages, require, character.only = TRUE)
setwd("C:\\Users\\YeGuyu\\Desktop\\Thesis")

##Read in stata file to get the link between Cluster No. and settlement type
##Data for 2008/09
KEHR.08 <- read.dta13("Thesis\\KEHR_2008_Small.dta")
#View(KEHR.08) #range(KEHR.08$Year)
KEHR.08$Year[KEHR.08$Year == "2008"] <- "2009"


Cluster_Settlement_09 <- subset(KEHR.08, select = c(Cluster, Settlement))
Cluster_Settlement_09 <- subset(Cluster_Settlement_09, !duplicated(Cluster_Settlement_09$Cluster))
Cluster_Settlement_09$urban <- ifelse(Cluster_Settlement_09$Settlement=="urban", 1,0)

KEHR.08$County <- NULL
kenya_2009 <- read.dta13("Kenya Household 2009.dta")
cluster_county_09 <- kenya_2009[, c("hv001", "COUNTY")]
cluster_county_09 <- cluster_county_09[!duplicated(cluster_county_09),]
colnames(cluster_county_09) <- c("Cluster", "County")
KEHR.08 <- merge(KEHR.08, cluster_county_09, by = "Cluster", all = TRUE)


##Data for 2014
KEHR.14 <- read.dta13("Thesis\\KEHR_2014_Small.dta")
#View(KEHR.14)
Cluster_Settlement_14 <- subset(KEHR.14, select = c(Cluster, Settlement))
Cluster_Settlement_14 <- subset(Cluster_Settlement_14,!duplicated(Cluster_Settlement_14$Cluster))
Cluster_Settlement_14$urban <- ifelse(Cluster_Settlement_14$Settlement=="urban", 1,0)
KEHR.14$County <- gsub(".   ", "", KEHR.14$County)
KEHR.14$County <- str_to_title(KEHR.14$County)

## read in shapefile for 2009 powerline and transform to projected system
## add coordinate system to powerlines
#powerline09 <- readOGR("Thesis\\power09_above33.shp", layer = "power09_above33")
powerline09 <- readOGR("Thesis\\poweline2006.shp", layer = "poweline2006")
powerline14 <- readOGR("Thesis\\powerline_2012_Project.shp", layer = "powerline_2012_Project")
#Source for 2006 map: http://unesdoc.unesco.org/images/0014/001488/148866E.pdf
#powerline14 <- readShapeLines("Thesis\\powerline_2012_Project.shp")
#powerline14 <- readOGR("Thesis\\powerline2014.shp", layer = "powerline2014")

crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs.proj <- CRS("+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 
                +datum=WGS84 +units=m +no_defs")

## Transform polylines to projected system
powerline09_proj <- spTransform(powerline09, crs.proj)
powerline14_proj <- spTransform(powerline14, crs.proj)
plot(powerline09)
plot(powerline14)

kenya <- readShapePoly("C:/Users/YeGuyu/Desktop/Thesis/Kenya shapefiles/County.shp")
kenya@proj4string <- crs.geo
kenya_proj <- spTransform(kenya, crs.proj)

### =======================================================================================================================
### Distance without buffer without deviation ###
## Vincenty Ellipsoid
## calculates distance between unprojected points and lines
hhcluster08 <- read.csv("Thesis\\GEO_2008.csv",header = TRUE)
hhcluster08 <- subset(hhcluster08, hhcluster08$Latnum != 0)
hhcluster08$Year[hhcluster08$Year == "2008"] ="2009"
hhcluster08$Year <- as.numeric(hhcluster08$Year)
#hhcluster08 <- cbind(hhcluster08, Cluster_Settlement_09)

hhcluster08_sp <- SpatialPoints(hhcluster08[, c("Longnum", "Latnum", "Cluster", "Year")], crs.geo)
hhcluster08_proj <- spTransform(hhcluster08_sp, crs.proj)


hhcluster14 <- read.csv("Thesis\\GEO_2014.csv",header = TRUE)
hhcluster14 <- subset(hhcluster14, hhcluster14$Latnum != 0)
#hhcluster14 <- cbind(hhcluster14, Cluster_Settlement_14)

hhcluster14_sp <- SpatialPoints(hhcluster14[, c("Longnum", "Latnum", "Cluster", "Year")], crs.geo)
hhcluster14_proj <- spTransform(hhcluster14_sp, crs.proj)


## Extract shortest distance for each household cluster
dist08_1 <- numeric(nrow(hhcluster08_proj@coords))
for (i in (1:nrow(hhcluster08_proj@coords))) {
  dist08_1[i] <- gDistance(hhcluster08_proj[i,], powerline09_proj)
  }

dist14_1 <- numeric(nrow(hhcluster14_proj@coords))
for (i in (1:nrow(hhcluster14_proj@coords))) {
  dist14_1[i] <- gDistance(hhcluster14_proj[i,], powerline14_proj)
  }

#range(dist08_1)
#range(dist14_1) * Overall, distance to grid is smaller in 2014

## Merge with other household-level data
hhcluster08$Dist<- (as.data.frame(dist08_1))$dist08_1
KEHR.08 <- merge(KEHR.08, hhcluster08, by = "Cluster")
KEHR.08$urban <- ifelse(KEHR.08$Settlement=="urban", 1,0)
KEHR.08 <-  KEHR.08 %>% mutate(Electricity = replace(Electricity, Electricity >1 , NA))

hhcluster14$Dist<- (as.data.frame(dist14_1))$dist14_1
KEHR.14 <- merge(KEHR.14, hhcluster14, by = "Cluster")
KEHR.14$urban <- ifelse(KEHR.14$Settlement=="urban", 1,0)
KEHR.14$Electricity <- ifelse(KEHR.14$Electricity=="yes", 1,0)
View(KEHR.14)

## Merge data for two years
KEHR.08$Microwave <- NA
KEHR.08$DVD_Player <- NA
KEHR.08$CD_Player <- NA
KEHR.08$Unit_Ag_Land <- NA


kenya.panel <- rbind(KEHR.08, KEHR.14)
save(kenya.panel, file = "kenya.panel.Rda")
#View(kenya.panel)

## Stats
ggplot(kenya.panel, aes(Number_of_Household_Members, fill=Year.x, colour=Year.x)) +
  geom_histogram(aes(y=..density..), breaks=seq(0,25,1), alpha=0.6, 
                 position="identity", lwd=0.2) +
  ggtitle("Normalized Number of Household Members")

#range(kenya.panel$Wealth_Score) #-250248  376404
ggplot(kenya.panel, aes(Wealth_Score, fill=Year.x, colour=Year.x)) +
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5)+
  ggtitle("Normalized Wealth_Score")

attach(kenya.panel)

scatterplot(Wealth_Index, Electricity)



## Collapsed dataset

collapsed.panel_1 <- summaryBy(Electricity+Wealth_Score+Number_of_Household_Members+Dist+urban + Longnum + Latnum
                               + Altitude + Education_1+Education_2+Education_3+Education_4+Education_5+Education_6
                               + Education_7 + Education_8 + Education_9 + Education_10 + Education_11 + Education_12
                               + Education_13 + Education_14 + Education_15 + Education_16 + Education_17 + Education_18
                               + Education_19 ~ Cluster+Year.x+Region + County, FUN = mean, data = kenya.panel)

setnames(collapsed.panel_1, old = c('Electricity.mean', 'Wealth_Score.mean', 'Year.x', 'Number_of_Household_Members.mean', 'Dist.mean', 'urban.mean', 'Longnum.mean', 'Latnum.mean', 'Altitude.mean',
                                    'Education_1.mean', 'Education_2.mean', 'Education_3.mean', 'Education_4.mean', 'Education_5.mean', 'Education_6.mean', 'Education_7.mean',
                                    'Education_8.mean', 'Education_9.mean', 'Education_10.mean', 'Education_11.mean', 'Education_12.mean', 'Education_13.mean', 'Education_14.mean',
                                    'Education_15.mean', 'Education_16.mean', 'Education_17.mean', 'Education_18.mean', 'Education_19.mean'),
         new = c('Electricity', 'Wealth_Score', 'Year', 'Number_of_Household_Members', 'Dist', 'Urban', 'Longnum', 'Latnum', 'Altitude', 'Education_1', 'Education_2', 'Education_3',
                 'Education_4', 'Education_5', 'Education_6', 'Education_7', 'Education_8', 'Education_9', 'Education_10', 'Education_11', 'Education_12',
                 'Education_13', 'Education_14', 'Education_15', 'Education_16', 'Education_17', 'Education_18', 'Education_19'))
#View(collapsed.panel_1)


attach(collapsed.panel_1)
scatterplot(Dist, Electricity)
scatterplot.matrix(~Electricity+Wealth_Score+Number_of_Household_Members+Dist+Urban, data = collapsed.panel_1)

## Collapsed 08 dataset

collapsed.panel08_1 <- subset(collapsed.panel_1, Year == '2009')
#View(collapsed.panel08_1)
scatterplot(collapsed.panel08_1$Dist, collapsed.panel08_1$Electricity)


## Collapsed 14 dataset

collapsed.panel14_1 <- subset(collapsed.panel_1, Year == '2014')
#View(collapsed.panel14_1)
scatterplot(collapsed.panel14_1$Dist, collapsed.panel14_1$Electricity)


save(collapsed.panel_1, file="collapsed.panel_1.Rda")
save(collapsed.panel08_1, file="collapsed.panel08_1.Rda")
save(collapsed.panel14_1, file="collapsed.panel14_1.Rda")



### ==========================================================================================================================
### Distance without buffer with Deviations
### Rural: 5-10km (use 5km); Urban: 2km

## Separate dataset into rural and urban

hhcluster08 <- merge(hhcluster08, Cluster_Settlement_09, by = "Cluster")
hhcluster14 <- merge(hhcluster14, Cluster_Settlement_14, by = "Cluster")

hhcluster08_rural <- subset(hhcluster08[ which(hhcluster08$urban == "0"),], select = c(Longnum, Latnum, Cluster))
hhcluster08_urban <- subset(hhcluster08[ which(hhcluster08$urban == "1"),], select = c(Longnum, Latnum, Cluster))
hhcluster14_rural <- subset(hhcluster14[ which(hhcluster14$urban == "0"),], select = c(Longnum, Latnum, Cluster))
hhcluster14_urban <- subset(hhcluster14[ which(hhcluster14$urban == "1"),], select = c(Longnum, Latnum, Cluster))


hhcluster08r_proj <- SpatialPointsDataFrame(spTransform(SpatialPoints(hhcluster08_rural, crs.geo), crs.proj), hhcluster08_rural)
hhcluster08u_proj <- SpatialPointsDataFrame(spTransform(SpatialPoints(hhcluster08_urban, crs.geo), crs.proj), hhcluster08_urban)
hhcluster14r_proj <- SpatialPointsDataFrame(spTransform(SpatialPoints(hhcluster14_rural, crs.geo), crs.proj), hhcluster14_rural)
hhcluster14u_proj <- SpatialPointsDataFrame(spTransform(SpatialPoints(hhcluster14_urban, crs.geo), crs.proj), hhcluster14_urban)

rural08_buffer <- gBuffer(hhcluster08r_proj, width = 5000, byid = TRUE)
urban08_buffer <- gBuffer(hhcluster08u_proj, width = 2000, byid = TRUE)
rural14_buffer <- gBuffer(hhcluster14r_proj, width = 5000, byid = TRUE)
urban14_buffer <- gBuffer(hhcluster14u_proj, width = 2000, byid = TRUE)

writeOGR(urban14_buffer, "Kenya Shapefiles", "cluster_urban_14", driver = "ESRI Shapefile")
writeOGR(rural14_buffer, "Kenya Shapefiles", "cluster_rural_14", driver = "ESRI Shapefile")

writeOGR(urban08_buffer, "Kenya Shapefiles", "cluster_urban_08", driver = "ESRI Shapefile")
writeOGR(rural08_buffer, "Kenya Shapefiles", "cluster_rural_08", driver = "ESRI Shapefile")

load("C:/Users/YeGuyu/Dropbox/Thesis/results08.Rda")
load("C:/Users/YeGuyu/Dropbox/Thesis/normalized_14.Rda")

colnames(results08) <- c("ex_dsmp", "Cluster", "ex_dsmp_n")
#histogram(results08$ex_dsmp_n)
#histogram(normalized_14$pred)
hhcluster08r_proj@data <- merge(hhcluster08r_proj@data, results08, by = "Cluster")
hhcluster08u_proj@data <- merge(hhcluster08u_proj@data, results08, by = "Cluster")
hhcluster08_proj <- rbind(hhcluster08r_proj, hhcluster08u_proj)

nightlight_county_08 <- over(kenya_proj, hhcluster08_proj, fun = mean, na.rm = TRUE)
kenya_proj@data$nightlight08 <- nightlight_county_08

hhcluster14r_proj@data <- merge(hhcluster14r_proj@data, normalized_14, by.x = "Cluster", by.y = "ex_2014$V11")
hhcluster14u_proj@data <- merge(hhcluster14u_proj@data, normalized_14, by.x = "Cluster", by.y = "ex_2014$V11")
hhcluster14_proj <- rbind(hhcluster14r_proj, hhcluster14u_proj, na.rm = TRUE)


writeOGR(hhcluster_sp, "Kenya shapefiles", "hhcluster_sp", driver = "ESRI Shapefile")

results08$ID <- paste0("2009", results08$Cluster)
colnames(normalized_14) <- c("ex_viir", "ex_dsmp_n", "Cluster")
normalized_14$ID <- paste0("2014", normalized_14$Cluster)
results08$ex_viir <- NA
normalized_14$ex_dsmp <- NA
nightlight_all <- rbind(results08, normalized_14)

collapsed.panel_1 <- merge(collapsed.panel_1, nightlight_all, by = "ID", all = TRUE)
save(collapsed.panel_1, file = "collapsed.panel_1.Rda")


rural08_buffer <- rbind(rural08_buffer, hhcluster08_rural)

rural08.df <- SpatialPolygonsDataFrame(rural08_buffer, data.frame(id=ID, row.names=ID))

urban08_buffer <- gBuffer(hhcluster08u_proj, width = 2000, byid = TRUE)

rural14_buffer <- gBuffer(hhcluster14r_proj, width = 5000, byid = TRUE)
urban14_buffer <- gBuffer(hhcluster14u_proj, width = 2000, byid = TRUE)

# Vincenty Ellipsoid
# calculates distance between unprojected points and lines
hhcluster08_buffer_sp <- spTransform(hhcluster08_buffer, crs.geo)
#dist08_rural <- gDistance(powerline09_proj, hhcluster08_buffer, byid = TRUE)
dist08_urban <- gDistance(powerline09_proj, urban08_buffer, byid = TRUE)
View(dist08_urban)



## Maybe will be regression calibration instead

### =================================================================================================================
### Distance with buffer and no deviation
### Buffer originially set at 2km

## Create buffer around polylines
## Choose a distance to create treatment and control
## Try 2 km
powerline09_buffer <- gBuffer(powerline09_proj, width = 2000)
plot(powerline09_buffer)
powerline14_buffer <- gBuffer(powerline14_proj, width = 2000)
plot(powerline14_buffer)


dist08_3 <- numeric(nrow(hhcluster08_proj@coords))
for (i in (1:nrow(hhcluster08_proj@coords))) {
  dist08_3[i] <- gDistance(hhcluster08_proj[i,], powerline09_buffer)
}
dist08_3 <- cbind(hhcluster08_proj$Cluster, as.data.frame(dist08_3))
setnames(dist08_3, old = c('hhcluster08_proj$Cluster', 'dist08_3'), new = c('Cluster', 'dist2'))
dist08_3$Year <- "2009"

dist14_3 <- numeric(nrow(hhcluster14_proj@coords))
for (i in (1:nrow(hhcluster14_proj@coords))) {
  dist14_3[i] <- gDistance(hhcluster14_proj[i,], powerline14_buffer)
}
dist14_3 <- cbind(hhcluster14_proj$Cluster, as.data.frame(dist14_3))
setnames(dist14_3, old = c('hhcluster14_proj$Cluster', 'dist14_3'), new = c('Cluster', 'dist2'))
dist14_3$Year <- "2014"
newdist <- rbind(dist08_3, dist14_3)


## clusters within buffer has distance of 0. Can directly generate treatment and control from this
## Merge with other household-level data
## Collapsed dataset

collapsed.panel_1 <- merge(collapsed.panel_1, newdist, by = c("Cluster", "Year"))
#range(collapsed.panel_1$Educatoin_1, na.rm = TRUE)
collapsed.panel_1 <- collapsed.panel_1[, c("Cluster","Year", "Region", "County", "Electricity", "Wealth_Score","Number_of_Household_Members",
                                   "Dist", "Urban", "Longnum", "Latnum", "Altitude", "Education_1", "Education_2", "Education_3", "Education_4", "dist2")]

Education <- collapsed.panel_1[, c("Education_1", "Education_2", "Education_3", "Education_4")]
collapsed.panel_1$Avg_Edu <- apply(Education, 1, mean, na.rm = T)
collapsed.panel_1$Avg_Edu <- ifelse(collapsed.panel_1$Avg_Edu == "NaN", 0, collapsed.panel_1$Avg_Edu)
collapsed.panel_1$Highest_Ed <- apply(Education, 1, max, na.rm = T)
collapsed.panel_1$Highest_Ed <- ifelse(collapsed.panel_1$Highest_Ed== "-Inf", 0, collapsed.panel_1$Highest_Ed)
collapsed.panel_1$EduAll <- rowSums(Education[, c("Education_1", "Education_2", "Education_3", "Education_4")], na.rm = TRUE)
collapsed.panel_1$Edu_No <- collapsed.panel_1$EduAll/collapsed.panel_1$Avg_Edu
collapsed.panel_1$Edu_No <- ifelse(collapsed.panel_1$Edu_No == "NaN", 0, collapsed.panel_1$Edu_No)


save(collapsed.panel_1, file="collapsed.panel_1.Rda")
### ====================================================================================================================
### Spatial join - collapsed panel with spatial points

#collapsed.panel_1$Dist2 <- collapsed.panel_1$Dist-2000
hhcluster <- rbind(hhcluster08[, c("Longnum", "Latnum", "Cluster", "Year")], hhcluster14[, c("Longnum","Latnum","Cluster", "Year")])
hhcluster_sp <- SpatialPointsDataFrame(collapsed.panel_1[, c("Longnum", "Latnum")], collapsed.panel_1, coords.nrs = numeric(0), crs.geo)

writeOGR(hhcluster_sp, "Kenya shapefiles", "hhcluster_sp", driver = "ESRI Shapefile")


#### Move to Geoda to create spatial weight matrices and spatially lagged variable
#### k5 is the best weight matrix
#### Import dataset back and save for analysis

hhcluster_sp_w <- readOGR("Kenya Shapefiles\\hhcluster_sp_w.shp", layer = "hhcluster_sp_w")
collapsed.panel_1_w <- hhcluster_sp_w@data
setnames(collapsed.panel_1_w, old = c('Elctrct', 'W_ELEC', 'Wlth_Sc', 'W_WLTH_SCR', 'Nm__H_M','dist2.x'),
         new = c('Electricity','W_Electricity', 'Wealth_Score', 'W_Wealth_Score', 'Number_of_Household_Members', 'Dist2'))
save(collapsed.panel_1_w, file="collapsed.panel_1_w.Rda")



k5 <- read.gwt2nb("Kenya shapefiles\\k5.gwt", region.id = POLY_ID)
k5.listw <- nb2listw(k5)

collapsed.panel_1$w_Wealth <- lag.listw(k5.listw, collapsed.panel_1$Wealth_Score)
collapsed.panel_1$w_AvgEdu <- lag.listw(k5.listw, collapsed.panel_1$Avg_Edu)
#collapsed.panel_1$w_Electric <- lag.listw(k5.listw, collapsed.panel_1$Electricity)

save(collapsed.panel_1, file="collapsed.panel_1.Rda")


### Analysis for Thesis

setwd("C:\\Users\\YeGuyu\\Desktop\\Thesis")
load("collapsed.panel_1.Rda")
library(investr)
library(stargazer)
library(lmtest)
library(tseries)

### Two Stage Least Squares and use distance as IV
collapsed.panel_1 <- subset(collapsed.panel_1, Electricity != "NA")
attach(collapsed.panel_1)

collapsed.panel_1$Region[collapsed.panel_1$Region == "north eastern"] <- "northeastern"
collapsed.panel_1$County[collapsed.panel_1$County == "Keiyo-Marakwet"] <- "Elgeyo Marakwet"
collapsed.panel_1$County[collapsed.panel_1$County == "Trans-Nzoia"] <- "Trans Nzoia"
collapsed.panel_1$County[collapsed.panel_1$County == "Tharaka-Nithi"] <- "Tharaka"

stargazer(collapsed.panel_1, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")
