
### This script reads and projects geospatial files, calculates distance to grid, collapses DHS household-level data, 
### and linkes shapefiles to DHS data

## Load required packages and set working directory

packages <- c("readstata13","geosphere", "sp", "rgdal", "maptools","mapproj", "gdistance","rgeos","dplyr", "lattice",
              "car", "foreign", "gplots","ggplot2", "spdep", "plm", "sphet", "zoo", "pglm", "plyr", "doBy", "PBSmapping", "data.table")
lapply(packages, require, character.only = TRUE)
setwd("P:\\Thesis")

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


## Data for 2014
KEHR.14 <- read.dta13("Thesis\\KEHR_2014_Small.dta")
#View(KEHR.14)
Cluster_Settlement_14 <- subset(KEHR.14, select = c(Cluster, Settlement))
Cluster_Settlement_14 <- subset(Cluster_Settlement_14,!duplicated(Cluster_Settlement_14$Cluster))
Cluster_Settlement_14$urban <- ifelse(Cluster_Settlement_14$Settlement=="urban", 1,0)
KEHR.14$County <- gsub(".   ", "", KEHR.14$County)
KEHR.14$County <- str_to_title(KEHR.14$County)

## Read in shapefiles for power grids and transform to projected system
## Digitized power supply map for 2006 is used as a proxy for pre-existing power grid for 2008/09
## Digitized power supply map for 2012 is used as a proxy for pre-existing power grid for 2014

powerline09 <- readOGR("Thesis\\poweline2006.shp", layer = "poweline2006")
powerline14 <- readOGR("Thesis\\powerline_2012_Project.shp", layer = "powerline_2012_Project")
#Source for 2006 map: http://unesdoc.unesco.org/images/0014/001488/148866E.pdf

### Define geographic and projected systems
crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs.proj <- CRS("+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 
                +datum=WGS84 +units=m +no_defs")

## Transform polylines and polygons to projected system
powerline09_proj <- spTransform(powerline09, crs.proj)
powerline14_proj <- spTransform(powerline14, crs.proj)
plot(powerline09)
plot(powerline14)

kenya <- readShapePoly("C:/Users/YeGuyu/Desktop/Thesis/Kenya shapefiles/County.shp")
kenya@proj4string <- crs.geo
kenya_proj <- spTransform(kenya, crs.proj)

### =======================================================================================================================

## Calculate distances from household clusters to pre-existing powerlines
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

## Merge distances with other household-level data
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
## Keep only the columns in common for both years
KEHR.08[,c("Microwave","DVD_Player", "CD_Player", "Unit_Ag_Land"):=NULL]

kenya.panel <- rbind(KEHR.08, KEHR.14)
save(kenya.panel, file = "kenya.panel.Rda")
#View(kenya.panel)

## Quick distribution check
ggplot(kenya.panel, aes(Number_of_Household_Members, fill=Year.x, colour=Year.x)) +
  geom_histogram(aes(y=..density..), breaks=seq(0,25,1), alpha=0.6, 
                 position="identity", lwd=0.2) +
  ggtitle("Normalized Number of Household Members")

#range(kenya.panel$Wealth_Score) #-250248  376404
ggplot(kenya.panel, aes(Wealth_Score, fill=Year.x, colour=Year.x)) +
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5)+
  ggtitle("Normalized Wealth_Score")

attach(kenya.panel)

Education <- kenya.panel[, c("Education_1", "Education_2", "Education_3", "Education_4", "Education_5",
                             "Education_6", "Education_7", "Education_8", "Education_9", "Education_10",
                             "Education_11", "Education_12", "Education_13", "Education_14", "Education_15",
                             "Education_16", "Education_17", "Education_18", "Education_19")]
kenya.panel$Avg_Edu <- apply(Education, 1, mean, na.rm = T)
kenya.panel$Avg_Edu <- ifelse(kenya.panel$Avg_Edu == "NaN", 0, kenya.panel$Avg_Edu)

kenya.panel_small <- kenya.panel[, c("Cluster", "Month", "Year.x", "Number_of_Household_Members", "Children_Under_14",
                                     "Region", "Electricity", "Sex_HH_Head", "Age_HH_Head", "Own_Ag_Land", "Own_Livestock",
                                     "Wealth_Index", "Wealth_Score", "Size_Ag_Land", "Avg_Edu", "Dist", "urban")]
kenya.panel_small$Own_Ag_Land <- ifelse(kenya.panel_small$Own_Ag_Land== "yes", 1, kenya.panel_small$Own_Ag_Land)
kenya.panel_small$Own_Ag_Land <- ifelse(kenya.panel_small$Own_Ag_Land== "no", 0, kenya.panel_small$Own_Ag_Land)
kenya.panel_small$Own_Ag_Land <- ifelse(kenya.panel_small$Own_Ag_Land== "9", NA, kenya.panel_small$Own_Ag_Land)
kenya.panel_small$Own_Ag_Land<- as.numeric(kenya.panel_small$Own_Ag_Land)

kenya.panel_small$Own_Livestock <- ifelse(kenya.panel_small$Own_Livestock== "yes", 1, kenya.panel_small$Own_Livestock)
kenya.panel_small$Own_Livestock <- ifelse(kenya.panel_small$Own_Livestock== "no", 0, kenya.panel_small$Own_Livestock)
kenya.panel_small$Own_Livestock <- ifelse(kenya.panel_small$Own_Livestock== "9", NA, kenya.panel_small$Own_Livestock)
kenya.panel_small$Own_Livestock <- as.numeric(kenya.panel_small$Own_Livestock)

kenya.panel_small$Sex_HH_Head1 <- ifelse(kenya.panel_small$Sex_HH_Head== "male", 1, kenya.panel_small$Sex_HH_Head)
kenya.panel_small$Sex_HH_Head1 <- ifelse(kenya.panel_small$Sex_HH_Head== "female", 0, kenya.panel_small$Sex_HH_Head)

kenya.panel_small$Region[kenya.panel_small$Region == "north eastern"] <- "northeastern"
kenya.panel_small$ID <- paste0(kenya.panel_small$Year.x, kenya.panel_small$Cluster)
nightlight <- ml_last[, c("ID", "ex_dsmp_n", "ex_dsmp", "w_AvgEdu")]
kenya.panel_small <- merge(kenya.panel_small, nightlight, by = "ID", all = TRUE)
kenya.panel_small <- merge(kenya.panel_small, county_id, by = "ID", all = TRUE)


kenya.collapsed2_08 <- summaryBy(Number_of_Household_Members+ Children_Under_14+Electricity+Sex_HH_Head1+
                                   Age_HH_Head+Own_Ag_Land+Own_Livestock+Wealth_Score+Size_Ag_Land+
                                   Avg_Edu+Dist+urban+ex_dsmp_n+w_AvgEdu~Cluster+Year.x
                                 +County+Region, FUN = mean, data = subset(kenya.panel_small, Year.x == "2009"), na.rm = T, keep.names = TRUE)

kenya.collapsed2_14 <- summaryBy(Number_of_Household_Members+ Children_Under_14+Electricity+Sex_HH_Head1+
                                   Age_HH_Head+Own_Ag_Land+Own_Livestock+Wealth_Score+Size_Ag_Land+
                                   Avg_Edu+Dist+urban+ex_dsmp_n+w_AvgEdu~Cluster+Year.x
                                 +County+Region, FUN = mean, data = subset(kenya.panel_small, Year.x == "2014"), na.rm = T, keep.names = TRUE)


kenya.collapsed2_08 <- subset(kenya.collapsed2_all, kenya.collapsed2_all$Year.X =="2009")
kenya.collapsed2_14 <- subset(kenya.collapsed2_all, kenya.collapsed2_all$Year.X =="2014")
save(kenya.collapsed2_08, file = "kenya.collapsed2_08.Rda")
save(kenya.collapsed2_14, file = "kenya.collapsed2_14.Rda")
