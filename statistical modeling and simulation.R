### Statistical Analysis and Simulation


load("kenya.panel.Rda")
load("kenya.collapsed_08.Rda")
load("kenya.collapsed_14.Rda")

## KNN created in Geoda
# Generate spatially weighted variables for two years separately since there was one spatial matrix for each year.
k5 <- read.gwt2nb("Kenya shapefiles\\k5.gwt", region.id = POLY_ID)
k5.listw <- nb2listw(k5)
cluster08 <- as.data.frame(hhcluster08_sp1@data[, "Cluster" ])
colnames(cluster08) <- "Cluster"
kenya.collapsed2_08 <- merge(kenya.collapsed2_08, cluster08, by = "Cluster")
kenya.collapsed2_08$w_Children <- lag.listw(k5_09.listw, kenya.collapsed2_08$Children_Under_14)
kenya.collapsed2_08$w_HHM <- lag.listw(k5_09.listw, kenya.collapsed2_08$Number_of_Household_Members)
kenya.collapsed2_08$w_SexHead <- lag.listw(k5_09.listw, kenya.collapsed2_08$Sex_HH_Head1)
kenya.collapsed2_08$w_AgeHH <- lag.listw(k5_09.listw, kenya.collapsed2_08$Age_HH_Head)
kenya.collapsed2_08$w_AgLand <- lag.listw(k5_09.listw, kenya.collapsed2_08$Own_Ag_Land)
kenya.collapsed2_08$w_Livestock <- lag.listw(k5_09.listw, kenya.collapsed2_08$Own_Livestock)
#kenya.collapsed2_08$w_dsmp <- lag.listw(k5_09.listw, kenya.collapsed2_08$ex_dsmp_n)
kenya.collapsed2_08$w_dsmp <- NULL
kenya.collapsed2_08$w_Wealth <- lag.listw(k5_09.listw, kenya.collapsed2_08$Wealth_Score)
kenya.collapsed2_08$w_urban <- lag.listw(k5_09.listw, kenya.collapsed2_08$urban)
kenya.collapsed2_08$w_Electric <- lag.listw(k5_09.listw, kenya.collapsed2_08$Electricity)
kenya.collapsed2_08$ww_Electric <- lag.listw(k5_09.listw, kenya.collapsed2_08$w_Electric)
kenya.collapsed2_08$ww_AvgEdu <- lag.listw(k5_09.listw, kenya.collapsed2_08$w_AvgEdu)



cluster14 <- as.data.frame(hhcluster14_sp1@data[, "Cluster" ])
colnames(cluster14) <- "Cluster"
kenya.collapsed2_14 <- merge(kenya.collapsed2_14, cluster14, by.x = "Cluster.x", by.y = "Cluster")
kenya.collapsed2_14$w_Children <- lag.listw(k5_14.listw, kenya.collapsed2_14$Children_Under_14)
kenya.collapsed2_14$w_HHM <- lag.listw(k5_14.listw, kenya.collapsed2_14$Number_of_Household_Members)
kenya.collapsed2_14$w_SexHead <- lag.listw(k5_14.listw, kenya.collapsed2_14$Sex_HH_Head1)
kenya.collapsed2_14$w_AgeHH <- lag.listw(k5_14.listw, kenya.collapsed2_14$Age_HH_Head)
kenya.collapsed2_14$w_AgLand <- lag.listw(k5_14.listw, kenya.collapsed2_14$Own_Ag_Land)
kenya.collapsed2_14$w_Livestock <- lag.listw(k5_14.listw, kenya.collapsed2_14$Own_Livestock)
#kenya.collapsed2_14$w_dsmp <- lag.listw(k5_14.listw, kenya.collapsed2_14$ex_dsmp_n)
kenya.collapsed2_14$w_Wealth <- lag.listw(k5_14.listw, kenya.collapsed2_14$Wealth_Score)
kenya.collapsed2_14$w_urban <- lag.listw(k5_14.listw, kenya.collapsed2_14$urban)
kenya.collapsed2_14$w_Electric <- lag.listw(k5_14.listw, kenya.collapsed2_14$Electricity)
kenya.collapsed2_14$ww_Electric <- lag.listw(k5_14.listw, kenya.collapsed2_14$w_Electric)
kenya.collapsed2_14$ww_AvgEdu <- lag.listw(k5_14.listw, kenya.collapsed2_14$w_AvgEdu)

# Combine two years of data
kenya.collapsed2_all <- rbind(kenya.collapsed2_08, kenya.collapsed2_14)
save(kenya.collapsed2_all, file = "kenya.collapsed2_all.Rda")
hhcluster_sp <- readShapePoints("Kenya shapefiles/hhcluster_sp.shp")
hhcluster14_sp1 <- subset(hhcluster_sp, Year == "2014")
hhcluster14_sp1 <- subset(hhcluster14_sp1, !is.na(hhcluster14_sp1$Avg_Edu))

ggplot(data = kenya.collapsed2_all, aes(x = Dist, y = Avg_Edu, color = group)) +
  geom_point() + xlab("Distance to Grid") + ylab("Years of Schooling")+theme_bw()
ggsave("education_dist.png")


### Statistical Modeling

## Basic Instrumental Variable Model (2SLS)
library(AER)
preds.all <- cbind(Children_Under_14, Sex_HH_Head1, Age_HH_Head, Own_Ag_Land,
                   Own_Livestock, urban, Number_of_Household_Members)
wpred.all <- cbind(w_Children, w_SexHead, w_AgeHH, w_AgLand, w_Livestock, w_urban, w_HHM)

kenya.collapsed2_all$logDist <- log(kenya.collapsed2_all$Dist)

## First stage without spatial components
# Four models: Basic model, basic + year fixed effect, basic + year fixed effect + region fixed effect, basic + year fixed
# effect + county fixed effect

# First stage without nighttime light
first1_ns <- lm(Electricity~log(Dist)+preds.all, data = kenya.collapsed2_all)
first2_ns <- lm(Electricity~log(Dist)+preds.all + factor(Year.x)-1, data = kenya.collapsed2_all)
first3_ns <- lm(Electricity~log(Dist)+preds.all+factor(Year.x)-1 + factor(Region)-1, data = kenya.collapsed2_all)
first4_ns <- lm(Electricity~log(Dist)+preds.all+ factor(Year.x)-1 + factor(County)-1, data = kenya.collapsed2_all)
# First stage with nighttime light
first1_ns_l <- lm(Electricity~log(Dist)+log(ex_dsmp_ad)+preds.all, data = kenya.collapsed2_all)
first2_ns_l <- lm(Electricity~log(Dist)+log(ex_dsmp_ad)+preds.all + factor(Year.x)-1, data = kenya.collapsed2_all)
first3_ns_l <- lm(Electricity~log(Dist)+log(ex_dsmp_ad)+preds.all+factor(Year.x)-1 + factor(Region)-1, data = kenya.collapsed2_all)
first4_ns_l <- lm(Electricity~log(Dist)+log(ex_dsmp_ad)+preds.all+ factor(Year.x)-1 + factor(County)-1, data = kenya.collapsed2_all)

# Summary results in table
stargazer(first1_ns, first2_ns, first3_ns, first4_ns, type = "text", align = TRUE, no.space = TRUE, out = "first_stage_ns.txt")

# simple F-test
first1_test <- lm(Electricity~preds.all, data = kenya.collapsed2_all)
first2_test <- lm(Electricity~preds.all + factor(Year.x)-1, data = kenya.collapsed2_all)
first3_test <- lm(Electricity~preds.all+factor(Year.x)-1 + factor(Region)-1, data = kenya.collapsed2_all)
first4_test <- lm(Electricity~preds.all+ factor(Year.x)-1 + factor(County)-1, data = kenya.collapsed2_all)

first1_testl <- lm(Electricity~log(Dist)+log(ex_dsmp_ad)+preds.all, data = kenya.collapsed2_all)
first2_testl <- lm(Electricity~log(Dist)+log(ex_dsmp_ad)+preds.all + factor(Year.x)-1, data = kenya.collapsed2_all)
first3_testl <- lm(Electricity~log(Dist)+log(ex_dsmp_ad)+preds.all+factor(Year.x)-1 + factor(Region)-1, data = kenya.collapsed2_all)
first4_testl <- lm(Electricity~log(Dist)+log(ex_dsmp_ad)+preds.all+ factor(Year.x)-1 + factor(County)-1, data = kenya.collapsed2_all)

# F-test robust to heteroskedasticity: How different would the models be without instruments
waldtest(first1_ns, first1_test, vcov = vcovHC(first1_ns, type="HC0"))$F[2]
waldtest(first2_ns, first2_test, vcov = vcovHC(first2_ns, type="HC0"))$F[2]
waldtest(first3_ns, first3_test, vcov = vcovHC(first3_ns, type="HC0"))$F[2]
waldtest(first4_ns, first4_test, vcov = vcovHC(first4_ns, type="HC0"))$F[2]

waldtest(first1_ns_l, first1_testl, vcov = vcovHC(first1_ns_l, type="HC0"))$F[2]
waldtest(first2_ns_l, first2_testl, vcov = vcovHC(first2_ns_l, type="HC0"))$F[2]
waldtest(first3_ns_l, first3_testl, vcov = vcovHC(first3_ns_l, type="HC0"))$F[2]
waldtest(first4_ns_l, first4_testl, vcov = vcovHC(first4_ns_l, type="HC0"))$F[2]

# Prediction based on model
kenya.collapsed2_all$ns_pred1 <- predict(first1_ns)
kenya.collapsed2_all$ns_pred2 <- predict(first2_ns)
kenya.collapsed2_all$ns_pred3 <- predict(first3_ns)
kenya.collapsed2_all$ns_pred4 <- predict(first4_ns)


## Second Stage

# Second stage with nighttime light

iv_ns1 <- ivreg(Avg_Edu~Electricity+Electricity*log(ex_dsmp_ad)+log(ex_dsmp_ad)+preds.all |log(ex_dsmp_ad)+light_pred1*log(ex_dsmp_ad)+preds.all+log(Dist)+Dist, data = kenya.collapsed2_all)
iv_ns2 <- ivreg(Avg_Edu~Electricity+Electricity*log(ex_dsmp_ad)+log(ex_dsmp_ad)+preds.all+ factor(Year.x)-1 |log(ex_dsmp_ad)+preds.all+light_pred2*log(ex_dsmp_ad)+ factor(Year.x)-1 + log(Dist)+Dist, data = kenya.collapsed2_all)
iv_ns3 <- ivreg(Avg_Edu~Electricity+Electricity*log(ex_dsmp_ad)+log(ex_dsmp_ad)+preds.all+ factor(Year.x)-1 + factor(Region)-1|log(ex_dsmp_ad)+preds.all +light_pred3*log(ex_dsmp_ad) +factor(Year.x)-1 + factor(Region)-1+ log(Dist)+Dist, data = kenya.collapsed2_all)
iv_ns4 <- ivreg(Avg_Edu~Electricity+Electricity*log(ex_dsmp_ad)+log(ex_dsmp_ad)+preds.all+ factor(Year.x)-1 + factor(County)-1| log(ex_dsmp_ad)+preds.all +light_pred4*log(ex_dsmp_ad) + factor(Year.x)-1 + factor(County)-1+ log(Dist)+Dist, data = kenya.collapsed2_all)

stargazer(iv_ns1, iv_ns2, iv_ns3, iv_ns4, type = "text", align = TRUE, no.space = TRUE, out = "IV_Non-spatial_new.txt")

iv_ns1_t <- ivreg(Avg_Edu~Electricity+preds.all |preds.all+log(Dist)+Dist, data = kenya.collapsed2_all)
iv_ns2_t <- ivreg(Avg_Edu~Electricity+preds.all+ factor(Year.x)-1 |preds.all+factor(Year.x)-1 +log(Dist)+Dist, data = kenya.collapsed2_all)
iv_ns3_t <- ivreg(Avg_Edu~Electricity+preds.all+ factor(Year.x)-1 + factor(Region)-1|preds.all  +factor(Year.x)-1 + factor(Region)-1+ log(Dist) + Dist, data = kenya.collapsed2_all)
iv_ns4_t <- ivreg(Avg_Edu~Electricity+preds.all+ factor(Year.x)-1 + factor(County)-1| preds.all  + factor(Year.x)-1 + factor(County)-1+ log(Dist) + Dist, data = kenya.collapsed2_all)

Sargan_ns1<- lm(iv_ns1$residuals~log(Dist)+preds.all+log(ex_dsmp_ad), data=kenya.collapsed2_all)
Sargan_test1 <- Sargan_ns1$r.squared*nrow(kenya.collapsed2_all)
print(1-pchisq(Sargan_test1,1))

summary(iv_ns1, vcov = sandwich, diagnostics = TRUE)


## Spatial Models
library(spdep)

# Moran's I's to test spatial autocorrelation
moran.test(hhcluster08_sp1$Avg_Edu.x, k5_09.listw)
moran.test(hhcluster08_sp1$Number_of_Household_Members, k5_09.listw)
moran.test(hhcluster08_sp1$urban, k5_09.listw)
moran.test(hhcluster08_sp1$Sex_HH_Head1, k5_09.listw)
moran.test(hhcluster08_sp1$Age_HH_Head, k5_09.listw)
moran.test(hhcluster08_sp1$Children_Under_14, k5_09.listw)
moran.test(hhcluster08_sp1$Own_Ag_Land, k5_09.listw)
moran.test(hhcluster08_sp1$Own_Livestock, k5_09.listw)

moran.test(hhcluster14_sp1$Avg_Edu.x, k5_14.listw)
moran.test(hhcluster14_sp1$Number_of_Household_Members, k5_14.listw)
moran.test(hhcluster14_sp1$urban, k5_14.listw)
moran.test(hhcluster14_sp1$Sex_HH_Head1, k5_14.listw)
moran.test(hhcluster14_sp1$Age_HH_Head, k5_14.listw)
moran.test(hhcluster14_sp1$Children_Under_14, k5_14.listw)
moran.test(hhcluster14_sp1$Own_Ag_Land, k5_14.listw)
moran.test(hhcluster14_sp1$Own_Livestock, k5_14.listw)

# Spatially weighted educational attainmen is also potentially endogenous. To cope with this issue,
# I added a second first stage to instrument this variable with doubly spatially weighted educational
# attainment.

# Second first stage
spatial2_1 <- lm(w_AvgEdu~ww_AvgEdu+wpred.all, data = kenya.collapsed2_all)
spatial2_2 <- lm(w_AvgEdu~ww_AvgEdu+wpred.all + factor(Year.x)-1, data = kenya.collapsed2_all)
spatial2_3 <- lm(w_AvgEdu~ww_AvgEdu+wpred.all + factor(Year.x)-1 + factor(Region)-1, data = kenya.collapsed2_all)
spatial2_4 <- lm(w_AvgEdu~ww_AvgEdu+wpred.all + factor(Year.x)-1 + factor(County)-1, data = kenya.collapsed2_all)
stargazer(spatial2_1,spatial2_2,spatial2_3, spatial2_4, type = "text", align = TRUE, no.space = TRUE, out = "IV_Spatial_second_first_stage.txt")

# Second stage without nighttime light
iv_s1_nl <- ivreg(Avg_Edu~Electricity + w_AvgEdu + preds.all |  preds.all + log(Dist) + ww_AvgEdu + wpred.all, data = kenya.collapsed2_all)
iv_s2_nl <- ivreg(Avg_Edu~Electricity + w_AvgEdu + preds.all + factor(Year.x)-1 |  preds.all + factor(Year.x)-1 + log(Dist) + ww_AvgEdu + wpred.all, data = kenya.collapsed2_all)
iv_s3_nl <- ivreg(Avg_Edu~Electricity + w_AvgEdu + preds.all + factor(Year.x)-1 + factor(Region)-1|  preds.all + factor(Year.x)-1 + factor(Region)-1+ log(Dist) + ww_AvgEdu + wpred.all, data = kenya.collapsed2_all)
iv_s4_nl <- ivreg(Avg_Edu~Electricity + w_AvgEdu + preds.all + factor(Year.x)-1 + factor(County)-1|  preds.all + factor(Year.x)-1 + factor(County)-1+ log(Dist) + ww_AvgEdu + wpred.all, data = kenya.collapsed2_all)

# Second stage with nighttime light
iv_s1 <- ivreg(Avg_Edu~Electricity+Electricity*log(ex_dsmp_ad)+log(ex_dsmp_ad)+ w_AvgEdu+preds.all |preds.all+Electricity*log(ex_dsmp_ad)+log(ex_dsmp_ad) +log(Dist)+ww_AvgEdu, data = kenya.collapsed2_all)
iv_s2 <- ivreg(Avg_Edu~Electricity+Electricity*log(ex_dsmp_ad)+log(ex_dsmp_ad)+ w_AvgEdu+preds.all + factor(Year.x)-1 |log(ex_dsmp_ad)+preds.all+Electricity*log(ex_dsmp_ad) +factor(Year.x)-1+ log(Dist) + ww_AvgEdu, data = kenya.collapsed2_all)
iv_s3 <- ivreg(Avg_Edu~Electricity+Electricity*log(ex_dsmp_ad)+log(ex_dsmp_ad)+ w_AvgEdu+preds.all+factor(Year.x)-1 + factor(Region)-1 |log(ex_dsmp_ad)+preds.all+Electricity*log(ex_dsmp_ad) +factor(Year.x)-1+factor(Region)-1+ log(Dist) + ww_AvgEdu, data = kenya.collapsed2_all)
iv_s4 <- ivreg(Avg_Edu~Electricity+Electricity*log(ex_dsmp_ad)+log(ex_dsmp_ad)+ w_AvgEdu+preds.all +factor(Year.x)-1 + factor(County)-1|log(ex_dsmp_ad)+preds.all+Electricity*log(ex_dsmp_ad) +factor(Year.x)-1+factor(County)-1  + log(Dist) + ww_AvgEdu, data = kenya.collapsed2_all)

stargazer(iv_s1_nl, iv_s2_nl,iv_s3_nl, iv_s4_nl, type = "text", align = TRUE, no.space = TRUE, out = "IV_Spatial_NoLight.txt")
stargazer(iv_s1, iv_s2,iv_s3, iv_s4, type = "text", align = TRUE, no.space = TRUE, out = "IV_Spatial_Light.txt")


### Simulation

## Step 1: Generate new dataset
## a. decrease 10% in distance overall
## b. decrease 20% in distance overall
## c. decrease 50% in distance overall
## d. decrease 10% urban, 50% rural

detach(kenya.collapsed2_all)
all_10 <- kenya.collapsed2_all %>% mutate(Electricity = ifelse(Electricity<1&Electricity+0.1<1, Electricity+0.1, 1))
all_20 <- kenya.collapsed2_all %>% mutate(Electricity = ifelse(Electricity<1&Electricity+0.2<1, Electricity+0.2, 1))
all_50 <- kenya.collapsed2_all %>% mutate(Electricity = ifelse(Electricity<1&Electricity+0.5<1, Electricity+0.5, 1))
u10_r50 <- kenya.collapsed2_all %>% mutate(Electricity = ifelse(Electricity+0.1<1, (ifelse(urban=="1", Electricity+0.1, (ifelse(Electricity+0.5<1, Electricity+0.5, 1)))),1))

all_10 <- all_10 %>% mutate(ex_dsmp_ad = ifelse(ex_dsmp_ad*1.1<63, ex_dsmp_ad*1.1, 63))
all_20 <- all_20 %>% mutate(ex_dsmp_ad = ifelse(ex_dsmp_ad*1.2<63, ex_dsmp_ad*1.2, 63))
all_50 <- all_50 %>% mutate(ex_dsmp_ad = ifelse(ex_dsmp_ad*1.5<63, ex_dsmp_ad*1.5, 63))
u10_r50 <- u10_r50 %>% mutate(ex_dsmp_ad = ifelse(ex_dsmp_ad*1.1<63, (ifelse(urban=="1", ex_dsmp_ad*1.1, (ifelse(ex_dsmp_ad*1.5<63, ex_dsmp_ad*1.5, 63)))),63))

# The last model is selected to simulate these scenarios
all_10$prdct10<- predict(iv_ns4, newdata = all_10, type = "response")
all_20$prdct20<- predict(iv_ns4, newdata = all_20, type = "response")
all_50$prdct50<- predict(iv_ns4, newdata = all_50, type = "response")
u10_r50$prdct_mix<- predict(iv_ns4, newdata = u10_r50, type = "response")

all_10$prdct10_s<- predict(iv_s4, newdata = all_10, type = "response")
all_20$prdct20_s<- predict(iv_s4, newdata = all_20, type = "response")
all_50$prdct50_s<- predict(iv_s4, newdata = all_50, type = "response")
u10_r50$prdct_mix_s<- predict(iv_s4, newdata = u10_r50, type = "response")

#edu_collapse <- summaryBy()
other_collapse <- summaryBy(Avg_Edu+Electricity+ex_dsmp_ad~County+Year.x, data = all_10, FUN = mean, keep.names = TRUE)
other_collapse1 <- subset(other_collapse, other_collapse$Year.x == "2009")
other_collapse2 <- subset(other_collapse, other_collapse$Year.x == "2014")
other_reshape <- cbind(other_collapse1, other_collapse2)
colnames(other_reshape) <- c("County", "Year.x", "AvgEdu08", "Electric08", "ex_dsmp_ad08", "County1", "Year.y", "AvgEdu14", "Electric14", "ex_dsmp_ad14")
other_reshape$Year.x <- NULL
other_reshape$Year.y <- NULL
other_reshape$County1 <- NULL

all_10_collapsed1 <-summaryBy(prdct10 + prdct10_s~County, data = all_10, FUN = mean, keep.names = TRUE) 
all_20_collapsed1 <-summaryBy(prdct20 + prdct20_s~County, data = all_20, FUN = mean, keep.names = TRUE) 
all_50_collapsed1 <-summaryBy(prdct50 + prdct50_s~County, data = all_50, FUN = mean, keep.names = TRUE) 
u10_r50_collapsed1 <-summaryBy(prdct_mix + prdct_mix_s ~County, data = u10_r50, FUN = mean, keep.names = TRUE) 

kenya <- readShapePoly("Kenya shapefiles/County.shp")
proj4string(kenya) <- crs.geo
kenya.proj <- spTransform(kenya, crs.proj)
kenya.proj@data$County <- as.character(kenya.proj@data$COUNTY)
all_10_collapsed1$County[all_10_collapsed1$County == "Elgeyo Marakwet"] <- "Keiyo-Marakwet"
all_20_collapsed1$County[all_20_collapsed1$County == "Elgeyo Marakwet"] <- "Keiyo-Marakwet"
all_50_collapsed1$County[all_50_collapsed1$County == "Elgeyo Marakwet"] <- "Keiyo-Marakwet"
u10_r50_collapsed1$County[u10_r50_collapsed1$County == "Elgeyo Marakwet"] <- "Keiyo-Marakwet"

kenya.proj_simulation <- merge(kenya.proj, all_10_collapsed1)
kenya.proj_simulation <- merge(kenya.proj_simulation, all_20_collapsed1)
kenya.proj_simulation <- merge(kenya.proj_simulation, all_50_collapsed1)
kenya.proj_simulation <- merge(kenya.proj_simulation, u10_r50_collapsed1)
kenya.proj_simulation <- merge(kenya.proj_simulation, other_reshape, by = "County")
writeOGR(kenya.proj_simulation, "Kenya shapefiles2", "kenya.proj_simulaition", driver = "ESRI Shapefile")

