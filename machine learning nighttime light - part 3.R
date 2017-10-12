
## Normalize the data - Machine Learning
## 2014 data is projected/ predicted using the 2008 system because newer data has higher resolution and thus is more
## difficult to be predicted from a system with lower resolution.

# Scatterplot of data collected using two systems
ex_2013 <- ex_2013[, c("ex", "ex_dsmp")]

ggplot(data = ex_2013, aes(x = ex_dsmp, y = ex)) +
  geom_point()+ geom_smooth() + xlab("DSMP System") + ylab("VIIR System")+theme_bw()

ggsave("two system scatter 2013.png")

# Define function for normalization, and normalize data collected using two systems to 0-1 range
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
normalized <- as.data.frame(lapply(ex_2013, normalize))

# Setting 2/3 of the data as training data and the remaining as test data (to test the accuracy of prediction)
# Algorithm used is random forest 
ind <- sample(2, nrow(normalized), replace=TRUE, prob=c(0.67, 0.33))
ex.training <- normalized[ind==1, 1:2]
ex.test <- normalized[ind==2, 1:2]
model.light_lm <- train(ex_dsmp ~ ex
                        ,ex.training
                        ,method = "rf"
)


# Shape of scatterplot and R-squared of 94.6% suggest that this algorithm did a good job in normalizing nighttime
# brightness collected using two systems
ex1 <- predict(model.light_lm, newdata= ex.test) 
ex.test <- cbind(ex.test, ex1)
scatterplot(ex.test$ex1, ex.test$ex_dsmp)

ggplot(ex.test, aes(x=ex.test$ex_dsmp, y=ex.test$ex1)) + geom_point()+geom_smooth()+ theme_bw() + xlab("DSMP System") + ylab("VIIR System") 
ggsave("trained model.png")
model.light_lm

# Use machine-learned algorithm to predict 2014's data onto a 0-1 scale 
ex_2014 <- ex_2014[, c("V11", "ex")]
ex_2014 <- subset(ex_2014, !is.na(ex_2014$ex))
normalized_14 <- as.data.frame(normalize(ex_2014[2]), na.rm = FALSE)
normalized_14$pred <- predict(model.light_lm, newdata = normalized_14, na.rm = FALSE)
normalized_14 <- cbind(normalized_14, ex_2014$V11)
save(normalized_14, file = "normalized_14.Rda")


# Normalize 2008 data to a 0-1 range
normalized_08<- as.data.frame(normalize(results08[,1]), na.rm = FALSE)
results08 <- cbind(results08, normalized_08)
save(results08, file = "results08.Rda")

# Denormalize all data to a scale of 0-63 (the scale for DSMP system)
denormalize <- function(x,minval,maxval) {
  x*(maxval-minval) + minval
}

all_lights <- rbind(normalized_14, results08)
all_lights$ex_dsmp <- ifelse(is.na(all_lights$ex_dsmp), denormalize(all_lights$ex_dsmp_n, 0, 63), all_lights$ex_dsmp)
save(all_lights, file = "all_lights.Rda")

