library(e1071)
library(car)
library(lattice)
library(plyr)
set.seed(123)
online.news.popularity <- read.csv(file="new_sample_OnlineNewsPopularity.csv",head=TRUE,sep=",")

############################################# 
### 1. Build new nominal features         ###
############################################# 

channel <- online.news.popularity$data_channel_is_lifestyle + 
  2*online.news.popularity$data_channel_is_entertainment + 3*online.news.popularity$data_channel_is_bus +
  4*online.news.popularity$data_channel_is_socmed + 5*online.news.popularity$data_channel_is_tech +
  6*online.news.popularity$data_channel_is_world

weekday <- as.matrix(online.news.popularity[,20:26]) %*% as.matrix(1:7, nrow = 7)

numBlocks = 4
timegroup <- cut(online.news.popularity$timedelta, breaks = 4)
data <- cbind(online.news.popularity, channel, weekday, timegroup)


###################################### 
### 2. Contingency tables          ###
###################################### 

table(data$channel, data$timegroup)
table(data$channel, data$weekday)
table(data$weekday, data$timegroup)

getQueteletIndex <- function(v1, v2) {
  size <- length(v1)
  cont.table  <- table(v1, v2)
  row.sums  <- rowSums(cont.table)
  col.sums  <- colSums(cont.table)
  norm.cont.table  <-  cont.table / size
  norm.row.sums  <- row.sums / size
  norm.col.sums  <- col.sums / size
  list(Quetelet = norm.cont.table/(norm.row.sums%*% t(norm.col.sums)) - 1,
       PearsonIndexMatrix = (-norm.row.sums%*%t(norm.col.sums)  + norm.cont.table )/sqrt(norm.row.sums%*%t(norm.col.sums)))
}

getQueteletIndex(data$channel, data$timegroup)
getQueteletIndex(data$channel, data$weekday)
getQueteletIndex(data$weekday, data$timegroup)

#######################################
### 3. chi2                         ###
#######################################

chisq.test(table(data$channel, data$timegroup))
chisq.test(table(data$channel, data$weekday))
chisq.test(table(data$weekday, data$timegroup))

################################################# 
### 4. Sufficient sample size for dependence  ###
#################################################

# For size = 1e4*1.325 = 13250 we get p-value = 0.05
chisq.test(table(data$weekday, data$timegroup)*1.325)

# For size = 16000 we get p-value = 0.01
chisq.test(table(data$weekday, data$timegroup)*1.6)

