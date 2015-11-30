library(e1071)
library(lattice)
library(plyr)
library(xtable)

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

hannel_vs_timegroup_table <- table(data$channel, data$timegroup)
channel_vs_weekday_table   <- table(data$channel, data$weekday)
weekday_vs_timegroup_table <- table(data$weekday, data$timegroup)

# relative frequencies tables
addmargins(prop.table(channel_vs_timegroup_table)) * 100
addmargins(prop.table(channel_vs_weekday_table)) * 100
addmargins(prop.table(weekday_vs_timegroup_table)) * 100



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



#######################################
### 3. chi2                         ###
#######################################

get_pretty_table_chi2 <- function(v1,v2) {
  mat <- getQueteletIndex(v1, v2)[[2]]
  chi <- as.matrix(mat)
  colnames(chi) <- NULL
  chi <- data.frame(matrix(unlist(chi), ncol = ncol(mat)))
  chi2 <- chi^2
  res <- data.frame(matrix("", nrow = nrow(chi) + 1, ncol = ncol(chi) + 1), stringsAsFactors = FALSE)
  for (i in 1:nrow(chi)) {
    for (j in 1:ncol(chi)) {
      res[i,j] <- paste(round(chi[i,j],3), " (", round(chi2[i,j],3), ")")
    }
    res[i,ncol(chi)+1] <- paste('(', round(sum(chi2[i,]),3), ')')
  }
  for (j in 1:ncol(chi)) {
    res[nrow(chi) + 1,j] <- paste('(', round(sum(chi2[,j]),3), ')')
  }
  res[nrow(chi) + 1,ncol(chi)+1] = sum(chi2)
  if (length(colnames(mat)) > 0) {
    colnames(res)[1:ncol(mat)] <- colnames(mat)
  }
  if (length(rownames(mat)) > 0) {
    rownames(res)[1:nrow(mat)] <- rownames(mat)
  }
  colnames(res)[ncol(mat) + 1] <- "Sum"
  rownames(res)[nrow(mat) + 1] <- "Sum"
  xtable(res)
}

data <- data[sample(1:10000, size = 1000, replace = FALSE),]
get_pretty_table_chi2(data$channel, data$timegroup)
get_pretty_table_chi2(data$channel, data$weekday)

#################################################
### 4. Sufficient sample size for dependence  ###
#################################################

# For size = 1e4*1.325 = 13250 we get p-value = 0.05
chisq.test(table(data$channel, data$weekday)*1.0)

# For size = 16000 we get p-value = 0.01
chisq.test(table(data$channel, data$timegroup))

