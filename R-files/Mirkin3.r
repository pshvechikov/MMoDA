library(e1071)
library(car)
library(lattice)
library(plyr)
setwd('D:/Documents/�����/������������/1 ���� ���/������, ����������� ������ ������� ������')
getwd()
set.seed(123)
online.news.popularity <- read.csv(file="code/new_sample_OnlineNewsPopularity.csv",head=TRUE,sep=",")

colnames(online.news.popularity)
features <- c(3:11,19,27:35)
windows()
pairs(online.news.popularity[,c(19:20,28:34)])

channel <- online.news.popularity$data_channel_is_lifestyle + 
  2*online.news.popularity$data_channel_is_entertainment + 3*online.news.popularity$data_channel_is_bus +
  4*online.news.popularity$data_channel_is_socmed + 5*online.news.popularity$data_channel_is_tech +
  6*online.news.popularity$data_channel_is_world

data <- subset(online.news.popularity, #shares <= 1e+05 & self_reference_avg_sharess <= 1e+05 & 
                 shares > 0 & self_reference_avg_sharess > 0) 
data <- transform(data, num_hrefs = log(num_hrefs), num_self_hrefs = log(num_self_hrefs),
                  self_reference_avg_sharess = log(self_reference_avg_sharess),
                  shares = log(shares), n_tokens_content = log(n_tokens_content))

temp <- sample(1:(nrow(data)), 1000)
scatterplotMatrix(data[temp,features])

########################################################################## 
### 1. Scatter-plot of features with "linear-like" regression          ###
########################################################################## 
#"global_sentiment_polarity"  "rate_positive_words"
feature1 <- 27 ; feature2 <- 30
windows()
xyplot(data[,feature1] ~ data[,feature2] | channel)
  
temp <- which(data$data_channel_is_tech == 1)
xyplot(data[temp,feature1] ~ data[temp,feature2])

########################################################################## 
### 2. Linear regression                                               ###
##########################################################################
model <- lm(data[temp,feature1] ~ data[temp,feature2])
summary(model)

slope <- model$coefficients[1]; b <- model$coefficients[2] #Intercept
plot(data[temp,feature2], data[temp,feature1], col = 1)
abline(slope, b, col = "red")

##########################################################################
### 3. Correlation and determinacy coefficients                        ###
##########################################################################
cor(data[temp,feature1], data[temp,feature2])

(R.sq <- cor(data[temp,feature1], data[temp,feature2])^2) #��� �� model


########################################################################## 
### 4. CI's for slope, intercept and correlation coef (via bootstrap)  ###
##########################################################################
generate_bs_data_Nd <- function(Nboot, sample.size, data) {
  n = dim(data)[1]
  bootstrap_matrix_index <- sample(1:n, size = Nboot * sample.size, replace = TRUE)
  bootstrap_list <- lapply(1:dim(data)[2], function(column)
    matrix(sapply(bootstrap_matrix_index, 
                  function(index){data[index, column]}), nrow = Nboot, ncol = sample.size))
  bootstrap_list
}

Nboot <- 5000
m <- 500
alpha <- 0.05

data.temp <- data[temp, c(feature1,feature2)]

#bs_data <- generate_bs_data_Nd(Nboot, m, data = data.temp)
#lm.coefs <- sapply(1:Nboot, function(i) lm((bs_data[[1]])[i,] ~ (bs_data[[2]])[i,])$coefficients)
load("27_28.RData")

hist(lm.coefs[1,]) #similar to normal
hist(lm.coefs[2,]) #similar to normal

ci.lvl <- -qnorm(alpha)
bs_mean <- mean(lm.coefs[1,])
bs_sd <- sd(lm.coefs[1,])
pivot_ci_slope <- c(bs_mean - ci.lvl * bs_sd, bs_mean + ci.lvl * bs_sd)

bs_mean <- mean(lm.coefs[2,])
bs_sd <- sd(lm.coefs[2,])
pivot_ci_intercept <- c(bs_mean - ci.lvl * bs_sd, bs_mean + ci.lvl * bs_sd)

cor_sample <- sapply(1:Nboot, function(i) cor((bs_data[[1]])[i,],(bs_data[[2]])[i,]))
hist(cor_sample) #similar to normal
bs_mean <- mean(cor_sample)
bs_sd <- sd(cor_sample)
pivot_ci_cor <- c(bs_mean - ci.lvl * bs_sd, bs_mean + ci.lvl * bs_sd)

list(Slope_CI = pivot_ci_slope, Intersept_CI = pivot_ci_intercept, Correlation_CI = pivot_ci_cor)

xyplot(data.temp[,1] ~ data.temp[,2],
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(lm(y ~ x), ol = "red", lwd = 2)
         panel.abline(pivot_ci_slope[1], pivot_ci_intercept[1], col = "red", lwd = 2)
         panel.abline(pivot_ci_slope[2], pivot_ci_intercept[2], col = "red", lwd = 2)
       }, cex = 1.5,
       xlab = "Rate positive words", 
       ylab = "Global sentiment polarity")

##########################################################################
### 5. Estimation of average relative error                            ### 
##########################################################################
mean((data.temp[,1] - model$fitted.values)/(data.temp[,1])*100)

mean( abs((data.temp[,1] - model$fitted.values)/data.temp[,1]) *100)
R.sq

##########################################################################
### 6. Natural inspired optimization approach                          ###
##########################################################################
#ddr: determines the boundaries of a and b in y = a*x+b
ddr <- function(x,y){
  ll <- length(x)
  eps <- 0.05
  a <- ldply(1:(ll-1), function(i){
    v <- sapply((i+1):ll, function(j) 
      ifelse(abs(x[i] - x[j]) > eps, (y[i] - y[j])/(x[i] - x[j]), NA))
    c(min(v, na.rm = TRUE), max(v, na.rm = TRUE))
  })
  b <- ldply(1:(ll-1), function(i){
    v <- sapply((i+1):ll, function(j) 
      ifelse(abs(x[i] - x[j]) > eps, (y[j]*x[i] - y[i]*x[j])/(x[i] - x[j]), NA))
    c(min(v, na.rm = TRUE), max(v, na.rm = TRUE))
  })
  
  list(a.boundaries = c(min(a[,1]), max(a[,2])), 
       b.boundaries = c(min(b[,1]), max(b[,2])))
}
#computing the quality of the approximation
delta <- function(coef, x,y){
  a = coef[1]; b = coef[2]
  yp <- a*x + b
  #esq <- sum((y - yp)^2)/length(x)
  esq <- mean( abs((y - yp)/y) ) 
} 

nlr <- function(x,y){
  ll <- length(x)
  bounds <- ddr(x,y) #intervals for coefficients
  lb <- c(bounds[[1]][1],bounds[[2]][1])
  rb <- c(bounds[[1]][2],bounds[[2]][2])
  p = 15 #population size
  feas <- matrix(c(rep(rb[1] - lb[1],p), rep(rb[2] - lb[2],p)), ncol = 2) * cbind(rnorm(p),rnorm(p)) +
    matrix(c(rep(lb[1],p), rep(lb[2],p)), ncol = 2)
  flag = 1; count = 0; iter = 5000; funp = 0
  vv <- apply(feas, 1, function(d) delta(d, x, y))  
  funi <- min(vv); ini <- which.min(vv); soli <- feas[ini,]
  si = 1
  while (flag == 1){
    count <- count + 1
    feas <- feas + cbind(rnorm(p),rnorm(p))
    feas <- t(apply(feas, 1, function(x) ifelse(x > lb, ifelse(x < rb, x, rb), lb)))
    vec <- apply(feas, 1, function(d) delta(d, x, y)) 
    fun <- min(vec); un <- which.min(vec); sol <- feas[un,]
    wf <- max(vec); wi <- which.max(vec); wun <-feas[wi,]
    if (wf > funi){
      feas[wi,] <- soli 
      vec[wi] <- funi
    } #changing the worst for the best of the previous generation
    if (fun < funi) {
      soli <- sol; funi <- fun
    }
    if (count >= iter) { flag = 0 }
    residvar <- funi/var(y)
    if (count %% 500 == 0){
      print(soli)
      print(residvar)
    }
  }
  a <- soli[1]; b <- soli[2]
  c(a,b)
}

model.nlr <- nlr(data.temp[,2], data.temp[,1])
mean( abs((data.temp[,1] - model.nlr[1]*data.temp[,2] - model.nlr[2])/data.temp[,1]) )*100

optim(par = c(model$coefficients[2],model$coefficients[1]),
      function(v) mean( abs((data.temp[,1] - data.temp[,2]*v[1] -v[2])/data.temp[,1]) * 100 ))
