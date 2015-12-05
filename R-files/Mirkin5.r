library(lattice)
library(e1071)
set.seed(123)
online.news.popularity <- read.csv(file="new_sample_OnlineNewsPopularity.csv",head=TRUE,sep=",")

normalize <- function(x) {
  y <- (x - min(x))/(max(x) - min(x))
  y
}

###################################################
### 1. Build multiple linear regression         ###
###################################################
n <- ncol(online.news.popularity)
data <- online.news.popularity[,-c(1,2,n)]
data <- transform(data, num_hrefs = log(num_hrefs), num_self_hrefs = log(num_self_hrefs),
                  self_reference_avg_sharess = log(self_reference_avg_sharess),
                  shares = log(shares), n_tokens_content = log(n_tokens_content))
data[data == -Inf] = -4000
data.nrm <- as.data.frame(apply(data, MARGIN = 2, normalize))



min.model <- lm(num_hrefs~1, data=data)
max.model <- lm(num_hrefs~., data=data)
bidir.model <- step(min.model,  scope=list(upper=max.model), data = data, direction = "both", trace = 0)

min.model.nrm <- lm(num_hrefs~1, data=data.nrm)
max.model.nrm <- lm(num_hrefs~., data=data.nrm)
bidir.model.nrm <- step(min.model.nrm,  scope=list(upper=max.model.nrm), data = data.nrm, direction = "both", trace = 0)

summary(bidir.model)
summary(bidir.model.nrm)

# fwd.model <- step(min.model,  scope=list(lower=min.model, upper=max.model), direction = "forward", trace = 0)
# bcwd.model <- step(max.model,  data = data, direction = "backward", trace = 0)
# summary(fwd.model)
# summary(bcwd.model)

##############################################
### 2. Determinacy coefficient, its CI     ###
##############################################

alpha = 0.95
mr <- summary(bidir.model)

get.R2.distribution <- function(data, num_iterations, bs_sample_size) {
  sapply(1:num_iterations, function(n) {
    indices <- sample(1:(nrow(data)), bs_sample_size, replace = TRUE)
    bs.data <- data[indices,]
    mr <- lm(num_hrefs ~ n_tokens_content + num_self_hrefs + n_unique_tokens + 
         global_rate_positive_words + timedelta + avg_positive_polarity + 
         average_token_length + data_channel_is_entertainment, data = bs.data)
    summary(mr)$adj.r.squared
  })
}

r2 <- get.R2.distribution(data, 1000,1000)
plot(density(r2))

CI_R2 <- c(quantile(r2, probs = (1 - alpha)/2), 
           quantile(r2, probs = (1 + alpha)/2))

CI_R2
mr$adj.r.squared

#############################################
### 3. Linear, Fisher discriminant rule   ###
#############################################

n <- ncol(online.news.popularity)
data <- online.news.popularity[,-c(1,2,n)]
data <- subset(data, n_tokens_content > 0)

data <- apply(data, MARGIN = 2,  function(x) {
  y <- (x - mean(x))/sd(x)
  y
})

data <- data.frame(data)

# colnames(data)[25:29]
# # 1  -  strongly negative articles
# # 2  -  strongly positive articles
# # 3  -  neutral articles
label <- ifelse(data[,25] < 0 & data[,26] < 0 & data[,27] > 0 & data[,28] < 0 & data[,29] > 0,1,
        ifelse(data[,25] > 0 & data[,26] > 0 & data[,27] < 0 & data[,28] > 0 & data[,29] < 0, 2, 3))
bad.index <- which(label == 3)

# plot(X.proj[-bad.index,1], X.proj[-bad.index,2], col = label[-bad.index])

data <- data[-bad.index,]
label <- label[-bad.index]

library(MASS)
library(corrplot)

lin_da <- lda(data, label)
corrplot.mixed(cor(data))
data <- data[,-c(24,28)]
corrplot.mixed(cor(data))


data$label <- as.factor(label)

library(caret)

getCVErrorLDA <- function(data, folds) {
  k = length(folds)
  cv.errors <- vector('numeric', k)
  for (i in 1:k) {
    test <- data[folds[[i]],]
    learning <- data[-folds[[i]],]
    fit <- lda(label~., learning)
    cv.errors[i] <- sum(predict(fit, test)$class != test$label) / length(folds[[i]])
  }
  cv.errors
}

getFisherVector <- function(data) {
  data$label <- as.numeric(data$label)
  v <- -apply(aggregate(data, by = list(data$label), mean), 2, diff)[-c(1,33)]
  sample1 <- subset(data, label == 1)[,-32]
  sample2 <- subset(data, label == 2)[,-32]
  W <- ( nrow(sample1) * cov(sample1) + nrow(sample2) * cov(sample2) ) / (nrow(data) - 2)
  vect <- solve(W) %*% t(t(v))
  list(vect = vect, score = mean(c(sum(vect * colMeans(sample1)), sum(vect * sum(vect * colMeans(sample2))))))
}

predictFisherDA <- function(fisher_vector, element) {
  ifelse(t(t(element)) %*% t(t(fisher_vector$vect)) > fisher_vector$score, 1, 2)
}

getCVErrorFisher <- function(data, folds) {
  k = length(folds)
  cv.errors <- vector('numeric', k)
  for (i in 1:k) {
    test <- data[folds[[i]],]
    learning <- data[-folds[[i]],]
    fit <- getFisherVector(learning)
    cv.errors[i] <- sum(predictFisherDA(fit, test[,-32]) != test$label) / length(folds[[i]])
  }
  cv.errors
}


folds <- createFolds(y = 1:nrow(data), k = 2)

getCVErrorLDA(data,folds)
getCVErrorFisher(data,folds)


folds <- createFolds(y = 1:nrow(data), k = 10)

mean(getCVErrorLDA(data,folds))
mean(getCVErrorFisher(data,folds))


######################
### 0. PCA plots   ###
######################



n <- ncol(online.news.popularity)
data <- online.news.popularity[,-c(1,2,n)]
data <- subset(data, n_tokens_content > 0)

data <- apply(data, MARGIN = 2,  function(x) {
  y <- (x - mean(x))/sd(x)
  y
})

data <- as.matrix(data)

s <- svd(data)
u <- s$u[,1:2]
v <- s$v[,1:2]
X.proj <- data %*% v
plot(s$d)

data <- data.frame(data)

colnames(data)[25:29]
# # 1  -  strongly negative articles
# # 2  -  strongly positive articles
# # 3  -  neutral articles
label <- ifelse(data[,25] < 0 & data[,26] < 0 & data[,27] > 0 & data[,28] < 0 & data[,29] > 0,1,
                ifelse(data[,25] > 0 & data[,26] > 0 & data[,27] < 0 & data[,28] > 0 & data[,29] < 0, 2, 3))
bad.index <- which(label == 3)

plot(X.proj[,1], X.proj[,2], col = label)
plot(X.proj[-bad.index,1], X.proj[-bad.index,2], col = label[-bad.index])



