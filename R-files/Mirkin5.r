library(lattice)
library(car)
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


