library(lattice)
set.seed(123)
online.news.popularity <- read.csv(file="new_sample_OnlineNewsPopularity.csv",head=TRUE,sep=",")

###################################################
### 1. Build multiple linear regression         ###
###################################################

data <- online.news.popularity[sample(1:1e4, 1000),-(1:2)]
min.model <- lm(shares~1, data=data)
max.model <- lm(shares~., data=data)
fwd.model <- step(min.model,  scope=list(lower=min.model, upper=max.model), direction = "forward")
bcwd.model <- step(max.model,  data = data, direction = "backward")
bidir.model <- step(min.model,  scope=list(upper=max.model), data = data, direction = "both")
summary(fwd.model)
summary(bcwd.model)
summary(bidir.model)

##############################################
### 2. Determinacy coefficient, its CI     ###
##############################################



#############################################
### 3. Linear, Fisher discriminant rule   ###
#############################################


