library(lattice)
library(e1071)
set.seed(123)
online.news.popularity <- read.csv(file="new_sample_OnlineNewsPopularity.csv",head=TRUE,sep=",")

normalize.range <- function(x) {
  midrange <- (max(x) + min(x))/2
  halfrange <- (max(x) - min(x))/2
  y <- (x - midrange)/halfrange
  y
}

#########################################################
### 1.Selection 3 features related to the same aspect ###
#########################################################
# From previous hometask
n <- ncol(online.news.popularity)
data <- online.news.popularity[,-c(1,2,n)]
data <- subset(data, n_tokens_content > 0)
data <- apply(data, MARGIN = 2,  function(x) {
  y <- (x - mean(x))/sd(x)
  y
})

data <- data.frame(data)
label <- ifelse(data[,25] < 0 & data[,26] < 0 & data[,27] > 0 & data[,28] < 0 & data[,29] > 0,1,
                ifelse(data[,25] > 0 & data[,26] > 0 & data[,27] < 0 & data[,28] > 0 & data[,29] < 0, 2, 3))
bad.index <- which(label == 3)


features <- c(7,25,33)
colnames(data)[features]
n <- ncol(online.news.popularity)
data <- online.news.popularity[,-c(1,2,n)]
data <- subset(data, n_tokens_content > 0)

data <- data[,features]
data <- transform(data, shares = log(shares))
#########################################################
### 2.Visualization (PCA)                             ###
#########################################################

data <- data[-bad.index,]
label <- label[-bad.index]
data.nrm.range <- as.data.frame(apply(data, MARGIN = 2, normalize.range))
data.nrm.std <- scale(data, center = TRUE, scale = TRUE)

#First normalization
pca.std <- prcomp(data.nrm.std, center = FALSE, scale. = FALSE)
biplot(pca.std)
plot(pca.std$x[,1], pca.std$x[,2], col = label, xlab = 'PCA 1', ylab = 'PCA 2', main = 'PCA: Z-score')

#Second normalization
pca.range <- prcomp(data.nrm.range, center = FALSE, scale. = FALSE)
biplot(pca.range)
plot(pca.range$x[,1], pca.range$x[,2], col = label, xlab = 'PCA 1', ylab = 'PCA 2', main = 'PCA: Range stardardization')

svd.range <- svd(data.nrm.range)
svd.range$v
svd.std <- svd(data.nrm.std)
svd.std$v

# library(ggplot2)
# pca.std <- as.data.frame(pca.std$x)
# pca.range <- as.data.frame(pca.range$x)
# 
# ggplot(data = pca.std, aes(x = PC1, y = PC2, label = rep('*',length(label))))+
#   geom_hline(yintercept = 0, colour = "gray65") +
#   geom_vline(xintercept = 0, colour = "gray65") +
#   geom_text(colour = label, alpha = 0.8, size = 4) +
#   ggtitle("PCA plot: Z-scoring")
# 
# ggplot(data = pca.range, aes(x = PC1, y = PC2, label = rep('*',length(label))))+
#   geom_hline(yintercept = 0, colour = "gray65") +
#   geom_vline(xintercept = 0, colour = "gray65") +
#   geom_text(colour = label, alpha = 0.8, size = 4) +
#   ggtitle("PCA plot: Range standardization")

#########################################################
### 3. Hidden factor                                  ###
#########################################################

hf.std <- svd(data.nrm.std)$v[,1]
plot(svd(data.nrm.std)$d)
hf.range <- svd(data.nrm.range)$v[,1]
plot(svd(data.nrm.range)$d)
# Singular values show that the first vector is much more important than the others.


hf.std <- abs(hf.std)
hf.range <- abs(hf.range)
hf.std <- hf.std / sum(hf.std) * 100
hf.range <- hf.range / sum(hf.range) * 100
hf <- data.frame(FeatureAbsoluteContribution = t(cbind(t(hf.std), t(hf.range))),
                 StdType = c(rep("Std", 3), rep("Range", 3)),
                 Feature = c("sentiment_polarity", "rate_positive_words", "rate_negative_words"))
hf
xyplot(FeatureAbsoluteContribution ~ Feature | StdType, data = hf, pch = 15)
