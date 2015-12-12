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

weigths.std <- svd(data.nrm.std)$v[,1]
plot(svd(data.nrm.std)$d)
weigths.range <- svd(data.nrm.range)$v[,1]
plot(svd(data.nrm.range)$d)
# Singular values show that the first vector is much more important than the others.

hf.std <- data.nrm.std %*% weigths.std
head(hf.std)
hf.range <- as.matrix(data.nrm.range) %*% weigths.range
head(hf.range)

hf.std <- hf.std - min(hf.std)
hf.range <- hf.range - min(hf.range)
hf.std <- hf.std / max(hf.std) * 100
hf.range <- hf.range / max(hf.range) * 100

#Hidden factor for raw data
weigths.raw <- svd(data)$v[,1]
plot(svd(data)$d)
hf.raw <- as.matrix(data) %*% weigths.raw
head(hf.raw)

hf.raw <- hf.raw - min(hf.raw)
hf.raw <- hf.raw / max(hf.raw) * 100
head(hf.raw)
c(min(hf.raw),max(hf.raw)) # from 0 to 100! Perfecto!
