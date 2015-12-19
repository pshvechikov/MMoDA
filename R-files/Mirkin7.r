library(lattice)
library(e1071)
library(class)
set.seed(123)
online.news.popularity <- read.csv(file="new_sample_OnlineNewsPopularity.csv",head=TRUE,sep=",")

online.news.popularity$channel <- online.news.popularity$data_channel_is_lifestyle +
                                  2*online.news.popularity$data_channel_is_entertainment +
                                  3*online.news.popularity$data_channel_is_bus +
                                  4*online.news.popularity$data_channel_is_socmed +
                                  5*online.news.popularity$data_channel_is_tech +
                                  6*online.news.popularity$data_channel_is_world

online.news.popularity <- online.news.popularity[,-c(1,2,36)]


# online.news.popularity <- subset(online.news.popularity, n_tokens_content > 0)

data <- transform(online.news.popularity, 
                  num_hrefs = log(num_hrefs), 
                  num_self_hrefs = log(num_self_hrefs),
                  self_reference_avg_sharess = log(self_reference_avg_sharess),
                  shares = log(shares), 
                  n_tokens_content = log(n_tokens_content),
                  num_imgs = log(num_imgs),
                  num_videos = log(num_videos))
data[data == -Inf] = -2
data.nrm <- scale(data)



#########################################################
### 1. K Means ###
#########################################################

ind <- sample(1:nrow(data.nrm), 1000)
polarity_features <- c(25:32)
num_of_words_feautures <- c(2:4, 9:10)
channel_feature <- c(11:16)
hrefs_and_images_features <- c(5:8)
images_video_features  <-  c(7:8)

features_to_cluster <- images_video_features
features_to_cluster <- hrefs_and_images_features
features_to_cluster <- channel_feature
features_to_cluster <- num_of_words_feautures
features_to_cluster <- polarity_features
features_to_cluster <- c(channel_feature, num_of_words_feautures)

colnames(data.nrm)[features_to_cluster]
data <- data.nrm[ind, features_to_cluster]

k_mean_3 <- kmeans(data, 3, nstart = 100)
pca.std <- prcomp(data, center = FALSE, scale. = FALSE)
plot(pca.std$x[,1], pca.std$x[,2], col = k_mean_3$cluster, xlab = 'PCA 1', ylab = 'PCA 2', main = 'PCA: Z-score')
     
k_mean_4 <- kmeans(data, 4, nstart = 100)
pca.std <- prcomp(data, center = FALSE, scale. = FALSE)
plot(pca.std$x[,1], pca.std$x[,2], col = k_mean_4$cluster, xlab = 'PCA 1', ylab = 'PCA 2', main = 'PCA: Z-score')

k_mean_7 <- kmeans(data, 7, nstart = 100)
pca.std <- prcomp(data, center = FALSE, scale. = FALSE)
plot(pca.std$x[,1], pca.std$x[,2], col = k_mean_7$cluster, xlab = 'PCA 1', ylab = 'PCA 2', main = 'PCA: Z-score')

get_errors_for_list_of_k <- function(data, max_iter, k_range) {
  return (sapply(k_range,
                 function(k)
                    return(kmeans(data, k, nstart = max_iter)$tot.withinss)
                 )
          )
}

plot(get_errors_for_list_of_k(data, 50, 1:20),
     main="Least squares errors",
     xlab="different k",
     ylab="within-cluster sum of squares")

s <- silhouette(x = list(data = data, clustering = as.vector(k_mean_7$cluster)),
                dist = dist(data))
plot(sortSilhouette(s))


##########################
### 2. NN MST          ###
##########################

library('igraph')
data <- online.news.popularity[c("n_tokens_title", "num_hrefs")]
features_to_cluster <- c("global_sentiment_polarity" , "global_rate_positive_words", 
                         "global_rate_negative_words", "rate_positive_words", 
                         "rate_negative_words", "avg_positive_polarity", 
                         "avg_negative_polarity", "title_sentiment_polarity" )
data <- online.news.popularity[features_to_cluster]
data.nrm_range <- data[sample(1:nrow(data), 500), ]
data.nrm_range <- as.data.frame(apply(data.nrm_range, MARGIN = 2, scale)) 
weighted_adjacency_matrix <- as.matrix(dist(data.nrm_range))
graph <- graph_from_adjacency_matrix(weighted_adjacency_matrix, weighted = T)
mst_res <- mst(graph, weights = graph$weighted, algorithm = 'prim')
plot(mst_res, edge.label=round(E(mst_res)$weight, 1))

mst_edges <- E(mst_res)$weight
k <- 3
heaviest_edges <- which(mst_edges >= sort(mst_edges, decreasing=T)[k-1])
mst_edges[heaviest_edges]

mst_res <- delete_edges(mst_res, E(mst_res)[heaviest_edges])
plot(mst_res, edge.label=round(E(mst_res)$weight, 1))

ncomp <- components(mst_res)$no
comps <- list()
for (i in 1:ncomp){
  comps[[i]] <- as.vector(which(components(mst_res)$membership == i))
}
comps

s <- silhouette(x = list(data = data.nrm_range, clustering = as.vector(components(mst_res)$membership)),
                dist = dist(data.nrm_range))


V(mst_res)$color <- rainbow(ncomp)[components(mst_res)$membership]
plot(mst_res, mark.groups = split(1:vcount(mst_res), components(mst_res)$membership))


pca.std <- prcomp(data.nrm_range, center = FALSE, scale. = FALSE)
plot(pca.std$x[,1], pca.std$x[,2], col = components(mst_res)$membership,
     xlab = 'PCA 1', ylab = 'PCA 2', main = 'PCA: Z-score', pch = 8)

