library(lattice)
library(e1071)
library(class)
library(cluster)
library(igraph)

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


ind <- sample(1:nrow(data.nrm), 500)
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
features_to_cluster <- c(channel_feature, num_of_words_feautures, 34)


colnames(data.nrm)[features_to_cluster]
data <- data.nrm[ind, features_to_cluster]

xyplot(pca.std$x[,1] ~ pca.std$x[,2], xlab = 'PCA 1', ylab = 'PCA 2',
       main = 'PCA plot')

k_mean_3 <- kmeans(data, 3, nstart = 100)
pca.std <- prcomp(data, center = FALSE, scale. = FALSE)
xyplot(pca.std$x[,1] ~ pca.std$x[,2], col = k_mean_3$cluster, xlab = 'PCA 1', ylab = 'PCA 2',
       main = 'PCA - K-Means. K = 3')

k_mean_4 <- kmeans(data, 4, nstart = 100)
pca.std <- prcomp(data, center = FALSE, scale. = FALSE)
xyplot(pca.std$x[,1] ~ pca.std$x[,2], col = k_mean_4$cluster, xlab = 'PCA 1', ylab = 'PCA 2',
       main = 'PCA - K-Means. K = 4')


k_mean_7 <- kmeans(data, 7, nstart = 100)
pca.std <- prcomp(data, center = FALSE, scale. = FALSE)
xyplot(pca.std$x[,1] ~ pca.std$x[,2], col = k_mean_7$cluster, xlab = 'PCA 1', ylab = 'PCA 2',
     main = 'PCA - K-Means. K = 7')


get_errors_for_list_of_k <- function(data, max_iter, k_range) {
  return (sapply(k_range,
                 function(k)
                    return(kmeans(data, k, nstart = max_iter)$tot.withinss)
                 )
          )
}

xyplot(get_errors_for_list_of_k(data, 50, 1:20) ~ 1:20,
     main="Least squares errors",
     xlab="different k",
     ylab="within-cluster sum of squares", pch = 16)

s <- silhouette(x = list(data = data, clustering = as.vector(k_mean_7$cluster)),
                dist = dist(data))


##########################
### 2. NN MST          ###
##########################
data.pca.proj <-  data %*% pca.std$rotation[, 1:2]


lo <- layout.norm(as.matrix(data.pca.proj))
weighted_adjacency_matrix <- as.matrix(dist(data))
graph <-  as.undirected(graph_from_adjacency_matrix(weighted_adjacency_matrix,  weighted = T))
mst_res <- mst(graph, weights = graph$weighted, algorithm = 'prim')
#plot.igraph(mst_res, edge.label=round(E(mst_res)$weight, 1),
#            vertex.label=NA, vertex.size=5,  rescale=T)

pdf ('../images/mst_whole', width=100 , height=80)
plot.igraph(mst_res, vertex.label=NA, vertex.size=5, layout=lo)
dev.off()

mst_edges <- E(mst_res)$weight
k <- 7
heaviest_edges_index <- which(mst_edges >= sort(mst_edges, decreasing=T)[k-1])
mst_res <- delete_edges(mst_res, E(mst_res)[heaviest_edges_index])


V(graph)$color <- rainbow(ncomp)[components(mst_res)$membership]
pdf ('../images/mst_whole_removed_most_heaviest', width=100 , height=80)
plot.igraph(mst_res, vertex.label=NA, vertex.size=5, layout=lo, vertex.color=components(mst_res)$membership)
dev.off()


pdf ('../images/mst_clusters_splitted', width=100 , height=80)
plot (mst_res,  vertex.size=6, vertex.label=NA, layout=lo,
      mark.groups = split(1:vcount(graph), components(mst_res)$membership))
dev.off()



ncomp <- components(mst_res)$no
comps <- list()
for (i in 1:ncomp){
  comps[[i]] <- as.vector(which(components(mst_res)$membership == i))
}
comps
s <- silhouette(x = list(data = data.pca.proj,
                         clustering = as.vector(components(mst_res)$membership)),
                dist = dist(data.pca.proj))
summary(s)

