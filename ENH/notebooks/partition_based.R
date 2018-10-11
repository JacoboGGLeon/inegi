Sys.setenv('R_MAX_VSIZE'= 40000000000)

library('fpc')
library('dbscan')
library('factoextra')
library('ggplot2')
library(clustertend)
library(clValid)

df <- read.csv('enh_grouped.csv')
head(df)s
data <- df[, 5:58]
data <- scale(x = data)
#data$id <- df[, 1] + 1

# assesing cluster tendency

library(clustertend)

# Hopkins statistic
set.seed(123)
hopkins <- clustertend::hopkins(data = data, n = nrow(data)-1)
hopkins

# dist
dist_euc <- stats::dist(x = data,method = "euclidean")

cor <- stats::cor(x = data)

# VAT
vat <- factoextra::fviz_dist(dist.obj = dist, show_labels = F)
vat + labs(title='data')

install.packages('corrplot')
library(corrplot)
library(RColorBrewer)

vat_cor <- corrplot::corrplot(corr = cor,  
                              method = 'color', 
                              type = 'upper',
                              order = 'hclust', 
                              tl.col = 'black', 
                              tl.srt = 45, 
                              col = brewer.pal(n = 8,name = 'RdBu'))
vat_cor

elbow_kmeans <- factoextra::fviz_nbclust(x = data, 
                                        FUNcluster = kmeans, 
                                        method = 'wss', 
                                        k.max = 15,
                                        print.summary = T)

elbow_kmeans +  
  ggplot2::geom_vline(xintercept = c(2, 4, 6), linetype=2, color='red') +
  ggplot2::theme_minimal() + 
  ggplot2::theme(legend.position = 'bottom')


silhouette_kmeans <- factoextra::fviz_nbclust(x = data, 
                                        FUNcluster = kmeans, 
                                        method = 'silhouette', 
                                        k.max = 15,
                                        print.summary = T)

silhouette_kmeans +
  ggplot2::geom_vline(xintercept = 2, linetype=2, color='red') +
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = 'bottom')

gap_kmeans <- factoextra::fviz_nbclust(x = data, 
                                              FUNcluster = kmeans, 
                                              method = 'gap_stat', 
                                              k.max = 15, nstart=25,
                                              nboot = 50,
                                              print.summary = T)

gap_kmeans

library(NbClust)
library(factoextra)

dunn_km <- c()
range_km <- c(2:15)
i <- 1

for (h in range_km) {
  set.seed(123)
  km <- stats::kmeans(x = data, centers = h, nstart = 25)
  
  index_internal_km <- clusterCrit::intCriteria(traj = data,
                                                part = as.integer(km$cluster), 
                                                crit = "dunn")
  dunn_km[i] <- index_internal_km$dunn
  
  i <- i + 1
}

# plot_dunn_km <- ggplot2::qplot(x = c(2:15), 
#                                     y = dunn_km, 
#                                     geom = 'point', 
#                                     main = 'Compactness index: km', 
#                                     xlab = 'clusters', ylab = 'compactness')
# 
# plot_compact_km  + geom_vline(xintercept=range_km[which.max(dunn_km)], 
#                               linetype = 2, color='red')

# plot_clusters_km <- ggplot2::qplot(x = c(2:15), 
#                                      y = clusters_km, 
#                                      geom = 'point', 
#                                      main = 'Clusters index: km', 
#                                      xlab = 'eps', ylab = 'clusters')
# 
# plot_clusters_db_4  + geom_vline(xintercept = 100, linetype = 2, color='red')

plot_dunn_km <- ggplot2::qplot(x = range_km, 
                                 y = dunn_km, 
                                 geom = 'line', 
                                 # main = 'Dunn index: km', 
                                 xlab = 'clusters', ylab = 'distances')

plot_dunn_km + 
  theme_bw() +
  geom_vline(xintercept = range_km[which.max(dunn_km)], linetype = 2, color='red')

km <- stats::kmeans(x = data, centers = range_km[which.max(dunn_km)], nstart = 25)

plot_km <- factoextra::fviz_cluster(object = km, 
                                        # shape = nrow(data),
                                        data = data,
                                        # stand = F, 
                                        geom =  'point', 
                                        ellipse.type = 'convex',
                                        ellipse = T,
                                        # main = 'cluster:kmeans k:7',
                                        outlier.color = 'black',
                                        show.clust.cent = T)

plot_km + 
  theme_bw() +
  theme(legend.position = 'bottom')

# compactness <- diceR::compactness(data = data, labels = km$cluster)
# stats_km <- fpc::cluster.stats(d = dist, clustering = km$cluster)

library(clusterCrit)
index_internal_km <- clusterCrit::intCriteria(traj = data,
                                              part = as.integer(km$cluster), 
                                              crit = "all")
# index_internal_km$calinski_harabasz
# index_internal_km$silhouette
