df <- read.csv('enh_grouped.csv')
head(df)s
data <- df[, 5:58]
data <- scale(x = data)

dunn_clara <- c()
range_clara <- c(2:15)
i <- 1

for (h in range_clara) {
  set.seed(123)
  clara <- cluster::clara(x = data, k = h)
  
  internal <- fpc::cluster.stats(d = dist, clustering = clara$cluster)
  # internal_db$cluster.size
  dunn_clara[i] <- internal$dunn
  
  # clusters_clara[i] <- length(unique(clara$cluster))
  
  # install.packages('diceR', dependencies = T)
  # library(diceR)
  # compactness <- diceR::compactness(data = data, labels = clara$cluster)
  # compactness_clara[i] <- compactness
  
  i <- i + 1
}

plot_dunn_clara <- ggplot2::qplot(x = range_clara, 
                               y = dunn_clara, 
                               geom = 'line', 
                               main = 'Dunn index: clara', 
                               xlab = 'Clusters', ylab = 'Distance')

plot_dunn_clara + 
  theme_bw() +
  geom_vline(xintercept = range_clara[which.max(dunn_clara)], 
             linetype = 2, color='red')

clara <- cluster::clara(x = data, k = range_clara[which.max(dunn_clara)])

plot_clara <- factoextra::fviz_cluster(object = clara, 
                                    # shape = nrow(data),
                                    data = data,
                                    # stand = F, 
                                    geom =  'point', 
                                    ellipse.type = 'convex',
                                    ellipse = T,
                                    # main = 'cluster:claraeans k:7',
                                    outlier.color = 'black',
                                    show.clust.cent = T)

plot_clara + 
  theme_bw() +
  theme(legend.position = 'bottom')

library(clusterCrit)
index_internal_clara <- clusterCrit::intCriteria(traj = data,
                                              part = as.integer(clara$cluster), 
                                              crit = "all")
index_internal_clara$calinski_harabasz
index_internal_clara$silhouette


