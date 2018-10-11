df <- read.csv('enh_grouped.csv')
head(df)s
data <- df[, 5:58]
data <- scale(x = data)

dunn_pam_euc <- c()
range_pam_euc <- c(3:10)
i <- 1

dist_euc <- stats::dist(x = data,method = "euclidean")

for (h in range_pam_euc) {
  set.seed(123)
  pam <- cluster::pam(x = data, k = h, metric = "euclidean")
  
  internal <- fpc::cluster.stats(d = dist_euc, clustering = pam$cluster)
  # internal_db$cluster.size
  dunn_pam_euc[i] <- internal$dunn
  
  i <- i + 1
}

plot_dunn_pam_euc <- ggplot2::qplot(x = range_pam_euc, 
                                  y = dunn_pam_euc, 
                                  geom = 'line', 
                                  main = 'Dunn index: pam', 
                                  xlab = 'clusters', ylab = 'distances')

plot_dunn_pam_euc + 
  theme_bw() +
  geom_vline(xintercept = range_pam_euc[which.max(dunn_pam_euc)], linetype = 2, color='red')

pam <- cluster::pam(x = data, k = range_pam_euc[which.max(dunn_pam_euc)], metric = "euclidean")

plot_pam_euc <- factoextra::fviz_cluster(object = pam, 
                                    # shape = nrow(data),
                                    data = data,
                                    # stand = F, 
                                    geom =  'point', 
                                    ellipse.type = 'convex',
                                    ellipse = T,
                                    # main = 'cluster:pameans k:7',
                                    outlier.color = 'black',
                                    show.clust.cent = T)

plot_pam_euc + 
  theme_bw() +
  theme(legend.position = 'bottom')

library(clusterCrit)
index_internal_pam_euc <- clusterCrit::intCriteria(traj = data,
                                                 part = as.integer(pam$cluster), 
                                                 crit = "all")
# index_internal_pam_euc$c_index
# index_internal_pam_euc$davies_bouldin