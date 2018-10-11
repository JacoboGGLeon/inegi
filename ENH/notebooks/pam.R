df <- read.csv('enh_grouped.csv')
head(df)s
data <- df[, 5:58]
data <- scale(x = data)

dunn_pam_man <- c()
range_pam_man <- c(2:15)
i <- 1

dist_man <- stats::dist(x = data,method = "manhattan")

for (h in range_pam_man) {
  set.seed(123)
  pam <- cluster::pam(x = data, k = h, metric = "manhattan")
  
  internal <- fpc::cluster.stats(d = dist_man, clustering = pam$cluster)
  # internal_db$cluster.size
  dunn_pam_man[i] <- internal$dunn
  
  i <- i + 1
}

plot_dunn_pam_man <- ggplot2::qplot(x = range_pam_man, 
                                  y = dunn_pam_man, 
                                  geom = 'line', 
                                  main = 'Dunn index: pam', 
                                  xlab = 'clusters', ylab = 'distances')

plot_dunn_pam_man + 
  theme_bw() +
  geom_vline(xintercept = range_pam_man[which.max(dunn_pam_man)], linetype = 2, color='red')

pam <- cluster::pam(x = data, k = range_pam_man[which.max(dunn_pam_man)], metric = "manhattan")

plot_pam_man <- factoextra::fviz_cluster(object = pam, 
                                    # shape = nrow(data),
                                    data = data,
                                    # stand = F, 
                                    geom =  'point', 
                                    ellipse.type = 'convex',
                                    ellipse = T,
                                    # main = 'cluster:pameans k:7',
                                    outlier.color = 'black',
                                    show.clust.cent = T)

plot_pam_man + 
  theme_bw() +
  theme(legend.position = 'bottom')

library(clusterCrit)
index_internal_pam_man <- clusterCrit::intCriteria(traj = data,
                                                 part = as.integer(pam$cluster), 
                                                 crit = "all")
# index_internal_pam_man$c_index
# index_internal_pam_man$davies_bouldin