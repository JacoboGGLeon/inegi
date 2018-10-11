dunn_fanny_euc <- c()
range_fanny_euc <- c(2:15)
i <- 1

for (h in range_fanny_euc) {
  set.seed(123)
  fanny <- cluster::fanny(x = data, k = h, metric = "manhattan")
  
  index_internal_fanny_euc <- clusterCrit::intCriteria(traj = data,
                                                part = as.integer(fanny$cluster), 
                                                crit = "dunn")
  dunn_fanny_euc[i] <- index_internal_fanny_euc$dunn
  
  i <- i + 1
}

plot_dunn_fanny_euc <- ggplot2::qplot(x = range_fanny_euc, 
                               y = dunn_fanny_euc, 
                               geom = 'line', 
                               # main = 'Dunn index: fanny', 
                               xlab = 'clusters', ylab = 'distances')

plot_dunn_fanny_euc + 
  theme_bw() +
  geom_vline(xintercept = range_fanny_euc[which.max(dunn_fanny_euc)], 
             linetype = 2, color='red')

fanny_euc <- cluster::fanny(x = data, k=3)
                            # k = range_fanny_euc[which.max(dunn_fanny_euc)])

plot_fanny_euc <- factoextra::fviz_cluster(object = fanny_euc, 
                                    # shape = nrow(data),
                                    data = data,
                                    # stand = F, 
                                    geom =  'point', 
                                    ellipse.type = 'convex',
                                    ellipse = T,
                                    # main = 'cluster:fannyeans k:7',
                                    outlier.color = 'black',
                                    show.clust.cent = T)

plot_fanny_euc + 
  theme_bw() +
  theme(legend.position = 'bottom')

library(clusterCrit)
index_internal_fanny_euc <- clusterCrit::intCriteria(traj = data,
                                              part = as.integer(fanny$cluster), 
                                              crit = "all")
