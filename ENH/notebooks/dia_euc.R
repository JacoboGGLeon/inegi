dunn_dia_euc <- c()
range_dia_euc <- c(2:15)
i <- 1

dia_euc <- cluster::diana(x = data, metric = "euclidean")

for (h in range_dia_euc) {
  set.seed(123)
  
  dia_euc_group <- stats::cutree(tree = dia_euc, k = h) 
  
  index_internal_dia_euc <- clusterCrit::intCriteria(traj = data,
                                                     part = as.integer(dia_euc_group), 
                                                     crit = "dunn")
  dunn_dia_euc[i] <- index_internal_dia_euc$dunn
  
  i <- i + 1
}

plot_dunn_dia_euc <- ggplot2::qplot(x = range_dia_euc, 
                                    y = dunn_dia_euc, 
                                    geom = 'line', 
                                    main = 'Dunn index: agg euc', 
                                    xlab = 'clusters', ylab = 'distances')

plot_dunn_dia_euc + 
  theme_bw() +
  geom_vline(xintercept = range_dia_euc[which.max(dunn_dia_euc)], linetype = 2, color='red')

dia_euc_group <- stats::cutree(tree = dia_euc, k = range_dia_euc[which.max(dunn_dia_euc)]) 

table(dia_euc_group)

library(clusterCrit)
index_internal_dia_euc <- clusterCrit::intCriteria(traj = data,
                                                   part = as.integer(dia_euc_group), 
                                                   crit = "all")
