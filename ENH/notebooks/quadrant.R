quadrant_names <- c("agg_euc",
                    "agg_man",
                    "k-means", 
                    "dbscan", 
                    "dia_man",
                    "dia_euc")

quadrant_x <- c(index_internal_agg_euc$calinski_harabasz,
                index_internal_agg_man$calinski_harabasz,
                index_internal_km$calinski_harabasz,
                index_internal_db$calinski_harabasz,
                index_internal_dia_man$calinski_harabasz,
                index_internal_dia_euc$calinski_harabasz)

plot_quadrant <- ggplot2::qplot(x = quadrant_names, 
                                  y = quadrant_x, 
                                  geom = 'point', 
                                  # main = 'Dunn index: clara', 
                                  xlab = 'C-Index', 
                                ylab = 'Davies-Bouldin')

plot_quadrant + 
  theme_bw() 



  geom_vline(xintercept = quadrant_names[which.max(quadrant_x)], linetype = 2, color='red')



  theme(legend.text = quadrant_names)
