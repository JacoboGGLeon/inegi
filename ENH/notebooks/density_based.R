Sys.setenv('R_MAX_VSIZE'= 40000000000)

# Libraries
library('fpc')
library('dbscan')
library('ggplot2')
library('factoextra')
library('NbClust')


# install.packages('diceR', dependencies = T)
# library(diceR)


# Load
df <- read.csv('enh_grouped.csv')
head(df)
data <- df[, 5:58]
data <- scale(x = data)

dist <- stats::dist(x = data, method = 'euclidean')

k = 4
knndist_db <- dbscan::kNNdistplot(x = data, k = k)

range_db <- c(7:9)

abline(h=range_db, lty=2, col='red')

dunn_db <- c()
i <- 1

for (h in range_db) {
  set.seed(123)
  db <- fpc::dbscan(data = data, eps = h, MinPts = k)
  
  index_internal_db <- clusterCrit::intCriteria(traj = data,
                                               part = as.integer(db$cluster), 
                                               crit = "dunn")
  dunn_db[i] <- index_internal_db$dunn
  
  i <- i + 1
}

plot_dunn_db <- ggplot2::qplot(x = range_db, 
                                   y = dunn_db, 
                                   geom = 'point', 
                                   main = 'Compactness index: db minpts:3', 
                                   xlab = 'eps', ylab = 'clusters')

plot_dunn_db  + 
  geom_vline(xintercept = range_db[which.max(dunn_db)],
             linetype = 2, color='red')

db <- fpc::dbscan(data = data, eps = range_db[which.max(dunn_db)], MinPts = k)

plot_dbscan <- factoextra::fviz_cluster(object = db,
                                              # shape = round(nrow(data)*.10),
                                              data = data,
                                              # stand = T, 
                                              geom =  'point',
                                              ellipse.type = 'convex',
                                              ellipse = T,
                                              # main = 'cluster:dbscan k:4 eps:8',
                                              outlier.color = 'black',
                                              show.clust.cent = T)

plot_dbscan + 
  theme_bw() +
  theme(legend.position = 'bottom')
  

# compactness <- diceR::compactness(data = data, labels = db$cluster)
# stats_db <- fpc::cluster.stats(d = dist, 
#                                clustering = db$cluster, 
#                                noisecluster = T,
#                                silhouette = T, 
#                                G2 = T, G3 = T, 
#                                wgap = T, 
#                                sepindex = T, 
#                                sepprob = T, 
#                                sepwithnoise = T)

library(clusterCrit)
index_internal_db <- clusterCrit::intCriteria(traj = data,
                                              part = as.integer(db$cluster), 
                                              crit = "all")

