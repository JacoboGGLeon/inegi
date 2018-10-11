install.packages(c('fpc', 'dbscan', 'factoextra'), dependencies = TRUE)

library('fpc')
library('dbscan')
library('factoextra')
library('ggplot2')

df <- read.csv('enh_norm.csv')
head(df)

data <- df[, 0:58]

dbscan::kNNdistplot(x=df[, 0:58], k = 4)
abline(h=3, lty=2)

dbscan::kNNdistplot(x=df[, 0:58], k = 5)
abline(h=3.5, lty=2)

dbscan::kNNdistplot(x=df[, 0:58], k = 3)
abline(h=2.7, lty=2)

set.seed(123)
db <- fpc::dbscan(data = data, eps = 3, MinPts = 3)

factoextra::fviz_cluster(object = db, data = data, stand = F, geom = 'point', ellipse = T,
                         show.clust.cent = T) +
  theme_bw() + theme(legend.position = 'bottom')

#HD Video 16:9
1280*720

#minpts = 2*dim
dbscan::kNNdistplot(x = data, k = 116)
abline(h=58, lty=2)

set.seed(123)
db <- fpc::dbscan(data = data, eps = 58, MinPts = 116)
