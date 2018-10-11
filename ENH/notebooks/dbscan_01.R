install.packages(c('fpc', 'dbscan', 'factoextra'), dependencies = TRUE)
install.packages('clustertend', dependencies = T)

library('fpc')
library('dbscan')
library('factoextra')
library('ggplot2')
library(clustertend)

df <- read.csv('enh_norm.csv')
head(df)

data <- df[, 0:58]

library(clustertend)
# Hopkins statistic
set.seed(123)
hopkins <- clustertend::hopkins(data = data, n = nrow(data)-1)
hopkins




k = 2*58

dbscan::kNNdistplot(x = data, k = k)
abline(h=58, lty=2)

set.seed(123)
db <- fpc::dbscan(data = data, eps = 58, MinPts = 116)

factoextra::fviz_cluster(object = db, data = data, stand = F, geom = 'point', ellipse = T,
                         show.clust.cent = T) +
  theme_bw() + theme(legend.position = 'bottom')