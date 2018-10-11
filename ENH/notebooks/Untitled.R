Sys.setenv('R_MAX_VSIZE'= 40000000000)

library('fpc')
library('dbscan')
library('factoextra')
library('ggplot2')
library(clustertend)

df <- read.csv('enh_grouped.csv')
head(df)s
data <- df[, 5:58]
#data$id <- df[, 1] + 1

# assesing cluster tendency

library(clustertend)

# Hopkins statistic
set.seed(123)
hopkins <- clustertend::hopkins(data = data, n = nrow(data)-1)
hopkins

# dist
dist <- stats::dist(x = data)

cor <- stats::cor(x = data)

# VAT
vat <- factoextra::fviz_dist(dist.obj = dist, show_labels = F) + labs(title='data')
vat

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
                                        k.max = 20,
                                        print.summary = T)

elbow_kmeans

silhouette_kmeans <- factoextra::fviz_nbclust(x = data, 
                                        FUNcluster = kmeans, 
                                        method = 'silhouette', 
                                        k.max = 20,
                                        print.summary = T)

silhouette_kmeans

gap_kmeans <- factoextra::fviz_nbclust(x = data, 
                                              FUNcluster = kmeans, 
                                              method = 'gap_stat', 
                                              k.max = 20, nstart=25,
                                              nboot = 500,
                                              print.summary = T)

gap_kmeans

library(NbClust)
library(factoextra)

#install.packages('NbClust', dependencies = T)
# index: kl
nb_kmeans_kl <- NbClust::NbClust(data = data, 
                              distance = NULL,
                              diss = dist,
                              min.nc = 2, 
                              max.nc = 15, 
                              method = "kmeans", 
                              index = "kl")

nbclust_kmeans_kl <- factoextra::fviz_nbclust(nb_kmeans_kl)
nbclust_kmeans_kl

# index: ch
nb_kmeans_ch <- NbClust::NbClust(data = data, 
                                 distance = NULL,
                                 diss = dist,
                                 min.nc = 2, 
                                 max.nc = 15, 
                                 method = "kmeans", 
                                 index = 'ch')

nbclust_kmeans_ch <- factoextra::fviz_nbclust(nb_kmeans_ch)
nbclust_kmeans_ch

# index: hartigan
nb_kmeans_hartigan <- NbClust::NbClust(data = data, 
                                 distance = NULL,
                                 diss = dist,
                                 min.nc = 2, 
                                 max.nc = 15, 
                                 method = "kmeans", 
                                 index = 'hartigan')

nbclust_kmeans_hartigan <- factoextra::fviz_nbclust(nb_kmeans_hartigan)
nbclust_kmeans_hartigan

# index: ccc
nb_kmeans_cindex <- NbClust::NbClust(data = data, 
                                       distance = NULL,
                                       diss = dist,
                                       min.nc = 2, 
                                       max.nc = 15, 
                                       method = "kmeans", 
                                       index = "cindex")

nbclust_kmeans_cindex <- factoextra::fviz_nbclust(nb_kmeans_cindex)
nbclust_kmeans_cindex

# index: ccc
nb_kmeans_db <- NbClust::NbClust(data = data, 
                                     distance = NULL,
                                     diss = dist,
                                     min.nc = 2, 
                                     max.nc = 15, 
                                     method = "kmeans", 
                                     index = "db")

nbclust_kmeans_db <- factoextra::fviz_nbclust(nb_kmeans_db)
nbclust_kmeans_db

# index: ccc
nb_kmeans_silhouette <- NbClust::NbClust(data = data, 
                                     distance = NULL,
                                     diss = dist,
                                     min.nc = 2, 
                                     max.nc = 15, 
                                     method = "kmeans", 
                                     index = "silhouette")

nbclust_kmeans_silhouette <- factoextra::fviz_nbclust(nb_kmeans_silhouette)
nbclust_kmeans_silhouette

# index: ccc
nb_kmeans_duda <- NbClust::NbClust(data = data, 
                                     distance = NULL,
                                     diss = dist,
                                     min.nc = 2, 
                                     max.nc = 15, 
                                     method = "kmeans", 
                                     index = "duda")

nbclust_kmeans_duda <- factoextra::fviz_nbclust(nb_kmeans_duda)
nbclust_kmeans_duda

# index: ccc
nb_kmeans_pseudot2 <- NbClust::NbClust(data = data, 
                                     distance = NULL,
                                     diss = dist,
                                     min.nc = 2, 
                                     max.nc = 15, 
                                     method = "kmeans", 
                                     index = "pseudot2")

nbclust_kmeans_pseudot2 <- factoextra::fviz_nbclust(nb_kmeans_pseudot2)
nbclust_kmeans_pseudot2

# index: ccc
nb_kmeans_beale <- NbClust::NbClust(data = data, 
                                       distance = NULL,
                                       diss = dist,
                                       min.nc = 2, 
                                       max.nc = 15, 
                                       method = "kmeans", 
                                       index = "beale")

nbclust_kmeans_beale <- factoextra::fviz_nbclust(nb_kmeans_beale)
nbclust_kmeans_beale

pseudot2


library(cluster)
test <- factoextra::fviz_nbclust(x = data, 
                         FUNcluster = kmeans,
                         k.max = 15,
                         method = "wss")

install.packages('clValid', dependencies = T)
library(clValid)
clvalid_kmeans <-clValid::clValid(obj = data_scaled, 
                 nClust = 2:15, 
                 clMethods = 'kmeans', 
                 maxitems = nrow(data_scaled), 
                 validation = 'internal')

