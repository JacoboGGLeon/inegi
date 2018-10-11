Sys.setenv('R_MAX_VSIZE'= 40000000000)

# Libraries
library('fpc')
library('dbscan')
library('ggplot2')
library('factoextra')
library('NbClust')


# Load
df <- read.csv('enh_grouped.csv')
head(df)s
data <- df[, 5:58]

dist <- stats::dist(x = data, method = 'euclidean')

