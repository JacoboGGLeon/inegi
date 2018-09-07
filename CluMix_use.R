
install.packages("BiocManager", dependencies = TRUE)
source("https://bioconductor.org/biocLite.R")

BiocManager::install("marray")

BiocManager::install(version = "3.8")

source("https://bioconductor.org/biocLite.R")
biocLite("Biobase")

library(CluMix)
data(mixdata)
str(mixdata)

w <- rep(1:2, each=5)
mix.heatmap(mixdata, varweights=w, rowmar=7)
mix.heatmap(mixdata, dist.variables.method="distcor", rowmar=7)
mix.heatmap(mixdata, dist.variables.method="ClustOfVar", rowmar=7)

D.subjects <- dist.subjects(mixdata)
dend.variables <- dendro.variables(mixdata, method="distcor")

require(dendextend)
dend.variables <- dend.variables %>% 
    set("branches_k_color", k=2, value=2:3) %>%
    + set("branches_lwd", 2)
mix.heatmap(mixdata, D.subjects=D.subjects, dend.variables=dend.variables, rowmar=7)

colbar <- sample(c("purple", "darkgrey"), nrow(mixdata), replace=T)
mix.heatmap(mixdata, dist.variables.method="ClustOfVar", ColSideColors=colbar,
            + legend.colbar=c("aa", "bb"), rowmar=7)
