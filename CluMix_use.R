
install.packages("BiocManager", dependencies = TRUE)
source("https://bioconductor.org/biocLite.R")

BiocManager::install("marray")

BiocManager::install(version = "3.8")

source("https://bioconductor.org/biocLite.R")
biocLite("Biobase")

library(CluMix)
library(ClustOfVar)
data(mixdata)
str(mixdata)


sim_var <- CluMix::similarity.variables(data = mixdata)

sim_sub <- CluMix::similarity.subjects(data = mixdata)

dis_sub <- CluMix::dist.subjects(data = mixdata)

dis_var <- CluMix::dist.variables(data = mixdata)

distmap(mixdata, what="variables", margins=c(6,6))
S <- similarity.variables(mixdata)
distmap(S)

confounderPlot(mixdata, x="X4.ord", y="X1.cat")
