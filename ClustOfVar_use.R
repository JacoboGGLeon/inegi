install.packages("ClustOfVar", dependencies = TRUE)
install.packages("cluster", dependencies = TRUE)

library(ClustOfVar)
library(cluster)

data <- mixdata
names(data)
summary(data)

xquant <- data[,c(2, 3, 6, 7, 10)] # Numeric variables
xqual <- data[,c(1, 4, 5, 8, 9)]      # Categorical variables

tree <- hclustvar(xquant, xqual)
plot(tree)

stab <- stability(tree, B=50) # "B=50" refers to the number of bootstrap samples to use in the estimation.

k_means <- kmeansvar(X.quanti = xquant, X.quali = xqual, init=4)
summary(k_means)