install.packages('truncnorm', dependencies = T)
library(truncnorm)

df <- read.csv('enh_grouped.csv')
head(df)s
data <- df[, 5:58]

plot(truncnorm::rtruncnorm(n = data$antiguedad))
