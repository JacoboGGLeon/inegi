Sys.setenv('R_MAX_VSIZE'= 40000000000)

# Libraries
library('ggplot2')
library('factoextra')
library('stats')
library(gghighlight)



# Load
df <- read.csv('enh_grouped.csv')
head(df)
data <- df[, 5:58]
data <- scale(x = data)

dist_man <- stats::dist_man(x = data, method = 'manhattan')

single_hclust <- stats::hclust(d = dist_man, method = "single")
wardD_hclust <- stats::hclust(d = dist_man, method = "ward.D")
wardD2_hclust <- stats::hclust(d = dist_man, method = "ward.D2")
complete_hclust <- stats::hclust(d = dist_man, method = "complete")
average_hclust <- stats::hclust(d = dist_man, method = "average")
mcquitty_hclust <- stats::hclust(d = dist_man, method = "mcquitty")
median_hclust <- stats::hclust(d = dist_man, method = "median")
centroid_hclust <- stats::hclust(d = dist_man, method = "centroid")

# Compute cophentic dist_manance
single_coph <- stats::cophenetic(x = single_hclust)
wardD_coph <- stats::cophenetic(x = wardD_hclust)
wardD2_coph <- stats::cophenetic(x = wardD2_hclust)
complete_coph <- stats::cophenetic(x = complete_hclust)
average_coph <- stats::cophenetic(x = average_hclust)
mcquitty_coph <- stats::cophenetic(x = mcquitty_hclust)
median_coph <- stats::cophenetic(x = median_hclust)
centroid_coph <- stats::cophenetic(x = centroid_hclust)

# Correlation between cophenetic dist_manance and # the original dist_manance
cor_single <- stats::cor(x = dist_man, y = single_coph)
cor_single

cor_wardD <- stats::cor(x = dist_man, y = wardD_coph)
cor_wardD

cor_wardD2 <- stats::cor(x = dist_man, y = wardD2_coph)
cor_wardD2

cor_complete <- stats::cor(x = dist_man, y = complete_coph)
cor_complete

cor_average <- stats::cor(x = dist_man, y = average_coph)
cor_average

cor_mcquitty <- stats::cor(x = dist_man, y = mcquitty_coph)
cor_mcquitty

cor_median <- stats::cor(x = dist_man, y = median_coph)
cor_median

cor_centroid <- stats::cor(x = dist_man, y = centroid_coph)
cor_centroid

hcor_vect <- c(cor_wardD, cor_wardD2, 
               cor_single, cor_complete, 
               cor_average, cor_mcquitty, 
               cor_median, cor_centroid)

hcor_names <- c("Ward D", "Ward D2", 
                "Single", "Complete", 
                "Average", "McQuitty", 
                "Median", "Centroid")

df_hcor <- do.call(rbind, Map(f = data.frame, 
                              names = hcor_names, 
                              values = hcor_vect))

df_max_hcor_man <- do.call(rbind, Map(f = data.frame, 
                                      names = hcor_names[which.max(x = hcor_vect)], 
                                      values = max(hcor_vect)))

hcor_plot_man <- ggplot2::ggplot(data = df_hcor,
                                 aes(x = df_hcor$names, y = df_hcor$values))
max_hcor_man_man <- ggplot2::ggplot(data = df_max_hcor_man, 
                                    aes(x = df_max_hcor_man$names, y = df_max_hcor_man$values))

hcor_plot_man +
  ggplot2::theme_bw() + 
  ggplot2::geom_point() +
  ggplot2::labs(title = "Correlation Linkages", 
                x = "Links", 
                y = "Correlation") +
  ggplot2::geom_point(data = df_max_hcor_man,
                      aes(x = df_max_hcor_man$names, 
                          y = df_max_hcor_man$values),
                      color="red",
                      size=5) +
  ggplot2::geom_text(data = df_max_hcor_man,
                     aes(x = df_max_hcor_man$names, 
                         y = df_max_hcor_man$values, 
                         label = round(x = df_max_hcor_man$values, 
                                       digits = 2)), 
                     hjust=1/2, vjust=2)

dunn_dia_man <- c()
range_dia_man <- c(2:15)
i <- 1

dia_man <- cluster::diana(x = data, metric = "manhattan")

for (h in range_dia_man) {
  set.seed(123)
  
  dia_man_group <- stats::cutree(tree = dia_man, k = h) 
  
  index_internal_dia_man <- clusterCrit::intCriteria(traj = data,
                                                     part = as.integer(dia_man_group), 
                                                     crit = "dunn")
  dunn_dia_man[i] <- index_internal_dia_man$dunn
  
  i <- i + 1
}

plot_dunn_dia_man <- ggplot2::qplot(x = range_dia_man, 
                                    y = dunn_dia_man, 
                                    geom = 'line', 
                                    main = 'Dunn index: agg euc', 
                                    xlab = 'clusters', ylab = 'distances')

plot_dunn_dia_man + 
  theme_bw() +
  geom_vline(xintercept = range_dia_man[which.max(dunn_dia_man)], linetype = 2, color='red')

dia_man_group <- stats::cutree(tree = dia_man, k = range_dia_man[which.max(dunn_dia_man)]) 

table(dia_man_group)

library(clusterCrit)
index_internal_dia_man <- clusterCrit::intCriteria(traj = data,
                                                   part = as.integer(dia_man_group), 
                                                   crit = "all")
