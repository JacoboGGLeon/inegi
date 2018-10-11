install.packages("gghighlight", dependencies = T)


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

dist_euc <- stats::dist_euc(x = data, method = 'euclidean')

single_hclust <- stats::hclust(d = dist_euc, method = "single")
wardD_hclust <- stats::hclust(d = dist_euc, method = "ward.D")
wardD2_hclust <- stats::hclust(d = dist_euc, method = "ward.D2")
complete_hclust <- stats::hclust(d = dist_euc, method = "complete")
average_hclust <- stats::hclust(d = dist_euc, method = "average")
mcquitty_hclust <- stats::hclust(d = dist_euc, method = "mcquitty")
median_hclust <- stats::hclust(d = dist_euc, method = "median")
centroid_hclust <- stats::hclust(d = dist_euc, method = "centroid")

# Compute cophentic dist_eucance
single_coph <- stats::cophenetic(x = single_hclust)
wardD_coph <- stats::cophenetic(x = wardD_hclust)
wardD2_coph <- stats::cophenetic(x = wardD2_hclust)
complete_coph <- stats::cophenetic(x = complete_hclust)
average_coph <- stats::cophenetic(x = average_hclust)
mcquitty_coph <- stats::cophenetic(x = mcquitty_hclust)
median_coph <- stats::cophenetic(x = median_hclust)
centroid_coph <- stats::cophenetic(x = centroid_hclust)

# Correlation between cophenetic dist_eucance and # the original dist_eucance
cor_single <- stats::cor(x = dist_euc, y = single_coph)
cor_single

cor_wardD <- stats::cor(x = dist_euc, y = wardD_coph)
cor_wardD

cor_wardD2 <- stats::cor(x = dist_euc, y = wardD2_coph)
cor_wardD2

cor_complete <- stats::cor(x = dist_euc, y = complete_coph)
cor_complete

cor_average <- stats::cor(x = dist_euc, y = average_coph)
cor_average

cor_mcquitty <- stats::cor(x = dist_euc, y = mcquitty_coph)
cor_mcquitty

cor_median <- stats::cor(x = dist_euc, y = median_coph)
cor_median

cor_centroid <- stats::cor(x = dist_euc, y = centroid_coph)
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

df_max_hcor <- do.call(rbind, Map(f = data.frame, 
                                  names = hcor_names[which.max(x = hcor_vect)], 
                                  values = max(hcor_vect)))

hcor_plot <- ggplot2::ggplot(data = df_hcor,
                             aes(x = df_hcor$names, y = df_hcor$values))
max_hcor <- ggplot2::ggplot(data = df_max_hcor, 
                             aes(x = df_max_hcor$names, y = df_max_hcor$values))

hcor_plot +
  ggplot2::theme_bw() + 
  ggplot2::geom_point() +
  ggplot2::labs(title = "Correlation Linkages", 
                x = "Links", 
                y = "Correlation") +
  ggplot2::geom_point(data = df_max_hcor,
                      aes(x = df_max_hcor$names, 
                          y = df_max_hcor$values),
                      color="red",
                      size=5) +
  ggplot2::geom_text(data = df_max_hcor,
            aes(x = df_max_hcor$names, 
                y = df_max_hcor$values, 
                label = round(x = df_max_hcor$values, 
                              digits = 2)), 
            hjust=1/2, vjust=2)

dunn_agg_euc <- c()
range_agg_euc <- c(3:10)
i <- 1

agg_euc <- cluster::agnes(x = data, metric = "euclidean", method = "average")

for (h in range_agg_euc) {
  set.seed(123)
  
  agg_euc_group <- stats::cutree(tree = agg_euc, k = h) 
  
  index_internal_agg_euc <- clusterCrit::intCriteria(traj = data,
                                                part = as.integer(agg_euc_group), 
                                                crit = "dunn")
  dunn_agg_euc[i] <- index_internal_agg_euc$dunn
  
  i <- i + 1
}

plot_dunn_agg_euc <- ggplot2::qplot(x = range_agg_euc, 
                                    y = dunn_agg_euc, 
                                    geom = 'line', 
                                    main = 'Dunn index: agg euc', 
                                    xlab = 'clusters', ylab = 'distances')

plot_dunn_agg_euc + 
  theme_bw() +
  geom_vline(xintercept = range_agg_euc[which.max(dunn_agg_euc)], linetype = 2, color='red')

agg_euc_group <- stats::cutree(tree = agg_euc, k = range_agg_euc[which.max(dunn_agg_euc)]) 

table(agg_euc_group)

library(clusterCrit)
index_internal_agg_euc <- clusterCrit::intCriteria(traj = data,
                                                   part = as.integer(agg_euc_group), 
                                                   crit = "all")
