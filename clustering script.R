# Loading packages

install.load::install_load(c("tidyverse", "readxl", "factoextra", "NbClust"))

# Importing data

data <-  read_xlsx("COVID-19 and economic data in Nigeria vs3.xlsx", sheet = 1, skip = 1) %>% select(-c(2,3, 4, 7, 14, 17:21, 25)) %>% janitor::clean_names() %>%  mutate_at(5, ~as.numeric(.)) %>% tidyimpute::impute_mean() %>% column_to_rownames(var = "states")

# Scaling data 

data <- scale(data)

# Method of choosing K

# 1. Elbow method

fviz_nbclust(data, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) + # add line for better visualization
  labs(subtitle = "Elbow method") # add subtitle


# 2.  Silhouette method

fviz_nbclust(data, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")


# Clustering method:  k-means clustering

km.res <- eclust(data, "kmeans", k = 3, nstart = 50, graph = FALSE)

# k-means group number of each observation
head(km.res$cluster, 15)

# Visualize k-means clusters

fviz_cluster(km.res,  ellipse.type  = "norm", ellipse.level = 0.95)


# Clustering method: Hierarchical clustering: single linkage

hclust <- hclust(dist(data), method = "single")

plot(hclust)

rect.hclust(hclust,
            k = 3, # k is used to specify the number of clusters
            border = "blue"
)

barplot(hclust$height,
        names.arg = (nrow(data) - 1):1 # show the number of cluster below each bars
)


# Clustering method:  Enhanced hierarchical clustering

res.hc <- eclust(data, "hclust", k = 4,
                 method = "ward.D2", graph = FALSE) 

head(res.hc$cluster, 15)

# Dendrogram
fviz_dend(res.hc, rect = TRUE, show_labels = TRUE, cex = 0.5) 

fviz_cluster(res.hc, ellipse.type = "norm", ellipse.level = 0.68)

# Visualize the silhouette of clusters
fviz_silhouette(res.hc)

# Clustering method: Combining hierarchical clustering and k-means (Compute hierarchical k-means clustering)

res.hk <-hkmeans(data, 4)


# Visualize the tree
fviz_dend(res.hk, cex = 0.6, rect = TRUE)

# Visualize the hkmeans final clusters
fviz_cluster(res.hk, ellipse.type = "norm", ellipse.level = 0.68)


# Learning resources

browseURL("https://www.r-bloggers.com/2020/02/the-complete-guide-to-clustering-analysis-k-means-and-hierarchical-clustering-by-hand-and-in-r-2/")

browseURL("http://www.sthda.com/english/wiki/wiki.php?id_contents=8098")

browseURL("https://uc-r.github.io/kmeans_clustering")