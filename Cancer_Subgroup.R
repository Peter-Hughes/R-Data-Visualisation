library(FactoMineR)
library(factoextra)
library(pca3d)
library(cluster)  # daisy() for Dissimilarity Matrix Calculation

file_name <- "Complete_Dataset.csv"
# Read an input csv file from the working directory
Complete_dataaset_220 <- as.data.frame(read.csv(file_name, row.names = 1))


# Run k-means clustering algorithm on this dataset and identify the cluster labels (single run)
K_means_Model <- kmeans(t(Complete_dataaset_220),
                        centers = 4, 
                        iter.max = 50,
                        nstart = 10) 

plot(t(Complete_dataaset_220), col = K_means_Model$cluster) 
points(K_means_Model$centers, col = 1:4, pch = 10, cex = 5)  

fviz_cluster(K_means_Model,
             t(Complete_dataaset_220),
             geom = "point",
             shape = NULL,
             labelsize = 8,
             ellipse.type = "norm")


# Using k-means clustering in multiple runs with different initialisation settings
set.seed(20)
Number_of_runs <- 4
Matrix_labels_different_runs <- matrix(nrow = ncol(Complete_dataaset_220), ncol = Number_of_runs,0)
rownames(Matrix_labels_different_runs) <- colnames(Complete_dataaset_220)

_means_lapply <- lapply(1:Number_of_runs,
                        function(i) kmeans(t(Complete_dataaset_220),
                                           centers = 4, 
                                           iter.max = 50,
                                           nstart = 5)
                        )

for (j in 1:Number_of_runs) {
  Matrix_labels_different_runs [ , j]<- K_means_lapply[[j]]$cluster
  
  fviz_cluster(K_means_lapply[[j]],
               t(Complete_dataaset_220),
               geom = "point",
               shape = NULL,
               labelsize = 8,
               ellipse.type = "norm")
} 


# Calculating dissmilarity matrix
K_means_Model <- kmeans(t(Complete_dataaset_220),
                        centers = 4, 
                        iter.max = 50,
                        nstart = 10)

Dissimilarity_Matrix <- daisy(t(Complete_dataaset_220),
                              metric = c("euclidean"))

dE2   <- Dissimilarity_Matrix^2

Silhouette_kmeans   <- silhouette(K_means_Model$cluster, dE2)

plot(Silhouette_kmeans)

pca3d(Complete_dataaset_220, 
      col = c("blue", "red", "yellow", "green"),
      group = K_means_model$cluster, 
      fancy = F,
      show.ellipses = T, 
      ellipse.ci = 0.95
)
