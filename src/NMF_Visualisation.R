library(NMF)
library(FactoMineR)
library(factoextra)

filename <- "Complete_Dataset.csv"


complete_dataset_220 <- read.csv(filename, row.names = 1)

# Initialise the parameters used for the nmf
seq1 <- 2
seq2 <- 8
num_run <- 20
initseed <- 3062
usedmethod <- "ns"

# Calculate the nmf for the complete dataset from 2:8 with 20 runs
Res_MultiRank <- nmf(complete_dataset_220, seq(seq1, seq2),
                     method = usedmethod, nrun = num_run,
                     seed = initseed, .options = "t")

# Plot the multi rank 
plot(Res_MultiRank)

# Find the max cophen rank 
cophen_max <- max(Res_MultiRank$measures$cophenetic)

# Find the idex of the max rank 
for(i in 1:length(Res_MultiRank$measures$rank)){
  if(Res_MultiRank$measures$cophenetic[i] == cophen_max) {
    idx_cophen_max <- i
  }
}

# Use the index of the max rank to find the max fit
NMFfitClass <- Res_MultiRank$fit[[idx_cophen_max]]
# Take the H value of the max fit
h_matrix <- NMFfitClass@fit@H
# Take the W value of the max fit
w_matrix <- NMFfitClass@fit@W

# Get the PCA of the H matrix 
PCA_Model_prcomp <- prcomp(t(h_matrix), center = T, scale=F)
# Show the values of the PCA 
PCA_Model_prcomp$x 
# Show the summary of the PCA
summary(PCA_Model_prcomp)

# Visualise why we should only use 3 dimensions
fviz_eig(PCA_Model_prcomp, addlabels = T, ylim=c(0, 60))

# Extract the immune subtype groups from the PCA of the H matrix 
# Regex: Replace all characters before the last '_' character with an empty string
immune_subtype <- as.integer(sub(".*_", "", rownames(PCA_Model_prcomp$x)))

# Bind the first 3 dimensions of data to their immune subtype groups
PCA_Model <- cbind(PCA_Model_prcomp$x[,1:3], immune_subtype)

# Create a new PCA Model of the 3 dimensions 
PCA_Model_Input_Dataset <- PCA(PCA_Model[,1:3], scale.unit = TRUE, ncp = 17, graph = F)

# Visualise the data 
fviz_pca_ind(PCA_Model_Input_Dataset,
             geom.ind = "point",
             col.ind = as.character(immune_subtype),
             palette = c("blue", "green", "orange", "red"),
             addEllipses = TRUE,
             legend.title = "Groups"
)

