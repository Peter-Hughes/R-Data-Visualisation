# Clear objects from memory
rm(list = ls())

file_name <- "PanCanAtlas_440Genes_SampleIdsOrdered_RR020718_RownamesGenesWithSignature.csv"

# Read in the cancer data from the css, separated by the comma
cancer_data <- read.csv(file_name, sep = ",")


# Get the sample names from the column names 
sample_names <- colnames(cancer_data)

# Extract cancer subtype from the sample names
extract_cancer_subtype <- function(sample_name) substring(sample_name, 14, nchar(sample_name) - 3) 

# Extract cancer subtypes from sample names and sort them alphabetically
cancer_subtypes <- sort(sapply(sample_names[2:length(sample_names)], extract_cancer_subtype))

# Store cancer subtypes as a table in a data frame to get number of samples 
cancer_subtypes_table <- as.data.frame(table(cancer_subtypes))

# Set the column names to match assignment criteria 
colnames(cancer_subtypes_table) <- c("Cancer subtype", "Number of samples")

# Write cancer subtype table to csv file 
write.csv(cancer_subtypes_table, file = "Num_Samples_Per_Subtype.csv")

# Extract immune subtypes from sample names (Regex too delete everything before and including ".")
immune_subtypes <- sub(".*\\.", "", sample_names[2:length(sample_names)])

# Store immune subtypes as table in a data frame
immune_subtypes_table <- as.data.frame(table(immune_subtypes))

# Set the column names to match assignment criteria 
colnames(immune_subtypes_table) <- c("Immune subtypes", "Frequency")

# Save the following plot with text as a png 
png(filename = "Num_Immune_Subtypes.png")

# Plot immune subtypes in a bar plot
immune_subtypes_barplot <- barplot(table(immune_subtypes), xlab = "Immune Subtypes", ylab = "Frequency", col = "blue", 
                   ylim = c(0, 3000), legend.text = "Total")

# Append text for the bar plot of the frequency of immune subtypes above each bar
text(x = immune_subtypes_barplot, y = immune_subtypes_table$Frequency, pos = 3, labels = immune_subtypes_table$Frequency)

# Shut down all open graphics devices
dev.off()