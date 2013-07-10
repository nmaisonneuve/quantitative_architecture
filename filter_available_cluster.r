setwd("~/work/temporal_analysis/data/")

data <- read.csv("temporal_table.csv")
available_clusters <- read.csv("available_clusters.csv")[,1]

#filtering with only available images
filtetred_data = data[data$detector_id %in% as.vector(available_clusters),]
data <- write.csv(filtetred_data, "temporal_table_only_available_clusters.csv")
