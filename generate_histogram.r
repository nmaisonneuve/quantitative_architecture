library(ggplot2)
source("./tools.r")

#load data
data <- read.csv("data/temporal_table_only_available_clusters.csv")

# transform time period into readable factors
data$period_f = factor(data$period, levels = c(1,2,3,5,6,7,8,9,10,11,99), labels = time_period_labels)

# filtering data inside cluster (taking only the top n% elements for each cluster)
threshold = 0.5
data.filtered = ddply(data,.(detector_id), .fun=function(sub_data){
  cluster.filtered = score_filtering(sub_data,threshold)
  return(cluster.filtered)
})

# histogram for each cluster (detector_id)
data.histogram = ddply(data.filtered,.(detector_id), .fun=histogram)

# Choosing the top n clusters/detector_id ranked by entropy 
top_n = 100
best_cluster_entropy = filter_clusters_by_entropy(data.filtered, top_n)
write.csv(best_cluster_entropy,"cluster_entropy.csv")
cluster_list = best_cluster_entropy$id
# or defining a list of specific clusters
# cluster_list = c(2055,2453,5999,6628)

# selecting related data 
data.selected = data.histogram[data.histogram$detector_id %in% cluster_list,]
# reordering the factor attribute to be aligned with the ranking 
# (= not sorted by the natural order of the cluster_id integer)
data.selected$detector_id <- factor(data.selected$detector_id, levels = cluster_list)

# clean the directory if needed
base_dir = paste("./results/",as.integer(threshold*100))
histo_dir = paste(base_dir,"/histograms")
for (dir in c(base_dir, histo_dir)){
  file.remove(dir(dir,full.names = TRUE))
  dir.create(dir)
}

# generate csv with entropy related info
write.csv(best_cluster_entropy,"cluster_entropy.csv")

# generate png image of the histogram for each selected cluster
for (cluster_id in cluster_list) {
  cat("generating png for cluster ", cluster_id)
  cluster_data = data.selected[data.selected$detector_id == cluster_id,]
  save_histogram(cluster_data,cluster_id,histo_dir)
}