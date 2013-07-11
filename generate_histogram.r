library(ggplot2)
library("rjson")
source("./tools.r")

#load data
data <- read.csv("data/temporal_table_only_available_clusters.csv")

######## pre-processing ########

names(data)[2] = c("cluster_id")
# transform time period into readable factors
data$period_f = factor(data$period, levels = c(1,2,3,5,6,7,8,9,10,11,99), labels = time_period_labels)

# filtering data inside cluster (taking only the top n% elements for each cluster)
threshold = 0.2
data.filtered = ddply(data,.(cluster_id), .fun=function(sub_data){
  cluster.filtered = score_filtering(sub_data,threshold)
  return(cluster.filtered)
})

##### histogram analysis ######

# histogram for each cluster (cluster_id)
histogram = ddply(data.filtered,.(cluster_id), .fun=period_hist)

# compute entropy for each cluster 
# NOTE: data.table library seems much faster (http://stackoverflow.com/questions/5839265/r-plyr-ordering-results-from-ddply)
entropy = ddply(histogram,.(cluster_id), summarise, entropy=compute_entropy(dens))
# compute period dominance for each cluster
dominance = ddply(histogram,.(cluster_id), summarise, dominance=which.max(freq))
clusters = cbind(entropy, dominance=dominance$dominance)

# ranked by entropy
clusters = clusters[order(-clusters$entropy),]

top_n = 400
top_n = min(nrow(clusters), top_n)

top_clusters = clusters[1:top_n,]
cluster_list = top_clusters$cluster_id

# or defining a list of specific clusters
# cluster_list = c(2055,2453,5999,6628)

# selecting related data 
hist.selected = histogram[histogram$cluster_id %in% cluster_list,]
# reordering the factor attribute to be aligned with the ranking 
# (= not sorted by the natural order of the cluster_id integer)
hist.selected$cluster_id <- factor(hist.selected$cluster_id, levels = cluster_list)

# clean the directory if needed
base_dir = paste("./results/",as.integer(threshold*100),"/",sep = "")
histo_dir = paste(base_dir,"histograms/",sep = "")
for (dir in c(base_dir, histo_dir)){
  file.remove(dir(dir,full.names = TRUE))
  dir.create(dir)
}

# generate png images of the histogram for each selected cluster
for (cluster_id in cluster_list) {
  cat("generating png for cluster ", cluster_id)
  histogram = hist.selected[hist.selected$cluster_id == cluster_id,]
  save_histogram(histogram, cluster_id, histo_dir)
}

# generate json about cluster infos and related patches infos 
cluster_images <- lapply(cluster_list, FUN=function(cluster_id){
  images_filenames = data.filtered[data.filtered$cluster_id == cluster_id,]$filename
  cluster = clusters[clusters$id==cluster_id,]
  return (list(id=cluster_id, images = images_filename, entropy=cluster$entropy, dominance=cluster$dominance))
})
sink(paste(base_dir, "clusters.json",sep=""))
cat(toJSON(cluster_images))
sink()
