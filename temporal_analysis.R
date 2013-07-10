library(ggplot2)
source("./tools.r")

data <- read.csv("/Users/maisonne/Documents/work/data/detections/temporal_table.csv")


# transforming data
data$period_f = factor(data$period, levels = c(1,2,3,5,6,7,8,9,10,11,99), labels = time_period_labels)
#[ranked_clusters$id<7000,]

# filtering data inside cluster (taking only the top n% elements for each cluster)
threshold = 0.5
data.filtered = ddply(data.selected,.(detector_id), .fun=function(sub_data){
  cluster.filtered = score_filtering(sub_data,threshold)
  return(cluster.filtered)
})

# histogram for each cluster (detector_id)
data.histogram = ddply(data.filtered,.(detector_id), .fun=histogram)

#cumulative histogram
test <- data.histogram[data.histogram$detector_id == 405,]
cumul <- histogram(data.filtered)

test$weighted_dens = test$dens / cumul$dens
test$weighted_freq = test$freq / cumul$freq

qplot(data=cumul, x=period, y=dens, geom="bar", stat="identity")
qplot(data=test, x=period, y=dens, geom="bar", stat="identity")

qplot(data=test, x=period, y=freq, geom="bar", stat="identity")

qplot(data=test, x=period, y=weighted_dens, geom="bar", stat="identity")
qplot(data=test, x=period, y=weighted_freq, geom="bar", stat="identity")

# Choosing the top n clusters/detector_id ranked by entropy 
top_n = 0
cluster_list = filter_clusters_by_entropy(data.filtered, top_n)

print(cluster_list)

# or defining a list of specific clusters
# cluster_list = c(2055,2453,5999,6628)

# selecting related data 
data.selected = data[data$detector_id %in% cluster_list,]
# reordering the factor attribute to be aligned with the ranking 
# (= not sorted by the natural order of the cluster_id integer)
data.selected$detector_id <- factor(data.selected$detector_id, levels = cluster_list)


# overview graph
#plot2 <- ggplot(data= data.filtered) + geom_bar(aes(x=period_f)) + facet_wrap(~ detector_id, ncol=7) 

plot2 <- ggplot(data= data.histogram) + geom_bar(aes(x=period, y=dens), stat="identity") + facet_wrap(~ detector_id, ncol=7) 
plot2 <- plot2 + theme_bw() + ylab("Frequency")+ xlab("Period of time")   + opts(aspect.ratio=1)  +  coord_flip()
ggsave(plot2, file="histogram_top400_v2.pdf")



sapply(unique(data.selected$detector_id), FUN=function(x){ 
  save_histogram(x,"./histograms/")
  return(0)
  })

save_histogram(2755,"./histograms/")


plot_overview <- function(data){
  plot = ggplot(data = data) + geom_histogram(aes(x=period_f))  + theme_bw()+ facet_wrap(~ ranking, ncol=10)  + ylab("Frequency")+ xlab("Period of time")   + opts(aspect.ratio=1) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +  coord_flip()  
  return(plot)
}


#plot2 <- ggplot(data= data.selected) + geom_histogram(aes(x=period_f)) +  scale_x_discrete(breaks=NULL)+ scale_y_discrete(breaks=NULL) + theme_bw()+ facet_grid(~ ranking, ncol=10, labeller= mf_labeller)  + ylab("Frequency")+ xlab("Period of time")   + opts(aspect.ratio=1)  +  coord_flip()
#print(plot2)

# add the ranking number, replacing the detector_id column
# data.selected$ranking = sapply(data.selected$detector_id, FUN=function(x){return (which(list_top_clusters$id==x))})
# ggplot(data= data) + geom_histogram(aes(x=period))  + theme_bw()+ facet_wrap(~ detector_id, ncol=7)+ ylab("Frequency")+ xlab("Period of time")  + opts(axis.title.x = theme_text(size = 12, vjust = -0.5), axis.title.y = theme_text(size = 14, angle=90, vjust = 0),aspect.ratio=1)
# ggplot(data= data) + geom_histogram(aes(x=period_f))  + theme_bw()+ facet_wrap(~ detector_id, ncol=7)  + ylab("Frequency")+ xlab("Period of time")   + opts(aspect.ratio=1) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +  coord_flip()
# data_cluster = data[data$detector_id == 3961,]
# count<-ddply(data,.(detector_id, period_f), summarize, freq=length(period_f))