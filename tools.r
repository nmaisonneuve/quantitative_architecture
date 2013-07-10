library(plyr)


# format the period attribute into human readable factors 
time_period_labels = c("<= 1800", "1801-1850", "1851-1914","1915-1939","1940-1967","1968-1975","1976-1981","1982-1989","1990-1999",">= 2000","Unknown")
time_period = factor(time_period_labels, levels = time_period_labels)

# building temporal histogram 
# NOTE: mapping and not grouping time period method 
# i.e. compared a basic aggregation function:
# ddply(x,.(period_f), summarize, freq=length(period_f)), 
# this one takes each time period and compute its frequency *even if the period is not existing in the data*
histogram<-function (data){
  
  # mapping labels with their associated nb of occurences for each period of time
  count <- vapply(time_period_labels, 1, 
                  FUN = function(y) { 
                        count = length(which(data$period_f == y)) 
                        return(count)
                      },USE.NAMES = FALSE)
  # normalizing
  density = count / sum(count)
  
  histo <- data.frame(period = time_period, freq = count, dens = density)
  return (histo)
}


# Filtering the detections having a score below a relative threshold 
# e.g. filtering_by_score(data, 0.1)  = take the detections having a score in the top 10% of the set.
score_filtering <- function (data, threshold){
  
  # ordered desc by score
  data.ordered = data[order(-data$score),]
  
  # number of elements to take
  n_filtered_rows = nrow(data.ordered)  * threshold
  
  return(data.ordered[1:n_filtered_rows,])
}


# Compute entropy of the time period  histogram
entropy <- function(data){
  
  # compute histogram
  hist = histogram(data)
  
  # adding 0.0001 to the normalized histogram to prevent log(0)
  pi = hist$density + 0.000001 
  
  entropy = - sum(pi * log(pi)) 
  return (entropy)
}

# Choosing the top n clusters/detector_id ranked by entropy 
filter_clusters_by_entropy <- function (data, top_n = 10){
  # Computing the entropy of the histogram for each cluster
  cluster.entropy = ddply(data,.(detector_id), .fun=entropy) 
  names(cluster.entropy)= c("id", "entropy")
  
  # Choosing the top n clusters/detector_id ranked by entropy 
  ranked_clusters = cluster.entropy[order(-cluster.entropy$entropy),]
  top_clusters = ranked_clusters[1:top_n,]
return (top_clusters)
}

#create the histogram for a given cluster
save_histogram <-function (data, filename, dest_dir){
  filename = paste(dest_dir,filename,".png",sep="")

  plot2 <- ggplot(data = data) + geom_bar(aes(x=period, y=dens), stat="identity") + opts(aspect.ratio=1)
  # plot2 <- ggplot(data=d) + geom_histogram(aes(x=period_f)) + opts(aspect.ratio=1)  #+  coord_flip()
  # plot2 <-  plot2 +  ylab("Frequency")+  xlab("Period of time") 
  plot2 <- plot2 + theme(
    #axis.text.x  = element_text(angle=90, vjust=0.5) 
    #panel.grid.minor = element_blank(),
    #panel.grid.major = element_blank(),
    panel.border = element_blank(),
       axis.text.x=element_blank(),
     axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
     axis.title.y=element_blank()
  )
  # panel.background=theme_blank(),panel.border=theme_blank(),panel.grid.major=theme_blank(),
  # panel.grid.minor=theme_blank(),plot.background=theme_blank()
  ggsave(plot2,file=filename, width = 5, height = 5, dpi=50)
  return(plot2)
}

plot_overview <- function(data){
  plot = ggplot(data = data) + geom_histogram(aes(x=period_f))  + theme_bw()+ facet_wrap(~ ranking, ncol=10)  + ylab("Frequency")+ xlab("Period of time")   + opts(aspect.ratio=1) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +  coord_flip()  
  return(plot)
}
