# Take cosmic as an example to draw cluster

library(ggplot2)
library(factoextra)
library(magrittr)

# 1.clusting by distance
# different distance method for hierarchical clustering
distmethod <- c('euclidean', 'manhattan', 'maximum', 'minkowski', 'canberra')   # 欧几里得，曼哈顿，切比>雪夫，明可夫斯基距离, 兰氏距离
hcmethod <- c('complete', 'single', 'average', 'median', 'centroid', 'ward.D2')  # 最长距离，最短距离，平均距离，中间距离法，重心法，离差平方和法
cosmic_distance_cluster  <- function() {
  cosmicnorm <- read.csv(paste(pathinput,'cosmic_normal_methods.tsv', sep=''), header=T, sep='\t')
  cosmic <- t(cosmicnorm)
  for (dm in distmethod) {
    for (hc in hcmethod) {
    dist <- dist(cosmic, method=dm)
    filename <- paste(pathoutput, paste("cluster/cosmic_normal_method_",dm,"_",hc,"_plot.png", sep=""),sep="")
    png(file=filename,width=15,height=15,units="cm",res=300)
    par(mar = c(5,5,4,7))
    mt <- hclust(dist, method=hc)
    dend <- as.dendrogram(mt)
    plot(dend, horiz=T, main=paste('cosmic normal', dm, hc, sep=' '))
    dev.off()      
    }
  }
}


# 2.clusting by k-means
cosmic_kmeans_plot <- function() {
  cosmicnorm <- read.csv('/data/wangs/MutaAnalysis/cosmic/cosmic_normal_methods.tsv', header=T, sep='\t')
  cosmic <- t(cosmicnorm)
  num <- c(2,5,8)
  for (i in num) {
    filename <- paste(pathoutput, paste("cluster/cosmic_kmeans_",i,"_cluster_plot.png", sep=''),sep="")
    km.res <- kmeans(cosmic, centers = i, nstart=25)  # i is the optimal number of clusters
    p <- fviz_cluster(km.res, data = cosmic, ellipse.type = "convex", ggtheme = theme_minimal(), main=paste('cosmic kmeans',i,' cluster plot', sep=''))
    ggsave(p, filename=filename,width=23,height=21,units=c('cm'))
  } 
}
