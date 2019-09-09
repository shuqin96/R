rm(list = ls())
getwd()
setwd("User/dir/") # set your directory

library(pheatmap)
library(ggplot2)

## 1.heatmap
heatmap(data, main = '', xlab = '', ylab = '')  # data is matrix

## 2.pheatmap
pheatmap(data,border_color = 'grey',cluster_rows = FALSE,cluster_cols = FALSE,main='heatmap', # data is a matrix
        legend_breaks = c(0,0.5,1), legend_labels = c("no driver","","have at last one driver"), color = colorRampPalette(c("red", "white","blue")),  # User-defined color display
        display_numbers = F, 
        filename = filename1, width=8, height=8) 

## 3.ggplot2
plot <- ggplot(data,aes(x,y)) + xlab('') + ylab('') + 
  geom_tile(aes(fill=value),colour="white") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=1), plot.title=element_text(size=12, hjust=0.5), panel.background=element_blank()) + # horizontal display of horizontal axis
  labs(fill=value,title=value) + 
  scale_fill_gradientn(colors=c('white','white','red'),values=rescale(c(0,0.5,1))) + # User-defined color display
  theme(legend.background = element_rect(fill="gray", size=0.5, linetype="dotted"))
ggsave(plot, filename=filename, width=23, height=21, units=c("cm"))

