rm(list = ls())
getwd()
setwd("User/dir/") # set your directory

library(ggplot2)

## 1.barplot
cosmic_binary_prediction_barplot <- function() {
  filename <- paste(pathoutput,"cosmic_binary_pred_barplot.png",sep="")
  png(file=filename,width=22,height=20,units="cm",res=300) 
  par(mar=c(6, 7, 5, 2))
  f1 <- read.csv(paste(pathoutput,'cosmic_binary_prediction_count.txt',sep=''),header=T,sep = "\t")
  f2 <- f1[order(f1$percentage_of_damaging,decreasing = TRUE),]  # Sort the "percentage_of_damaging" column of f1 from largest to smallest, other columns also follow changes
  f3 <- t(f2)
  ratio = as.numeric(f3[4,])
  barplot(ratio, names.arg = f3[1,], main = 'cosmic', font.main = 4, las=3, cex.names = 0.6)
  dev.off()
}

## 2.ggplot2
cosmic_binary_prediction_barplot <- function() {
  filename <- paste(pathoutput,"cosmic_binary_pred_barplot.png",sep="")
  f1 <- read.csv(paste(pathoutput,'cosmic_binary_prediction_count.txt',sep=''),header=T,sep = "\t")
  p <- ggplot(f1, aes(x=reorder(f1$method, f1$percentage_of_damaging), f1$percentage_of_damaging)) +
        geom_bar(stat='identity',position=position_dodge()) + theme(axis.text.x=element_text(angle=45, size=12), axis.text.y=element_text(size=10), axis.title.y=element_text(size=13),
        plot.title=element_text(size=20, hjust=0.5), panel.background=element_blank(), axis.line.y = element_line(colour = "black")) +
        labs(x='', y = 'percentage of damaging mutation', title='COSMIC')
  ggsave(p, filename=filename, width=23, height=21, units=c("cm"))
  dev.off()
}
