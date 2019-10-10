
#Author: Wangshuqin
#Data: 2019-09-09 17:05:15
#Description: Drawing BoxPlot

## 1.drawing a boxplot
boxplot(df[, col], xaxt='n',outline=FALSE, cex.axis = 1.5)
mtext(ylab.name,side=2,line=3.5,cex=2,col="black",font=2)
mtext(method,side=3,line=1.5,col="black",cex=2,font=2)
dev.off()

## 2.drawing multiple boxplot in one picture
subset1 <- subset1[,col]
subset2 <- subset2[,col]
# If the number of subset1 and subset2 is different, do the next step.
length(subset1) <- max(length(subset1),length(subset2))
length(subset2) <- max(length(subset1),length(subset2))
df <- cbind(subset1,subset2)
boxplot(df, xaxt='n',outline=FALSE, cex.axis = 1.5)
legend(x='bottomleft', legend='')
axis(side=1,at=1:2,labels=c('subset1','subset2'),cex.axis = 1.0)
# If you still want to add point to the boxplot, go to the next step.
stripchart(subset1,at=1,vertical = TRUE, method = "jitter", add = TRUE, pch = 20, col = "blue")
stripchart(subset2,at=2,vertical = TRUE, method = "jitter", add = TRUE, pch = 20, col = "red") 
# Add label
mtext(ylab.name,side=2,line=3.5,cex=2,col="black",font=2)
mtext(xlab.name,side=3,line=1.5,col="black",cex=2,font=2)
dev.off()

## 3.drawing multiple boxplot in one picture and sort each boxplot by meanvalue. Here take cosmic data as an example.
cosmic_age_boxplot <- function () {
  age <- c('age0','age1','age2')
  for (method in methodlist) {
    aa <- list()
    filename <- paste(pathoutput,"age_boxplot/cosmic_age_",method,"_boxplot.png",sep="")
    png(file=filename,width=22,height=20,units="cm",res=300) 
    par(mar=c(6, 7, 5, 2))
    ylab.name = "score"
    for (type in age) {
      datatype <- subset(cosmic,cosmic$age==type)
      data.val <- datatype[,method]
      aa <- c(aa, list(data.val))
    }
    # take the average of each subset into b1
    b1 <- c()
    for (nl in aa) {
      b <- mean(nl)
      b1 <- c(b1,b)
    }
    aa1 <- aa[order(b1)]
    boxplot(aa1, xaxt='n',outline=FALSE, cex.axis = 1.5)
    axis(side=1,at=1:length(age),labels=age[order(b1)],cex.axis = 1.0)
    mtext(ylab.name,side=2,line=3.5,cex=2,col="black",font=2)
    mtext(method,side=3,line=1.5,col="black",cex=2,font=2)
    dev.off() 
  }
}

