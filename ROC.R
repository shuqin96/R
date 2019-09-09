library(ROCR)

pathoutput <- ('/data/wangs/MutaAnalysis/three.data0223/figure/')
hpm_hpm.random <- read.csv(file=paste(pathoutput, 'combine_cancer_score_from_hpm_hpmrandom_debug.csv', sep=''), header=T)

methodlist.roc <- c('CHASMplus', 'ParsSNP', 'vote_score', 'knn')
collist <- c('thistle3', 'palevioletred1', 'purple', 'orange')

each_data_method_roc <- function() {
	namelist <- c('hpm_hpm.random', 'fmd_fmn')
	for (name in namelist) {
		data <- eval(parse(text = name))
		# AUC
		filename1 <- paste(pathoutput,"data_vote_auc_value_table.txt",sep='')
		if (file.exists(filename1)) file.remove(filename1) 
		write.table(paste("data","method","auc",sep="\t"),filename1,append=TRUE,quote=FALSE, col.names = FALSE, row.names = FALSE, sep="\t")
		for (i in 1:length(methodlist.roc)) {
			auc.tmp <- performance(prediction (data[,methodlist.roc[i]], data$label),"auc")
			auc <- as.numeric(auc.tmp@y.values)
			auc <- as.numeric(formatC(auc, format="f", digits=2))
			write.table(paste(name,methodlist.roc[i],auc,sep="\t"),filename1,append=TRUE, quote=FALSE, col.names = FALSE, row.names = FALSE, sep="\t")
		}
		# ROC curve
		filename2 <- paste(pathoutput, paste(name,"_method_roc.png",sep=''),sep="")
		png(file=filename2,width=15,height=15,units="cm",res=300)
		par(mar = c(5,5,4,1))
		for (i in 1:length(methodlist.roc)) {
			method <- methodlist.roc[i]
			if (i == 1) {
				perf <- performance(prediction(data[,method], data$label), measure = "tpr", x.measure = "fpr")
				plot(perf,lwd=4,cex.lab=1.5, col=collist[i], main=paste(name," method roc", sep=''))
			} else {
				perf <- performance(prediction(data[,method], data$label), measure = "tpr", x.measure = "fpr")
				plot(perf,add = T,lwd=4,cex.lab=1.5, col=collist[i])
			}
		}
		legend("bottomright", bty="n", inset=.005, legend=methodlist.roc, fill=collist, cex = 1.3, text.font=1)
		dev.off()
	}
}

