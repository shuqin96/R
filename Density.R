rm(list = ls())
getwd()
setwd("User/dir/") # set your directory

# 查看score的数值分布
all_score_distribution2 <- function() {
    for (name in c('hpm_fmn','fmd_fmn')) {
        data <- eval(parse(text = name))
        nl <- strsplit(name,'_')[[1]]
        for (method in methodlist) {
            filename <- paste(pathoutput, paste("roc/fmn/", name,method, "_distribution2.png",sep=''), sep="")
            png(file=filename, width=22, height=20, units="cm", res=300)
            par(mfcol=c(2,1))
            # positive dataset
            positive <- eval(parse(text = nl[1]))
            pos_score <- positive[,method]
            plot(density(pos_score), col='red', xlab = method, main=name)
            # negative dataset
            negative <- eval(parse(text = nl[2]))
            neg_score <- negative[,method]
            lines(density(neg_score), col='steelblue')
            legend("topright", bty="n", inset=.005, legend=c(nl[1],nl[2]), fill=c('red','steelblue'), cex = 1.3, text.font=2)
            dev.off()
        }
    }
}

