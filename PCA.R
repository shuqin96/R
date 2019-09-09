
library(ggplot2)

methodlist <- c('CADD_phred','CHASMplus','CanDrA_Score','CONDEL','DANN_score','Eigen.raw','integrated_fitCons_score','FATHMM_score','fathmm.MKL_coding_score',
	'FATHMM_Cancer_score','GERP.._RS','GenoCanyon_score','LRT_score_new','M.CAP_score','MetaSVM_score','MetaLR_score','MutPred_score','MutationTaster_score_new',
	'MutationAssessor_score','phyloP100way_vertebrate','phastCons100way_vertebrate','PROVEAN_score','ParsSNP','Polyphen2_HDIV_score','Polyphen2_HVAR_score',
	'REVEL_score','SIFT_score','SiPhy_29way_logOdds','VEST3_score')
method.map <- c('integrated_fitCons_score'='fitCons','SIFT_score'='SIFT','Polyphen2_HDIV_score'='PolyPhen2-HDIV','Polyphen2_HVAR_score'='PolyPhen2-HVAR',
	'LRT_score_new'='LRT','MutationAssessor_score'='MutationAssessor','PROVEAN_score'='PROVEAN','VEST3_score'='VEST3','MetaSVM_score'='MetaSVM',
	'MetaLR_score'='MetaLR','M.CAP_score'='M-CAP','REVEL_score'='REVEL','MutPred_score'='MutPred','CADD_phred'='CADD','DANN_score'='DANN',
	'fathmm.MKL_coding_score'='FATHMM-MKL','Eigen.raw'='Eigen','GERP.._RS'='GERP++','phyloP100way_vertebrate'='phyloP','phastCons100way_vertebrate'='phastCons',
	'SiPhy_29way_logOdds'='SiPhy','FATHMM_score'='FATHMM','FATHMM_Cancer_score'='FATHMM-Cancer','GenoCanyon_score'='GenoCanyon','CanDrA_Score'='CanDrAplus', 
	'MutationTaster_score_new'='MutationTaster','CHASMplus'='CHASMplus','ParsSNP'='ParsSNP','CONDEL'='CONDEL')

each_data_pca <- function() {
	datalist <- c('hpm','fmd','fmn','exac','cancer')
	namelist <- c('HPM','FDM','FNM','ExAC','Cancer')
	for (i in 1:length(datalist)) {
		data <- datalist[i]
		filename <- paste(pathoutput,"pca/",data,"_pca.png",sep="")
		f <- eval(parse(text=data))
		fm <- f[,methodlist]
		fm = rename(fm, method.map)
		df <- t(fm)
		prcomp <- prcomp(df, center = TRUE, scale = TRUE) # 对数据进行标准化
		f.pca <- data.frame(cbind (df, prcomp$x))
		f.plot <- ggplot (f.pca, aes (x = PC1, y = PC2)) + geom_point(size=0.3) + geom_text_repel (aes (label = row.names (f.pca)), max.iter = 50000, size = 3, segment.alpha = 0.2) + 
			theme (axis.line.x = element_line (color = "black"), axis.line.y = element_line (color = "black"), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none", panel.background = element_blank()) + 
			ggtitle (namelist[i]) + theme (plot.title = element_text (face = "bold", hjust = 0.5)) + 
			xlim (min (f.pca$PC1) - 1, max (f.pca$PC1) + 10) + ylim (min (f.pca$PC2) - 1, max (f.pca$PC2) + 1)
		ggsave(f.plot, filename=filename, width=15, height=13, units=c("cm"))
	}
}


## 根据不同方法进行分类
each_data_type_pca <- function() {
	typelist <- c('ensemble','cancer-related','cancer-related','ensemble','ensemble','ensemble','function','function','ensemble',
		'cancer-related','conservation','ensemble','function','ensemble','ensemble','ensemble','function','function','function',
		'conservation','conservation','function','cancer-related','function','function','ensemble','function','conservation','function')
	datalist <- c('hpm','fmd','fmn','exac','cancer')
	namelist <- c('HPM','FDM','FNM','ExAC','Cancer')
	for (i in 1:length(datalist)) {
		data <- datalist[i]
		filename <- paste(pathoutput,"pca/",data,"_type_pca.png",sep="")
		f <- eval(parse(text=data))
		fm <- f[,methodlist]
		fm <- rename(fm, method.map)
		df <- data.frame(t(fm))
		df$type <- typelist  # add type of methods
		df.nd <- df[,1:(ncol(df)-1)]  # select methods columns
		prcomp <- prcomp(df.nd, center = TRUE, scale = TRUE) # 对数据进行标准化
		f.pca <- data.frame(cbind (df, prcomp$x))
		f.plot <- ggplot (f.pca, aes (x = PC1, y = PC2)) + geom_point(aes (color = type),size=0.3) + 
			geom_text_repel (aes (label = row.names (f.pca), color = type), max.iter = 50000, size = 3, segment.alpha = 0.2) + 
			theme (axis.line.x = element_line (color = "black"), axis.line.y = element_line (color = "black"), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none", panel.background = element_blank()) + 
			ggtitle (namelist[i]) + theme (plot.title = element_text (face = "bold", hjust = 0.5)) + 
			xlim (min (f.pca$PC1) - 1, max (f.pca$PC1) + 10) + ylim (min (f.pca$PC2) - 1, max (f.pca$PC2) + 1) +
			scale_color_manual (values = c ("red", "orange", "#598234", "#4cb5f5"))  # 不同类型的颜色设置
		ggsave(f.plot, filename=filename, width=15, height=13, units=c("cm"))
	}
}
