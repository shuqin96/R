
## ----------------------------------------------------------------------------------------------------
## Using BioMart convert different database accession number. 
## You can also do this in http://grch37.ensembl.org/biomart/martview/0fd547b7e56dc315bd6e0c030eb44fa5  
## ----------------------------------------------------------------------------------------------------

rm(list=ls())

# Install the library if you don't have it installed
if (!requireNamespace("BiocManager", quietly = TRUE))
install.packages("BiocManager")
# Load library
BiocManager::install("biomaRt")
library("biomaRt")

## View dataset in BioMart
listMarts()
# results:
#                biomart               version
# 1 ENSEMBL_MART_ENSEMBL      Ensembl Genes 97
# 2   ENSEMBL_MART_MOUSE      Mouse strains 97
# 3     ENSEMBL_MART_SNP  Ensembl Variation 97
# 4 ENSEMBL_MART_FUNCGEN Ensembl Regulation 97

# Use the useMart function to select the database
mart <- useMart("ENSEMBL_MART_ENSEMBL")
# Use the listDatasets() function to display the genome annotations contained in the current database.
listDatasets(mart)

## Use the useDataseq() function to select the genome in the database
ensembl=useDataset("hsapiens_gene_ensembl", useMart("ENSEMBL_MART_ENSEMBL"))

a = listAttributes(ensembl)  # Corresponding to the attributes parameter in the following command line
b = listFilters(ensembl)  # Corresponding to the filter parameter in the following command line

## EXAMPLE
# 1.convert uniprot_swissprot_accession to ensembl_transcript_id(ENST)
result1 <- getBM(mart = ensembl, filters = "uniprotswissprot", values = 'Q13068', attributes = c("uniprotswissprot","ensembl_transcript_id"))
# 2.convert gene to ensembl_transcript_id
result2 <- getBM(mart = ensembl, filters = "hgnc_symbol", values = 'TP53', attributes = c("hgnc_symbol","ensembl_transcript_id"))
# 3.get the nucleotide of the specified site
result3 <- getBM(mart = ensembl, filters = c("chromosome_name",'start','end'), values = list(chromosome = '1', start = '183196716', end='183196716'), attributes = c("ensembl_gene_id","ensembl_transcript_id","allele"))
