res <- readRDS("res.Rds")
res <- res %>% rownames_to_column(var = "ENSEMBL")
res <- res %>% select(ENSEMBL, log2FoldChange, padj)
data <- res
validatedGene <- as.data.frame( readxl::read_xlsx("ens3cols.xlsx", sheet = 1))
data <- formatData( validatedGene, "Mm", "ensg" )
lost <- which(is.na(data$ENTREZID))
gene <- data$ENSEMBL[lost]
if(length(lost)!=0){ data <- data[-lost, ] }
data$SYMBOL <- ifelse(is.na(data$SYMBOL), data$ENSEMBL, data$SYMBOL )
fcsign <- sign(data$logFC)
logP <- -log10(data$pval)
data$rank <- logP/fcsign
data <- data %>% arrange(desc(logFC))
data <- data %>% filter(!is.infinite(rank))
gsea <- gseaKegg(data[, c("ENTREZID","logFC")], "Mm" )


write.csv(res, "dataGSEA.csv")
