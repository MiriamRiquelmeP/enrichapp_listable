


validatedGene <- as.data.frame( readxl::read_xlsx("ens3cols.xlsx", sheet = 1))
data <- formatData( validatedGene, "Mm", "ensg" )
lost <- which(is.na(data$ENTREZID))
gene <- data$ENSEMBL[lost]
if(length(lost)!=0){ data <- data[-lost, ] }
data$SYMBOL <- ifelse(is.na(data$SYMBOL), data$ENSEMBL, data$SYMBOL )

genesUp <- data[data$logFC >= 0.5 & data$pval <= 0.05,
                          c("SYMBOL","ENTREZID")]
      
genesDown <- data[data$logFC <= -0.5 & data$pval <=0.05,
                          c("SYMBOL","ENTREZID")]
      
genesall <- rbind(genesUp, genesDown)
goup <- customGO(genesUp, species = "Mm")
godown <- customGO(genesDown, species = "Mm")
goall <- customGO(genesall, species = "Mm")

goBarplot(enrichGO = goall, resGO = data, genes= genesall,
              category = "BP", nrows = 1:10)
goBarplot(enrichGO = goup, resGO = data, genes= genesUp,
              category = "BP", nrows = )
goBarplot(enrichGO = godown, resGO = data, genes= genesDown,
              category = "BP", nrows = 1:10)   
    
goBarplot <- function(enrichGO=NULL, resGO=NULL, genes=NULL,
                      category=NULL, nrows=NULL ){
    require(GOplot)
    go <- enrichGO
    go <- go[ go$Ont==category, ]
    if(is.null(nrows) | length(nrows)<2 ){
        totalRows <- min(90, dim(go)[1] )
        go <- go[ seq_len(totalRows), ]
    } else{
        go <- go[nrows, ]
        }
    res <- resGO
    go2 <- go %>% group_by(Ont) %>% as.data.frame() 
    goDT <- go2DT(go2, genes)
    # preparar tabla GO
    go2$genes <- goDT$genes
    go2 <- go2 %>% dplyr::select(Ont,go_id,Term,genes, P.DE)
    names(go2) <- c("Category","ID", "Term", "Genes", "adj_pval")
    #preparar tabla genelist
    #names(res)
    res2 <- res %>% dplyr::select(SYMBOL, logFC, pval)
    names(res2) <- c("ID","logFC","adj.P.Val")
    # crear objeto circ
    circ <- circle_dat(go2, res2)
    GOBar(circ)
}
