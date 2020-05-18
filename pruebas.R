formatData <- function(genelist, specie, annotation){
  require("EnsDb.Mmusculus.v79")
  require("org.Mm.eg.db")
  require("EnsDb.Hsapiens.v86")
  require("org.Hs.eg.db")
  require("EnsDb.Rnorvegicus.v79")
  require("org.Rn.eg.db")

  if(annotation=="ensg"){ann="ENSEMBL"} else{ann="SYMBOL"}
  ## listado simple
  if( dim(genelist)[2]==1 ){
    names(genelist)<-ann
  }
  ## listado doble
  if( dim(genelist)[2]==2){
    names(genelist) <- c(ann, "Rank")
  }

  if(specie=="Mm"){
      ensdb <- EnsDb.Mmusculus.v79
      orgdb <- org.Mm.eg.db
  }    else{
        ensdb <- EnsDb.Hsapiens.v86
        orgdb <- org.Hs.eg.db
    }
  genelist[[ann]] <- toupper(genelist[[ann]]) # todo a mayusculas
  if( ann=="SYMBOL" & (specie == "Mm" | specie == "Rn") ){ # si es symbol y mouse o rattus a capital
    genelist[[ann]] <- stringr::str_to_title(genelist[[ann]])
    }
  annot <- data.frame()
  genes <- genelist[[ann]]
  if(ann=="ENSEMBL"){
    annot <- data.frame(ENSEMBL = genes)
    annot$SYMBOL <-  mapIds(ensdb, keys=genes, column="SYMBOL",keytype="GENEID")
    annot$SYMBOL1 <- mapIds(orgdb, keys = genes, column = 'SYMBOL', keytype = 'ENSEMBL', multiVals = 'first')
    annot$description <- mapIds(orgdb, keys = genes, column = 'GENENAME', keytype = 'ENSEMBL', multiVals = 'first')
    annot <- as.data.frame(annot)
    consensus <- data.frame('Symbol'= ifelse(!is.na(annot$SYMBOL), as.vector(annot$SYMBOL),
                                             ifelse(!is.na(annot$SYMBOL1),as.vector(annot$SYMBOL1),
                                                    as.vector(annot$ENSEMBL))), stringsAsFactors = F)
    annot$consensus <- consensus$Symbol
    entrez1 <- mapIds(orgdb, keys = annot$consensus, column = "ENTREZID", keytype = "SYMBOL")
    entrez2 <- mapIds(orgdb, keys = as.character(annot$ENSEMBL),
                      column = "ENTREZID", keytype = "ENSEMBL")
    annot$entrez1 <- entrez1
    annot$entrez2 <- unlist(unname(lapply(entrez2, function(x){ifelse(is.null(x), NA, x)})))
    ENTREZID <- ifelse(!is.na(annot$entrez1), annot$entrez1, annot$entrez2)
    annot$ENTREZID <- ENTREZID
    annot <- annot[c("ENSEMBL","consensus","ENTREZID")]
    names(annot) <- c("ENSEMBL", "SYMBOL", "ENTREZID")
  }
  if(ann=="SYMBOL"){
    annot <- data.frame(SYMBOL = genes, stringsAsFactors = FALSE)
    annot$ENSEMBL <-  mapIds(orgdb, keys=genes, column="ENSEMBL",keytype="SYMBOL")
    entrez1 <- mapIds(orgdb, keys = annot$SYMBOL, column = "ENTREZID", keytype = "SYMBOL")
    entrez2 <- mapIds(orgdb, keys = as.character(annot$ENSEMBL),
                      column = "ENTREZID", keytype = "ENSEMBL")
    annot$entrez1 <- entrez1
    annot$entrez2 <- unlist(unname(lapply(entrez2, function(x){ifelse(is.null(x), NA, x)})))
    ENTREZID <- ifelse(!is.na(annot$entrez1), annot$entrez1, annot$entrez2)
    annot$ENTREZID <- ENTREZID
    annot <- annot[c("ENSEMBL","SYMBOL","ENTREZID")]
  }
  if(dim(genelist)[2]==2){
      annot$rank <- genelist[ ,2]
  }
  return(annot)
}

source("utils.R")
res <- readRDS("../darkEnrichApp/res.Rds")
# resE <- head(rownames(res), 50)
# resE <- as.data.frame(resE)
# kk<-formatData(genelist = resE, specie = "Mm", annotation = "ensg")

resy <- readxl::read_xlsx("listgenes.xlsx", sheet = 1, col_names = FALSE)

resy <- data.frame(as.character(res$GeneName_Symbol[1:1000]), rank = res$log2FoldChange[1:1000])
genelist = as.data.frame(resy); specie = "Mm"; annotation = "symbol"
kk2 <- formatData(genelist = resy, specie = "Mm", annotation = "symbol")
                lost <- which(is.na(kk2$ENTREZID))
                genelost <- kk2$SYMBOL[lost]
                kk2 <- kk2[-lost, ]

resen <- data.frame(rownames(res)[1:100])
genelist = resen; specie = "Mm"; annotation = "ensg"
kk3 <- formatData(genelist = resen, specie = "Mm", annotation = "ensg")
kk3



#symbol
lost2 <- which(is.na(kk2$ENTREZID))
recup2 <- kk2[-lost2, ]
#ensembl
lost3 <- which(is.na(kk3$ENTREZID))
recup3 <- kk3[-lost3, ]
recup3$SYMBOL[1] <- NA
recup3$SYMBOL <- ifelse(is.na(recup3$SYMBOL), recup3$ENSEMBL, recup3$SYMBOL )


