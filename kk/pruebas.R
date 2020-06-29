


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

gosBP <- godown[godown$Ont=="BP",]
plotGO(enrichdf = gosBP[1:10, ], nrows = length(1:10), ont="BP",
           colors = "red")

enrichdf <- gosBP[1:10, ]; ont="BP"
    names(enrichdf) <- gsub("P.DE", "p-val", names(enrichdf) )
    names(enrichdf) <- gsub("DE", "DEG", names(enrichdf) )
    dataTitle <- list(BP=c("Biological Process", colors),
                      MF=c("Molecular Function", colors),
                      CC=c("Cellular Component", colors))
    enrichdf <- enrichdf[enrichdf$Ont == ont, ]
    if(!is.null(orderby)){
        orderby = match.arg(orderby, c("DEG", "p-val", "N", "Term"))
        if(orderby=="DEG" | orderby =="Term"){
            enrichdf <- enrichdf %>% arrange(get(orderby))
        } else{ enrichdf <- enrichdf %>% arrange(desc(get(orderby)))}
    }
    p <- enrichdf[1:10,] %>%
        plot_ly(x=~DEG, y=~go_id, text=~Term, type = "bar",
                marker = list(color=dataTitle[[ont]][2]),
                orientation = "v",
                hovertext = paste0(enrichdf$Term,"\np-val: ",format(enrichdf$`p-val`, scientific = T, digits = 4))) %>%
        layout(margin = list(l=100), yaxis = list(title=""),
               title=dataTitle[[ont]][1], xaxis = list(tickvals = c(1:max(enrichdf$DEG) ) ))
p

##############################
p <- enrichdf[1:10, ] %>%  ggplot( aes( y = DEG, x = go_id,
                        text = paste0("p-val: ",format(`p-val`, scientific = T, digits = 4)) )) +
        geom_bar(position = "stack", stat = "identity") + coord_flip() +
        theme(axis.text.y = element_text(angle = 0, hjust = 1)) + theme_bw() +
        scale_fill_manual(values = "red") +
        theme(panel.grid.major.y  = element_blank(),
              axis.title.y = element_blank())
    p <- p %>% ggplotly(tooltip = "all")
    