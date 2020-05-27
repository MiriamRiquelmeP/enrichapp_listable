fluidPage(
      tabsetPanel(
###### Upregulated genes #####################################
## BP up #############################
        tabPanel( "DEG genes",          # pestaña upregulates
            tabsetPanel(  # tabsetpanel up
             tabPanel("Biological Proccess",
            fluidRow(  # primera fila
                column( width = 3,
                    box(title = "GO BP description",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL, 
                        textAreaInput(
                            "GOBpUpText",
                            label = "",
                            resize = NULL
                        )
                    ) # fin caja para el texto
                ),
                column( width = 9,
                    box(title = "Biological proccess terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableBP")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                  plotlyOutput("plotBP")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 plotlyOutput("BPDotUp")
                                 ) # dotplot
                        )
                    )
            )),
## MF up #############################
tabPanel("Molecular function",
fluidRow(  # primera fila
                column( width = 3,
                    box(title = "GO MF description",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL, 
                        textAreaInput(
                            "GOMfUpText",
                            label = "",
                            resize = NULL
                        )
                    ) # fin caja para el texto
                ),
                column( width = 9,
                    box(title = "Molecular function terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableMF")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                  plotlyOutput("plotMF")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 plotlyOutput("MFDotUp")
                                 ) # dotplot
                        )
                    )
            )),
## CC up ###################################
        tabPanel("Cellular component",
            fluidRow(  # primera fila
                        column( width = 3,
                            box(title = "GO CC description",
                                solidHeader = FALSE,
                                status = "primary",
                                width = NULL, 
                                textAreaInput(
                                    "GOCcUpText",
                                    label = "",
                                    resize = NULL
                                )
                            ) # fin caja para el texto
                        ),
                        column( width = 9,
                            box(title = "Cellular component terms",
                                solidHeader = FALSE,
                                status = "primary",
                                width = NULL,
                                DTOutput("tableCC")
                            ) # caja para la tabla
                            )
                        ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                  plotlyOutput("plotCC")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 plotlyOutput("CCDotUp")
                                 ) # dotplot
                            )
                        )
                    )
        ))) #fin tab upregulated genes
    ) # fin tabsetpanel
)#fin fluidpage