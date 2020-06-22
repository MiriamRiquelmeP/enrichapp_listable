fluidPage(
    tabsetPanel(
        tabPanel( "DEG genes",          # pestaña upregulates
            tags$br(),
            fluidRow(  # primera fila
                column( width = 3,
                    box(title = "Kegg description",
                        circleButton(
                            inputId = "information9",
                            icon = icon("info"),
                            size = "xs",
                            status = "primary"
                        ),
                        bsTooltip(
                            "information9",
                            paste0("Enter free text explaining the results obtained ",
                            " here or the data selected"),
                            trigger = "hover",
                            placement = "right"
                        ),
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL, 
                        textAreaInput(
                            "keggUpText",
                            label = "Kegg upregulated genes",
                            resize = NULL
                        )
                    ) # fin caja para el texto
                ),
                column( width = 9,
                    box(title = "Table of pathways",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("table")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( 
                    circleButton(
                        inputId = "information6",
                        icon = icon("info"),
                        size = "xs",
                        status = "primary"
                    ),
                    bsTooltip(
                        "information6",
                        paste0("Please, select rows with the pathways of interest ", 
                                "in the table to activate all the plots."),
                        trigger = "hover",
                        placement = "left"
                    ),
                    width = 9, offset = 3,
                    tabBox( width = 12, # caja con pestañas para los plots
                        tabPanel(title = "Barplot",
                                 plotlyOutput("keggPlot")
                                              ),  #barplot
                        tabPanel(title = "Chordplot",
                                 tagList(fluidRow(
                                     column(
                                         width = 8,
                                         chorddiagOutput("keggChord",
                                                         width = "100%",
                                                         height = "600px")
                                     ),
                                     column(
                                         width = 4,
                                         plotOutput("legendChorUp", 
                                                    width ="100%",
                                                    height ="600px")
                                     )
                                 ))
                                 ), #cordplot
                        tabPanel(title = "Dotplot",
                                 plotOutput("keggDotUp")
                                 ), # dotplot
                        tabPanel(title = "Heatmap",
                                 plotlyOutput("heatmapKeggUp", height = "600px")
                                 ), # heatmap
                        tabPanel(title = "Netplot",
                                 column(width = 1,
                                        switchInput(
                                            size = "mini",
                                            inputId = "keggNet_switch",
                                            offLabel = "Static",
                                            onLabel = "Interactive")
                                        ),
                                 column(width = 11,
                                        uiOutput("keggNet")
                                        )
                                  ) #visnetup
                        )
                    )
            )
        )
    ) # fin tabsetpanel
) #fin fluidpage    


