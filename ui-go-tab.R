fluidPage(
      tabsetPanel(id = "goTabSetPanel",
## All genes ##########################################
## BP all #######################
        tabPanel( "All DE genes",          # pestaña All
            #tags$br(),
            tabsetPanel( id = "goAllTabSetPanel", # tabsetpanel all
                tabPanel("Biological Proccess",
                         fluidRow(# primera fila all
                             column(
                                 width = 3,
                                 box(
                                   width = 12,
                                   status = "info",
                                   h3("Voyage Voyage. Desireless"),
                                   p("Au dessus des vieux volcans"),
                                   p("Glissent des ailes sous les tapis du vent"),
                                   p("Voyage, voyage"),
                                   p("Éternellement,..."),
                                   p("De nuages en marécages")
                                 ) #tabbox
                             ),
                             column(
                                 width = 9,
                                 box(
                                     title = "Biological proccess terms",
                                     solidHeader = FALSE,
                                     status = "primary",
                                     width = NULL,
                                     DTOutput("tableBPall")
                                 ) # caja para la tabla
                             )), 
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( id = "boxPanelBP", width = 12,  height = "650px",# caja con pestañas para los plots
                            tabPanel(title = "GO term cloud", width = "100%", height = "600px" ,
                                     fluidRow(column(width=12, downloadButton("cloudbpall","Download SVG"))),
                                     fluidRow(column(width=12, plotOutput("cloudBPAll", height = "600px")))
                                     ),
                            tabPanel(title = "Barplot",
                                     fluidRow(column(
                                         width = 3,
                                         radioGroupButtons(
                                             inputId = "selectbpall",
                                             label = "Select bar plot type",
                                             choices = c("Dodge", "Stack", "Opposite"),
                                             selected = "Dodge",
                                             size = "sm",
                                             status = "primary",
                                             checkIcon = list(
                                                 yes = icon("ok",
                                                            lib = "glyphicon"),
                                                 no = icon("remove",
                                                           lib = "glyphicon")
                                             )
                                         )
                                     ),
                                     column(width=2, offset = 4,
                                                     downloadButton("barBpAll","Download SVG"))
                                     ), # fin fluidRow, column & radioGroupButtons
                                     fluidRow(class = "text-center",
                                              column(
                                                  align = "center",
                                                  plotlyOutput("plotBPall"),
                                                  width = 9
                                              ))
                                     ),  #barplot
                            tabPanel(title = "Dotplot",
                                     fluidRow(column(width=2,
                                            downloadButton("dotBpAll","Download SVG"))),
                                     plotOutput("BPDotall")
                                     ), # dotplot
                            tabPanel(title = "GoBarplot", value = "gobarplotallbp",
                                     fluidRow(column(width=2,
                                            downloadButton("gobarBpAll","Download SVG"))),
                                     plotOutput("gobarplotAllBP")
                                     ),
                            tabPanel(title = "GoCirclePlot", value = "gocirplotallbp",
                                     fluidRow(column(width=2,
                                            downloadButton("cirBpAll","Download SVG"))),
                                     plotOutput("goCircleAllBP")
                                     )
                        )
                    )
            )),
                ### MF all #############################
            #tags$br(),
            tabPanel("Molecular function",  # MF all
             fluidRow(  # primera fila mf all 
                column( width = 3,
                        box(
                          width = 12,
                          status = "info",
                          h3("Voyage Voyage. Desireless"),
                          p("Au dessus des vieux volcans"),
                          p("Glissent des ailes sous les tapis du vent"),
                          p("Voyage, voyage"),
                          p("Éternellement,..."),
                          p("De nuages en marécages")
                        ) #tabbox
                ),
                column( width = 9,
                    box(title = "Molecular function terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableMFall")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( id = "boxPanelMF", width = 12,  height = "650px",# caja con pestañas para los plots
                            tabPanel(title = "GO term cloud",width = "100%", height = "600px",
                                     fluidRow(downloadButton("cloudmfall","Download SVG")),
                                     fluidRow(plotOutput("cloudMFAll", height = "600px"))), 
                            tabPanel(title = "Barplot",
                                     fluidRow(column(
                                         width = 3,
                                         radioGroupButtons(
                                             inputId = "selectmfall",
                                             label = "Select bar plot type",
                                             choices = c("Dodge", "Stack", "Opposite"),
                                             selected = "Dodge",
                                             size = "sm",
                                             status = "primary",
                                             checkIcon = list(
                                                 yes = icon("ok",
                                                            lib = "glyphicon"),
                                                 no = icon("remove",
                                                           lib = "glyphicon")
                                             )
                                         )
                                     ),
                                     column(width=2, offset = 3,
                                                     downloadButton("barMfAll","Download SVG"))
                                     ), # fin fluidRow, column & radioGroupButtons
                                     fluidRow(class = "text-center",
                                              column(
                                                  align = "center",
                                                  plotlyOutput("plotMFall"),
                                                  width = 9
                                              ))
                                     ),  #barplot
                            tabPanel(title = "Dotplot",
                                     fluidRow(column(width=2,
                                            downloadButton("dotMfAll","Download SVG"))),
                                     plotOutput("MFDotall")
                                     ), # dotplot
                            tabPanel(title = "GoBarplot", value = "gobarplotallmf",
                                     fluidRow(column(width=2,
                                            downloadButton("gobarMfAll","Download SVG"))),
                                     plotOutput("gobarplotAllMF")
                                     ),
                            tabPanel(title = "GoCirclePlot", value = "gocirplotallmf",
                                     fluidRow(column(width=2,
                                            downloadButton("cirMfAll","Download SVG"))),
                                     plotOutput("goCircleAllMF")
                            )
                        )
                    )
            )),
                ### CC all #############################
                tabPanel("Cellular component",   ##tab CC all
                fluidRow(  # primera fila all CC
                    column( width = 3,
                            box(
                              width = 12,
                              status = "info",
                              h3("Voyage Voyage. Desireless"),
                              p("Au dessus des vieux volcans"),
                              p("Glissent des ailes sous les tapis du vent"),
                              p("Voyage, voyage"),
                              p("Éternellement,..."),
                              p("De nuages en marécages")
                            ) #tabbox
                    ),
                column( width = 9,
                    box(title = "Cellular component terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableCCall")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox(id = "boxPanelCC", width = 12, height = "650px", # caja con pestañas para los plots
                           tabPanel(title = "GO term cloud", width = "100%", height = "600px",
                                    fluidRow(downloadButton("cloudccall","Download SVG")),
                                    fluidRow( plotOutput("cloudCCAll", height = "600px") )
                                    ),
                            tabPanel(title = "Barplot",
                                     fluidRow(column(
                                         width = 3,
                                         radioGroupButtons(
                                             inputId = "selectccall",
                                             label = "Select bar plot type",
                                             choices = c("Dodge", "Stack", "Opposite"),
                                             selected = "Dodge",
                                             size = "sm",
                                             status = "primary",
                                             checkIcon = list(
                                                 yes = icon("ok",
                                                            lib = "glyphicon"),
                                                 no = icon("remove",
                                                           lib = "glyphicon")
                                             )
                                         )
                                     ),
                                     column(width=2, offset = 3,
                                                     downloadButton("barCcAll","Download SVG"))
                                     ), # fin fluidRow, column & radioGroupButtons
                                     fluidRow(class = "text-center",
                                              column(
                                                  align = "center",
                                                  plotlyOutput("plotCCall"),
                                                  width = 9
                                              ))
                                     ),  #barplot
                            tabPanel(title = "Dotplot",
                                     fluidRow(column(width=2,
                                            downloadButton("dotCcAll","Download SVG"))),
                                     plotOutput("CCDotall")
                                     ), # dotplot
                            tabPanel("GoBarplot", value = "gobarplotallcc", 
                                     fluidRow(column(width=2,
                                            downloadButton("gobarCcAll","Download SVG"))),
                                     plotOutput("gobarplotAllCC")
                                     ),
                            tabPanel( "GoCirclePlot", value="gocirplotallcc",
                                     fluidRow(column(width=2,
                                            downloadButton("cirCcAll","Download SVG"))),
                                     plotOutput("goCircleAllCC")
                            )
                        ) #tabbox
                    ) #column 
            ) #fluidrow
            ) # tabpanel CC 
            ) # tabsetpanel all
        ), #fin tab all genes
###### Upregulated genes #####################################
## BP up #############################
        tabPanel(value = "goUpTab", title = "Upregulated genes",          # pestaña upregulates
            tabsetPanel(  # tabsetpanel up
             tabPanel("Biological Proccess",
            fluidRow(  # primera fila
                column( width = 3,
                        box(
                          width = 12,
                          status = "info",
                          h3("It's my life. Bon Jovi"),
                          p("It's my life"),
                          p("It's now or never"),
                          p("I ain't gonna live forever"),
                          p("I just want to live while I'm alive"),
                          p("My heart is like an open highway...")
                        ) #tabbox
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
                    tabBox( width = 12,  height = "650px",# caja con pestañas para los plots
                            tabPanel(title = "GO term cloud", width = "100%", height = "600px",
                                     fluidRow(downloadButton("cloudbpup","Download SVG")),
                                     fluidRow( plotOutput("cloudBPUp", height = "600px") )
                                     ),
                        tabPanel(title = "Barplot",
                                 fluidRow(column(width=2,
                                            downloadButton("barBpUp","Download SVG"))),
                                  plotlyOutput("plotBP")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 fluidRow(column(width=2,
                                            downloadButton("dotBpUp","Download SVG"))),
                                 plotOutput("BPDotUp")
                                 ), # dotplot
                        tabPanel(title = "GoBarplot",
                                 fluidRow(column(width=2,
                                            downloadButton("gobarBpUp","Download SVG"))),
                                     plotOutput("gobarplotUpBP")
                                     ),
                        tabPanel(title = "GoCirclePlot",
                                 fluidRow(column(width=2,
                                            downloadButton("cirBpUp","Download SVG"))),
                                     plotOutput("goCircleUpBP")
                                     )
                        )
                    )
            )),
## MF up #############################
tabPanel("Molecular function",
fluidRow(  # primera fila
                column( width = 3,
                        box(
                          width = 12,
                          status = "info",
                          h3("It's my life. Bon Jovi"),
                          p("It's my life"),
                          p("It's now or never"),
                          p("I ain't gonna live forever"),
                          p("I just want to live while I'm alive"),
                          p("My heart is like an open highway...")
                        ) #tabbox
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
                    tabBox( width = 12,  height = "650px",# caja con pestañas para los plots
                        tabPanel(title = "GO term cloud", width = "100%", height = "600px" ,
                                 fluidRow(downloadButton("cloudmfup","Download SVG")),
                                 fluidRow( plotOutput("cloudMFUp", height = "600px") )
                                 ),
                        tabPanel(title = "Barplot",
                                 fluidRow(column(width=2,
                                            downloadButton("barMfUp","Download SVG"))),
                                  plotlyOutput("plotMF")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 fluidRow(column(width=2,
                                            downloadButton("dotMfUp","Download SVG"))),
                                 plotOutput("MFDotUp")
                                 ), # dotplot
                        tabPanel(title = "GoBarplot",
                                 fluidRow(column(width=2,
                                            downloadButton("gobarMfUp","Download SVG"))),
                                     plotOutput("gobarplotUpMF")
                                     ),
                        tabPanel(title = "GoCirclePlot",
                                 fluidRow(column(width=2,
                                            downloadButton("cirMfUp","Download SVG"))),
                                     plotOutput("goCircleUpMF")
                                     )
                        )
                    )
            )),
## CC up ###################################
        tabPanel("Cellular component",
            fluidRow(  # primera fila
                        column( width = 3,
                                box(
                                  width = 12,
                                  status = "info",
                                  h3("It's my life. Bon Jovi"),
                                  p("It's my life"),
                                  p("It's now or never"),
                                  p("I ain't gonna live forever"),
                                  p("I just want to live while I'm alive"),
                                  p("My heart is like an open highway...")
                                ) #tabbox
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
                    tabBox( width = 12, height = "650px", # caja con pestañas para los plots
                        tabPanel(title = "GO term cloud", width = "100%", height = "600px",
                                 fluidRow(downloadButton("cloudccup","Download SVG")),
                                 fluidRow( plotOutput("cloudCCUp", height = "600px") )
                                 ),
                        tabPanel(title = "Barplot",
                                 fluidRow(column(width=2,
                                            downloadButton("barCcUp","Download SVG"))),
                                  plotlyOutput("plotCC")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 fluidRow(column(width=2,
                                            downloadButton("dotCcUp","Download SVG"))),
                                 plotOutput("CCDotUp")
                                 ), # dotplot
                        tabPanel(title = "GoBarplot",
                                 fluidRow(column(width=2,
                                            downloadButton("gobarCcUp","Download SVG"))),
                                     plotOutput("gobarplotUpCC")
                                     ),
                        tabPanel(title = "GoCirclePlot",
                                 fluidRow(column(width=2,
                                            downloadButton("cirCcUp","Download SVG"))),
                                     plotOutput("goCircleUpCC")
                                     )
                            )
                        )
                    )
        ))), #fin tab upregulated genes
## Down regulated genes ############################################
## BP down ##############################
        tabPanel(value = "goDownTab", title = "Downregulated genes",  # pestaña downregulates
            tabsetPanel(
                tabPanel( "Biological proccess",
            fluidRow(  # primera fila
                column( width = 3,
                        box(
                          width = 12,
                          status = "info",
                          h3("I gotta feeling. Black eyed peas"),
                          p("I gotta feeling"),
                          p("That tonight's gonna be a good night"),
                          p("That tonight's gonna be a good night"),
                          p("I just want to live while I'm alive"),
                          p("That tonight's gonna be a good, good night...")
                        ) #tabbox
                ),
                column( width = 9,
                    box(title = "Biological proccess terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableBPdown")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, height = "650px", # caja con pestañas para los plots
                            tabPanel(title = "GO term cloud",  width = "100%", height = "600px",
                                     fluidRow(downloadButton("cloudbpdown","Download SVG")),
                                     fluidRow( plotOutput("cloudBPDown", height = "600px") )
                                     ),
                        tabPanel(title = "Barplot",
                                 fluidRow(column(width=2,
                                            downloadButton("barBpDown","Download SVG"))),
                                  plotlyOutput("plotBPdown")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 fluidRow(column(width=2,
                                            downloadButton("dotBpDown","Download SVG"))),
                                 plotOutput("BPDotDown")
                                 ), # dotplot
                        tabPanel(title = "GoBarplot",
                                 fluidRow(column(width=2,
                                            downloadButton("gobarBpDown","Download SVG"))),
                                     plotOutput("gobarplotDownBP")
                                     ),
                        tabPanel(title = "GoCirclePlot",
                                 fluidRow(column(width=2,
                                            downloadButton("cirBpDown","Download SVG"))),
                                     plotOutput("goCircleDownBP")
                                     )
                        )
                    )
            )),
## MF down ###################################
        tabPanel( "Molecular function",
            fluidRow(  # primera fila
                column( width = 3,
                        box(
                          width = 12,
                          status = "info",
                          h3("I gotta feeling. Black eyed peas"),
                          p("I gotta feeling"),
                          p("That tonight's gonna be a good night"),
                          p("That tonight's gonna be a good night"),
                          p("I just want to live while I'm alive"),
                          p("That tonight's gonna be a good, good night...")
                        ) #tabbox
                ),
                column( width = 9,
                    box(title = "Molecular functions terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableMFdown")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, height = "650px", # caja con pestañas para los plots
                        tabPanel(title = "GO term cloud", width = "100%", height = "600px",
                                 fluidRow(downloadButton("cloudmfdown","Download SVG")),
                                 fluidRow( plotOutput("cloudMFDown", height = "600px") )
                                 ),
                        tabPanel(title = "Barplot",
                                 fluidRow(column(width=2,
                                            downloadButton("barMfDown","Download SVG"))),
                                  plotlyOutput("plotMFdown")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 fluidRow(column(width=2,
                                            downloadButton("dotMfDown","Download SVG"))),
                                 plotOutput("MFDotDown")
                                 ), # dotplot
                        tabPanel(title = "GoBarplot",
                                 fluidRow(column(width=2,
                                            downloadButton("gobarMfDown","Download SVG"))),
                                     plotOutput("gobarplotDownMF")
                                     ),
                        tabPanel(title = "GoCirclePlot",
                                 fluidRow(column(width=2,
                                            downloadButton("cirMfDown","Download SVG"))),
                                     plotOutput("goCircleDownMF")
                                     )
                        )
                    )
            )),
## CC down ###################################
                tabPanel("Cellular component",
                fluidRow(  # primera fila
                column( width = 3,
                        box(
                          width = 12,
                          status = "info",
                          h3("I gotta feeling. Black eyed peas"),
                          p("I gotta feeling"),
                          p("That tonight's gonna be a good night"),
                          p("That tonight's gonna be a good night"),
                          p("I just want to live while I'm alive"),
                          p("That tonight's gonna be a good, good night...")
                        ) #tabbox
                ),
                column( width = 9,
                    box(title = "Cellular component terms",
                        solidHeader = FALSE,
                        status = "primary",
                        width = NULL,
                        DTOutput("tableCCdown")
                    ) # caja para la tabla
                    )
                ),
            fluidRow( # 2 fila
                column( width = 9, offset = 3,
                    tabBox( width = 12, height = "650px",# caja con pestañas para los plots
                        tabPanel(title = "GO term cloud",  width = "100%", height = "600px" ,
                                 fluidRow( downloadButton("cloudccdown","Download SVG")),
                                 fluidRow( plotOutput("cloudCCDown", height = "600px") )
                                 ),
                        tabPanel(title = "Barplot",
                                 fluidRow(column(width=2,
                                            downloadButton("barCcDown","Download SVG"))),
                                  plotlyOutput("plotCCdown")
                                              ),  #barplot
                        tabPanel(title = "Dotplot",
                                 fluidRow(column(width=2,
                                            downloadButton("dotCcDown","Download SVG"))),
                                 plotOutput("CCDotDown")
                                 ), # dotplot
                        tabPanel(title = "GoBarplot",
                                 fluidRow(column(width=2,
                                            downloadButton("gobarCcDown","Download SVG"))),
                                     plotOutput("gobarplotDownCC")
                                     ),
                        tabPanel(title = "GoCirclePlot",
                                 fluidRow(column(width=2,
                                            downloadButton("cirCcDown","Download SVG"))),
                                     plotOutput("goCircleDownCC")
                                     )
                        )
                    )
            ))
) # fin tabsetpanel downregulated
        ) #fin tab downrergulated genes
    ) # fin tabsetpanel
)#fin fluidpage