fluidPage(
    fluidRow(infoBoxOutput("allbox", width = 4),
             infoBoxOutput("downbox", width = 4),
             infoBoxOutput("upbox", width = 4)),
    fluidRow(column(width = 2, offset = 3,
                 uiOutput("fcdown")),
             column(width=2,
                 uiOutput("fcup")),
             column(width = 2,
                 uiOutput("pval"))
             ),
    # fin fluidrow boxinfos
    tags$br(),
    fluidRow(
        column(
            width = 4,
            offset = 1, 
            circleButton(
                inputId = "information1",
                icon = icon("info"),
                size = "xs",
                status = "primary"
            ),
            bsTooltip(
                    "information1",
                    paste0("Customize here the statistical values that you intend",
                           " to apply to your experiment as a cutoff to consider a",
                           " gene differentially expressed. The numbers per",
                           " category can be checked above. Click on Apply values",
                           " once you have finished exploring this tab and",
                           " before moving to the next one."),
                    trigger = "hover",
                    placement = "right"
                ),
        box(
            align = 'center',
            title = "Cutoff values",
            solidHeader = FALSE,
            status = "primary",
            width = NULL,
            switchInput(inputId = "fc_switch",
                        offLabel = "log2FC",
                        onLabel = "FC",
                        size = "mini"
                        ),
            tags$br(),
            uiOutput("fc_control"),
            tags$br(),
            uiOutput("padj"),
            tags$br(),
            actionButton("applyParam", label = "Click to apply values")
        )
    ),
    column(
        width = 5,
        offset = 1,
        tabBox(
            width = 12,
            # title = "Biological context",
            # solidHeader = FALSE,
            # status = "primary",
            # width = 12,
            tabPanel(
                title = "Biological Context",
                circleButton(
                    inputId = "info1",
                    icon = icon("info"),
                    size = "xs",
                    status = "primary"
                ),
                bsTooltip(
                    "info1",
                    "Enter free text explaining biological experiment.",
                    trigger = "hover",
                    placement = "right"
                ),
                textAreaInput(
                    "biologicalText",
                    label = NULL,
                    resize = NULL
                    #width = "100%",
                    #height = "258px"
                )
            ),
            # tabpanel
            tabPanel(
                title = "Experiment results / Statistics",
                circleButton(
                  inputId = "info2",
                  icon = icon("info"),
                  size = "xs",
                  status = "primary"
                ),
                bsTooltip(
                  "info2",
                  paste0("Enter free text explaining the results obtained here",
                  " or the statistical value selected."),
                  trigger = "hover",
                  placement = "right"
                ),
                textAreaInput(
                    "explainPreview",
                    label = NULL,
                    resize = NULL
                    #width = "100%",
                    #height = "258px"
                ) #texarea
            ) #tabpanel
        ) #tabbox
    ) #column
    ), 


  fluidRow(
      column(width = 3,
             box( title = "Customize plots",
                  width = NULL,
                tagList(
                    tags$p("Volcano plots color scheme"),
             #    spectrumInput(
             #     inputId = "logfcColor",
             #     label = "Pick log FC color:",
             #     selected = '#2ca25f',
             #     width = "60%",
             #     choices = list(
             #         list("#b30000","#e34a33","#fc8d59","#fdbb84","#fdd49e","#fef0d9"),
             #         list('#045a8d','#2b8cbe','#74a9cf','#a6bddb','#d0d1e6','#f1eef6'),
             #         list('#006d2c', '#2ca25f', '#66c2a4', '#99d8c9', '#ccece6','#edf8fb'),
             #         list('#252525', '#636363', '#969696', '#bdbdbd', '#d9d9d9', '#f7f7f7')
             #     ),
             #     options = list(`toggle-palette-more-text` = "Show more")
             # ),
             #              spectrumInput(
             #     inputId = "padjColor",
             #     label = "Pick p-val color:",
             #     selected = '#2b8cbe',
             #     width = "60%",
             #     choices = list(
             #         list("#b30000","#e34a33","#fc8d59","#fdbb84","#fdd49e","#fef0d9"),
             #         list('#045a8d','#2b8cbe','#74a9cf','#a6bddb','#d0d1e6','#f1eef6'),
             #         list('#006d2c', '#2ca25f', '#66c2a4', '#99d8c9', '#ccece6','#edf8fb'),
             #         list('#252525', '#636363', '#969696', '#bdbdbd', '#d9d9d9', '#f7f7f7')
             #     ),
             #     options = list(`toggle-palette-more-text` = "Show more")
             # ),
             #              spectrumInput(
             #     inputId = "bothColor",
             #     label = "Pick logFC & p-pval color:",
             #     selected = "#e34a33",
             #     width = "60%",
             #     choices = list(
             #         list("#b30000","#e34a33","#fc8d59","#fdbb84","#fdd49e","#fef0d9"),
             #         list('#045a8d','#2b8cbe','#74a9cf','#a6bddb','#d0d1e6','#f1eef6'),
             #         list('#006d2c', '#2ca25f', '#66c2a4', '#99d8c9', '#ccece6','#edf8fb'),
             #         list('#252525', '#636363', '#969696', '#bdbdbd', '#d9d9d9', '#f7f7f7')
             #     ),
             #     options = list(`toggle-palette-more-text` = "Show more")
             # ),
             #              spectrumInput(
             #     inputId = "nsColor",
             #     label = "Pick no significant color:",
             #     selected = '#969696',
             #     width = "60%",
             #     choices = list(
             #         list("#b30000","#e34a33","#fc8d59","#fdbb84","#fdd49e","#fef0d9"),
             #         list('#045a8d','#2b8cbe','#74a9cf','#a6bddb','#d0d1e6','#f1eef6'),
             #         list('#006d2c', '#2ca25f', '#66c2a4', '#99d8c9', '#ccece6','#edf8fb'),
             #         list('#252525', '#636363', '#969696', '#bdbdbd', '#d9d9d9', '#f7f7f7')
             #     ),
             #     options = list(`toggle-palette-more-text` = "Show more")
             # ),
             spectrumInput(
                 inputId = "upColor",
                 label = "Pick upregulated color:",
                 selected = "#b30000",
                 width = "60%",
                 choices = list(
                     list("#b30000","#e34a33","#fc8d59","#fdbb84","#fdd49e","#fef0d9"),
                     list('#045a8d','#2b8cbe','#74a9cf','#a6bddb','#d0d1e6','#f1eef6'),
                     list('#006d2c', '#2ca25f', '#66c2a4', '#99d8c9', '#ccece6','#edf8fb'),
                     list('#252525', '#636363', '#969696', '#bdbdbd', '#d9d9d9', '#f7f7f7')
                 ),
                 options = list(`toggle-palette-more-text` = "Show more")
             ),
             spectrumInput(
                 inputId = "downColor",
                 label = "Pick downregulated color:",
                 selected = '#045a8d',
                 width = "60%",
                 choices = list(
                     list("#b30000","#e34a33","#fc8d59","#fdbb84","#fdd49e","#fef0d9"),
                     list('#045a8d','#2b8cbe','#74a9cf','#a6bddb','#d0d1e6','#f1eef6'),
                     list('#006d2c', '#2ca25f', '#66c2a4', '#99d8c9', '#ccece6','#edf8fb'),
                     list('#252525', '#636363', '#969696', '#bdbdbd', '#d9d9d9', '#f7f7f7')
                 ),
                 options = list(`toggle-palette-more-text` = "Show more")
             ),
             uiOutput("geneSelector")
             )
             )
  ),
      column(
     width = 9,
     tabBox(
         width = 12,
         title = "",
         tabPanel(
                title = "Volcano plot",
                plotOutput("volcano", click = "plot_click1" , width = "100%", height = "600px"),
                tableOutput("texto1")
            ),
         tabPanel(title = "KaryoPlot",
                                          circleButton(
                                            inputId = "karyoInfo",
                                            icon = icon("info"),
                                            size = "xs",
                                            status = "primary"
                                          ),
                                          bsTooltip(
                                            "karyoInfo",
                                            paste0("Pon lo que te salga del ornitorrinco "),
                                            trigger = "hover",
                                            placement = "right"
                                          ),
                                          tagList(
                                            fluidRow(
                                              column(width=10, offset=1,
                                                     plotOutput("karyoPlot", height = "800px") )))
                                          )
 ))),
 fluidRow(column(width = 4, offset = 4,
            strong("Click to compute enrichment"),
            tags$br(),
            actionButton("enrichButton", "Apply values", width = "100%")
             ))
) # fin page



