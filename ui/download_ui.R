tabPanel("Download",
         div(fluidRow(p(selectInput("huc8.download", "HUC 8",
                                    choices = sort(unique(param.range$ICPRB_NAME)),
                                    selectize = TRUE, selected = "All HuCs"))),
             style = "text-align: center;"),
         div(fluidRow(p(selectInput("site.download", "Site", choices = list.huc,
                                    selected = "All Sites", selectize = TRUE))),
             style = "text-align: center;"),
         div(fluidRow(p(selectInput("param.download", "Parameter",
                                    choices = unique(param.range$ICPRB_NAME),
                                    selectize = TRUE))),
             style = "text-align: center;")
) # End tabPanel