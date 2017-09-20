tabPanel("Figures",
         fluidRow(
           align = "center",
           plotOutput("PLOTS", height = "1200px", width = "100%")
           #fluidRow(uiOutput("plot.ui"))
           #fluidRow(plotOutput("PLOTS", height = uiOutput("plot.height"), width = "1500"))
         ) # End fluidRow
) # End tabPanel