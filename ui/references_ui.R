tabPanel("References",
         #tableOutput('tbl'))
         #column(11, dataTableOutput('param_table')),
         #column(1, p(class = 'text-center', downloadButton('param.tbl.download', 'Download Table')))
         fluidRow(dataTableOutput('references_table'))
) # End tabPanel
