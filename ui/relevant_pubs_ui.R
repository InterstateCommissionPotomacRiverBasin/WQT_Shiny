tabPanel("Relevant Publications",
         #tableOutput('tbl'))
         #column(11, dataTableOutput('param_table')),
         #column(1, p(class = 'text-center', downloadButton('param.tbl.download', 'Download Table')))
         fluidRow(dataTableOutput('relevant_pubs_table'))
) # End tabPanel
