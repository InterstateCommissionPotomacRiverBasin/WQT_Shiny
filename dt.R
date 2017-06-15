# Subset the data to only represent the selected Site and Parameter.
input.react <- reactive({
  # Prevent red error message from appearing while data is loading.
  if(is.null(param.tbl())) return(NULL)
  sites <- param.tbl()
  #final.df <- wqt[wqt$SITE %in% input$SITE.site, ]
  #final.df <- sites[sites$SITE %in% input$SITE.site, ]
  #final.df <- unique(final.df[final.df$PARAMETER %in% input$PARAM.site, ])
  #final.df <- unique(final.df[final.df$ICPRB_NAME %in% input$PARAM.site, ])
  final.df <- sites[order(sites$DATE), ]
  return(final.df)
}) # End input.react
#============================================================================= 
# Create the DataTable.
dt.react <- reactive({
  if (is.null(sel.param())) return(NULL)
  # Prevent red error message from appearing while data is loading.
  if(is.null(input.react())) return(NULL)
  
  final.dt <- datatable(input.react(),
                        options = list(
                          scrollX = 2000, 
                          scrollY = 700,
                          autoWidth = TRUE,
                          columnDefs = list(list(width = '300px',
                                                 targets = c(4, 65)),
                                            list(className = 'dt-center',
                                                 targets = 1:ncol(input.react()))),
                          pageLength = 25,
                          color = "black")) %>%
    formatDate(columns = "DATE", method = 'toLocaleDateString')
  
  return(final.dt)
})
#---------------------------------------------------------------------------- 
# Render a table representing the selected Site and Parameter.
output$param_table <- DT::renderDataTable(dt.react()) # End output$param_table

#============================================================================= 
# Create the DataTable.
dt.react2 <- reactive({
  #if (is.null(sel.param())) return(NULL)
  # Prevent red error message from appearing while data is loading.
  #if(is.null(gage.tbl())) return(NULL)
  
  final.dt <- datatable(gage.tbl())
  
  return(final.dt)
})

output$param_table2 <- DT::renderDataTable(dt.react2()) # End output$param_table
