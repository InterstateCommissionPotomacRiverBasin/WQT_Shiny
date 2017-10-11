# Subset the data to only represent the selected Site and Parameter.
input.react <- reactive({
  # Prevent red error message from appearing while data is loading.
  req(param.tbl())
  sites <- param.tbl()

  final.df <- sites %>% 
    arrange(DATE)
  return(final.df)
}) # End input.react
#============================================================================= 
# Create the DataTable.
dt.react <- reactive({
  req(sel.param(), input.react())

  
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
  req(gage.tble())
  
  final.dt <- datatable(gage.tbl())
  
  return(final.dt)
})

output$param_table2 <- DT::renderDataTable(dt.react2()) # End output$param_table
