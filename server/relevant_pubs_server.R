# Create the DataTable.
dt.references <- reactive({
  sub.param <- gsub("_.*", "", sel.param())
  final.df <- dbGetQuery(pool,
                         paste('select * from "literature_references"',
                               'WHERE "Parameter(s)" LIKE',
                               paste0("'%", sub.param, "%'")))
  final.dt <- datatable(final.df,
                        options = list(
                          scrollX = 2000, 
                          scrollY = 700,
                          autoWidth = TRUE,
                          pageLength = 25,
                          color = "black"))
  return(final.dt)
})

output$relevant_pubs_table <- DT::renderDataTable(dt.references()) # End output$reference_table



