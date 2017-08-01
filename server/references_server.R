# Create the DataTable.
dt.references <- reactive({
  sub.param <- gsub("_.*", "", sel.param())
  final.df <- dbGetQuery(pool,
                         paste('select * from "references"',
                               'WHERE "parameters" LIKE',
                               paste0("'%", sub.param, "%'")))
  final.dt <- datatable(final.df)
  return(final.dt)
})

output$references_table <- DT::renderDataTable(dt.references()) # End output$reference_table



