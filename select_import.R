#============================================================================
# Identify the currently selected parameter and site.
#============================================================================
sel.param <- reactive({
  if (input$query == "Site") final.vec <- input$PARAM.site
  if (input$query == "Parameter") final.vec <- input$PARAM.param
  return(final.vec)
})
#----------------------------------------------------------------------------
sel.site <- reactive({
  if (input$query == "Site") final.vec <- input$SITE.site
  if (input$query == "Parameter") final.vec <- input$SITE.param
  return(final.vec)
})
#----------------------------------------------------------------------------
sel.huc <- reactive({
  if (input$query == "Site") final.vec <- input$HUC_8.site
  if (input$query == "Parameter") final.vec <- input$HUC_8.param
  return(final.vec)
})
#==============================================================================
# Upload data from postgres for the selected site.
#==============================================================================
param.tbl <- reactive({
  #wqt <- dbReadTable(pool, paste0("SITE_", input$SITE.site))
  if (is.null(sel.site()) | sel.site() == "") return(NULL)
  if (is.null(sel.param())) return(NULL)
  wqt <- dbGetQuery(pool, paste(
    "SELECT * FROM",  paste0('"site_', sel.site(), '"'),
    'WHERE "ICPRB_NAME" =', paste0("'", sel.param(), "'")))
  if (is.null(wqt)) return(NULL)
  if ("ICPRB_VALUE" %in% names(wqt)) {
    final.df <- wqt[!is.na(wqt$ICPRB_VALUE), ]
  } else {
    final.df <- NULL
  }
  #final.df$DATE <- as.Date(final.df$DATE, "%Y-%m-%d")
  return(final.df)
}) # End param.tbl

#============================================================================
# Subset the data to only include the parameter of interest.
#============================================================================
#   output$tbl <- renderTable({
# Prevent red error message from appearing while data is loading.
#    if(is.null(param.tbl())) return(NULL)
#    final.df <- param.tbl()
#final.df <- final.df[final.df$PARAMETER %in% input$PARAM.site, ]
#    final.df <- final.df[final.df$ICPRB_NAME %in% input$PARAM.site, ]
#    return(final.df)
#  }) # End output$tbl
#============================================================================
# Update the list of parameters to reflect only the parameters observed at the
#selected site.
#============================================================================
param.react <- reactive({
  if (is.null(sel.site())) {
    final.vec <- NULL
  } else {
    final.vec <- unlist(dbGetQuery(pool, paste(
      'SELECT DISTINCT "ICPRB_NAME"',
      "FROM", paste0('"SITE_', sel.site(), '"'))))
  }
  
  return(final.vec)
}) # End param.react
#============================================================================
# Import the gage information related to the selected values.
#============================================================================
sel.gage <- reactive({
  if (is.null(param.tbl())){
    final.vec <- NULL
  } else {
    final.vec <- unique(param.tbl()$GAGE_ID)
  }
  return(final.vec)
})
#----------------------------------------------------------------------------
gage.info.react <- reactive({
  if (is.null(sel.gage())) return(NULL)
  final.df <- dbGetQuery(pool, paste(
    'SELECT * FROM "gage_info"',
    'WHERE "GAGE_ID" =', paste0("'", sel.gage(), "'")))
  if (nrow(final.df) == 0) final.df <- NULL
  return(final.df)
}) # End param.react