prep.react <- reactive({
  # Prevent red error message from appearing while data is loading.
  if(is.null(param.tbl())) return(NULL)
  # Prep data when data is available
  final.df <- prep_plot(param.tbl(), sel.site(), sel.param(), outliers)
  # Remove NAs from the REPORTING_VALUE column.
  # final.df <- final.df[complete.cases(final.df$REPORTED_VALUE), ]
  # End prep.react reactive function.
  return(final.df)
}) # End prep.react
#----------------------------------------------------------------------------
output$ICPRB_UNITS <- renderUI({
  # Prevent red error message from appearing while data is loading.
  if(is.null(prep.react())) return(NULL)
  ICPRB_UNITS <- unique(na.omit(prep.react()$ICPRB_UNITS))
  HTML(paste("<strong>Units:</strong>", ICPRB_UNITS, sep = " "))
}) # End output$ICPRB_UNITS
#----------------------------------------------------------------------------
plotInput <- reactive({
  # Prevent red error message from appearing while data is loading.
  if(is.null(prep.react())) return(NULL)
  prep.df <- prep.react()
  tile.plot <- tile_plot(prep.df, param.range)
  raw.loess.plot <- raw_loess_plot(prep.df)
  flow.correct.loess.plot <- flow_correct_loess_plot(prep.df)
  grid::grid.newpage()
  grid::grid.draw(rbind(ggplotGrob(tile.plot),
                        ggplotGrob(raw.loess.plot),
                        ggplotGrob(flow.correct.loess.plot),
                        size = "last"))
  
}) # End plotInput
#----------------------------------------------------------------------------
# Import the gradient tile tables.
output$PLOTS <- renderPlot({
  plotInput()
}) # End ouptut$PLOTS
