prep.react.raw <- reactive({
  # Prevent red error message from appearing while data is loading.
  if(is.null(param.tbl())) return(NULL)
  # Prep data when data is available
  final.df <- prep_plot(param.tbl(), sel.site(), sel.param(), outliers,
                        monthly.mean = FALSE)
  # Remove NAs from the REPORTING_VALUE column.
  # final.df <- final.df[complete.cases(final.df$REPORTED_VALUE), ]
  # End prep.react reactive function.
  return(final.df)
}) # End prep.react
#----------------------------------------------------------------------------
prep.react.monthly <- reactive({
  # Prevent red error message from appearing while data is loading.
  if(is.null(param.tbl())) return(NULL)
  # Prep data when data is available
  final.df <- prep_plot(param.tbl(), sel.site(), sel.param(), outliers,
                        monthly.mean = TRUE)
  # Remove NAs from the REPORTING_VALUE column.
  # final.df <- final.df[complete.cases(final.df$REPORTED_VALUE), ]
  # End prep.react reactive function.
  return(final.df)
}) # End prep.react
#----------------------------------------------------------------------------
output$ICPRB_UNITS <- renderUI({
  # Prevent red error message from appearing while data is loading.
  if(is.null(prep.react.monthly())) return(NULL)
  ICPRB_UNITS <- unique(na.omit(prep.react.monthly()$ICPRB_UNITS))
  HTML(paste("<strong>Units:</strong>", ICPRB_UNITS, sep = " "))
}) # End output$ICPRB_UNITS
#----------------------------------------------------------------------------
plotInput <- reactive({
  withProgress(message = "Loading...", value = 0, {
  # Prevent red error message from appearing while data is loading.
  if(is.null(prep.react.monthly()) |
     is.null(prep.react.raw())) return(NULL)
  #-------------------------------------------------------------------------
  monthly.df <- prep.react.monthly()
  incProgress(1/3)
  #-------------------------------------------------------------------------
  sub.outlier <- outliers[outliers$PARAMETER %in% sel.param(), ]
  raw.df <- param.tbl()
  raw.df <- raw.df[raw.df$REPORTED_VALUE < sub.outlier$UP_FENCE_4.5 &
                   raw.df$REPORTED_VALUE > sub.outlier$LOW_FENCE_4.5, ]
  incProgress(1/3)
  #-------------------------------------------------------------------------
  tile.plot <- tile_plot(monthly.df, param.range)
  raw.loess.plot <- raw_loess_plot(raw.df)
  if (!is.null(gage.tbl())) {
    flow.correct.loess.plot <- flow_correct_loess_plot(raw.df, gage.tbl())
    grid::grid.newpage()
    grid::grid.draw(rbind(ggplotGrob(tile.plot),
                          ggplotGrob(raw.loess.plot),
                          ggplotGrob(flow.correct.loess.plot),
                          size = "first"))
  } else {
    blank.plot <- blank_plot(raw.df)
    grid::grid.newpage()
    grid::grid.draw(rbind(ggplotGrob(tile.plot),
                          ggplotGrob(raw.loess.plot),
                          ggplotGrob(blank.plot),
                          size = "first"))
  }
  incProgress(1/3)
  })
  
}) # End plotInput
#----------------------------------------------------------------------------
# Import the gradient tile tables.
output$PLOTS <- renderPlot({
  plotInput()
}) # End ouptut$PLOTS

#----------------------------------------------------------------------------
plot.height <- reactive({
  if(is.null(sel.gage())) return(NULL)
  #uni.list <- uni.func(param.tbl()$GAGE_ID)
  uni.list <- sel.gage()
  #if (any(!grepl("<em>Blank</em>", uni.list))) {
  if (!is.na(uni.list)) {
    final.vec <- "1200"
  } else {
    final.vec <- "800"
  }
  return(final.vec)
}) # End output$GAGE


output$plot.ui <- renderUI({
  if(is.null(sel.gage())) return(NULL)
  #uni.list <- uni.func(param.tbl()$GAGE_ID)
  uni.list <- sel.gage()
  #if (any(!grepl("<em>Blank</em>", uni.list))) {
  if (!is.na(uni.list)) {
    final.vec <- "1200"
  } else {
    final.vec <- "800"
  }
 plotOutput("PLOTS", height = final.vec, width = "1500")
})



#----------------------------------------------------------------------------