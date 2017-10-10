plots.reac <- reactive({
  # Prevent red error message from appearing while data is loading.
  if(is.null(prep.react.monthly()) ||
     is.null(prep.react.raw())) return(NULL)
  #-------------------------------------------------------------------------
  monthly.df <- prep.react.monthly()
  #-------------------------------------------------------------------------
  sub.outlier <- outliers[outliers$PARAMETER %in% sel.param(), ]
  raw.df <- param.tbl()
  raw.df <- raw.df[raw.df$REPORTED_VALUE < sub.outlier$UP_FENCE_4.5 &
                     raw.df$REPORTED_VALUE > sub.outlier$LOW_FENCE_4.5, ]
  #-------------------------------------------------------------------------
  tile.plot <- tile_plot(monthly.df, param.range)
  raw.loess.plot <- raw_loess_plot(raw.df)
  if (!is.null(gage.tbl())) {
    flow.correct.loess.plot <- flow_correct_loess_plot(raw.df, gage.tbl())
    grid::grid.newpage()
    my.plots <- arrangeGrob(rbind(ggplotGrob(tile.plot),
                                  ggplotGrob(raw.loess.plot),
                                  ggplotGrob(flow.correct.loess.plot), size = "last"))
  } else {
    grid::grid.newpage()
    my.plots <- arrangeGrob(rbind(ggplotGrob(tile.plot),
                                  ggplotGrob(raw.loess.plot), size = "last"))
  }
  return(my.plots)
})

output$plot1 <- renderPlot({
  req(plots.reac())
  plot(plots.reac())
}, height = function() {
  if (!is.null(gage.tbl())) {
  session$clientData$output_plot1_width / 1.125
  } else {
    session$clientData$output_plot1_width / 1.687
  }
})

output$plot2 <- renderImage({
  client.width  <- session$clientData$output_plot2_width / 72
  # A temp file to save the output. It will be deleted after renderImage
  # sends it, because deleteFile=TRUE.
  outfile <- tempfile(fileext = '.png')
  
  if (!is.null(gage.tbl())) {
    ggsave(outfile, plots.reac(), width = client.width, height = client.width / 1.125, dpi = 72)
  } else {
    ggsave(outfile, plots.reac(), width = client.width, height = client.width / 1.687, dpi = 72)
  }
  
  # Return a list
  list(src = outfile,
       alt = "This is alternate text")
}, deleteFile = TRUE)

#============================================================================
# Download the data table as a csv.
#============================================================================
output$plots.download <- downloadHandler(
  filename = function(){
    paste0(paste("WQT", sel.site(), sel.param(), sep = "_"), "_",
           Sys.Date(), ".png")
  },
  content = function(file) {
    
    if (!is.null(gage.tbl())) {
      ggsave(file, plots.reac(), width = 18, height = 16, dpi = 300)
    } else {
      ggsave(file, plots.reac(), width = 18, height = 10.67, dpi = 300)
    }
  }
) # End ouput$plots.download
#============================================================================
# Download the data table as a csv.
#============================================================================
output$param.tbl.download <- downloadHandler(
  filename = function() {
    paste0(paste("WQT", sel.site(), sel.param(), sep = "_"), "_",
           Sys.Date(), ".csv")
  },
  content = function(con) {
    data.table::fwrite(input.react(), con)
  }
) # End ouput$param.tbl.download
#============================================================================
# Download the map as a pdf.
# NOT FINISHED
#============================================================================
output$pdflink <- downloadHandler(
  filename = 'plot.pdf',
  content = function(file) {
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    saveWidget(newmap(), "temp.html", selfcontained = FALSE)
    webshot("temp.html", file = file, cliprect = "viewport")
  }
) # End output$pdflink
