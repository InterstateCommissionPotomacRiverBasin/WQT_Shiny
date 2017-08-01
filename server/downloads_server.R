

#============================================================================
# Download the data table as a csv.
#============================================================================
output$plots.download <- downloadHandler(
  filename = function(){
    paste0(paste("WQT", sel.site(), sel.param(), sep = "_"), "_",
           Sys.Date(), ".png")
  },
  content = function(file) {
    if(is.null(prep.react())) return(NULL)
    prep.df <- prep.react()
    tile.plot <- tile_plot(prep.df, param.range)
    raw.loess.plot <- raw_loess_plot(prep.df)
    flow.correct.loess.plot <- flow_correct_loess_plot(prep.df)
    grid::grid.newpage()
    my.plots <- arrangeGrob(rbind(ggplotGrob(tile.plot),
                                  ggplotGrob(raw.loess.plot),
                                  ggplotGrob(flow.correct.loess.plot), size = "last"))
    
    ggsave(file, my.plots, width = 18, height = 16, dpi = 300)
  }
) # End ouput$plots.download
output$plots.download <- downloadHandler(
  filename = function(){
    paste0(paste("WQT", sel.site(), sel.param(), sep = "_"), "_",
           Sys.Date(), ".png")
  },
  content = function(file) {
    # Prevent red error message from appearing while data is loading.
    if(is.null(prep.react.monthly()) |
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
      ggsave(file, my.plots, width = 18, height = 16, dpi = 300)
    } else {
      grid::grid.newpage()
      my.plots <- arrangeGrob(rbind(ggplotGrob(tile.plot),
                                    ggplotGrob(raw.loess.plot), size = "last"))
      ggsave(file, my.plots, width = 18, height = 10.67, dpi = 300)
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
    write.csv(input.react(), con, row.names = FALSE)
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
