#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Maintained: Zachary M. Smith
# Created: January 2017
# Updated: 2/28/17
#==============================================================================
#==============================================================================

library(shiny)
library(png)
library(DT)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RPostgreSQL)
library(grid)
library(Cairo)
options(shiny.usecairo = TRUE)
#wqt <- read.csv("./data/wqt.csv", stringsAsFactors = FALSE)

shinyServer(function(input, output, session) {
  #==============================================================================
  # Provide a dropdown menu with all of the available site selections.
  # Filters according to the HUC8 selected.
  #==============================================================================
  site.react <- reactive({
    # Prevent red error message from appearing while data is loading.
    if (is.null(input$HUC_8)) {
      return(NULL)
    }   
    if (input$HUC_8 %in% "All HUCs") {
      return(sort(huc8$SITE))
    } 
    final.vec <- sort(unique(huc8[huc8$HUC_8 %in% input$HUC_8, "SITE"]))
    return(final.vec)
  }) # End site.react
  #==============================================================================
  # Upload data from postgres for the selected site.
  #==============================================================================
  param.tbl <- reactive({
    wqt <- dbReadTable(pool, paste0("SITE_", input$SITE))
  }) # End param.tbl
  #==============================================================================
  # Subset the data to only include the parameter of interest.
  #==============================================================================
  output$tbl <- renderTable({
    # Prevent red error message from appearing while data is loading.
    if(is.null(param.tbl())) return(NULL)
    final.df <- param.tbl()
    #final.df <- final.df[final.df$PARAMETER %in% input$PARAM, ]
    final.df <- final.df[final.df$ICPRB_NAME %in% input$PARAM, ]
    return(final.df)
  }) # End output$tbl
  #==============================================================================
  # Update the list of parameters to reflect only the parameters observed at the
  #selected site.
  #==============================================================================
  param.react <- reactive({
    # Prevent red error message from appearing while data is loading.
    if (is.null(param.tbl())) return(NULL)
    sites <- param.tbl()
    final.df <- sites[sites$SITE %in% input$SITE, ]
    #final.df <- wqt[wqt$SITE %in% input$SITE, ]
    #final.vec <- unique(final.df$PARAMETER)
    final.vec <- unique(final.df$ICPRB_NAME)
    return(final.vec)
  }) # End param.react
  #******************************************************************************
  # Sidebar Script
  # Dynamic reaction to changing the selected site.
  #******************************************************************************
  observeEvent(c(input$HUC_8), {
    # Prevent red error message from appearing while data is loading.
    if(is.null(site.react())) return(NULL)
    final.site <- site.react()
    # When a new HUC is selected, the sites are modified to reflect only the 
    # sites within the HUC. If the currently selected site was not in the newly
    # selected HUC, then the sites are sorted in alphabetical order and the first
    # site is  selected. However, if the currently selected site was in the newly
    # selected HUC, the sites are sorted in alphabetical order but the currently
    # selected site does not change.
    if (input$SITE %in% final.site) {
      select.this <- input$SITE
    } else {
      select.this <- final.site[1]
    }
    #==========================================================================
    # The sites in the dropdown menu will only represent the sites within the
    # selected HUC.
    #==========================================================================
    updateSelectInput(session, "SITE",
                      choices = as.character(final.site),
                      selected = select.this)
  }) # End observeEvent(c(input$HUC_8)
  #============================================================================
  observeEvent(c(input$SITE), {
    # Prevent red error message from appearing while data is loading.
    if(is.null(param.react())) return(NULL)
    sub.param <- param.react()
    
    final.param <- unique(sort(as.character(sub.param)))
    
    # When a new site is selected, the parameters are modified to reflect only
    # the parameters that have been measured at that site. If the currently
    # selected parameter was not measured at the newly selected site, then the
    # parameters are sorted in alphabetical order and the first parameter is 
    # selected. However, if the currently selected parameter was also measured
    # at the newly selected site, the parametes are sorted in alphabetical order
    # but the currently selected parameter does not change.
    if (input$PARAM %in% final.param) {
      select.this <- input$PARAM
    } else {
      select.this <- final.param[1]
    }
    #============================================================================= 
    # The parameters in the dropdown menu only represent parameters measured
    # at the selected site.
    #=============================================================================
    updateSelectInput(session, "PARAM",
                      choices = as.character(final.param),
                      selected = select.this)
    #============================================================================= 
    # A count of how many outliers were removed from the data used to create the figures.
    # The outliers will still appear in the "Data" tab.
    #=============================================================================
    output$OUTLIERS <- renderUI({
      # Prevent red error message from appearing while data is loading.
      if(is.null(param.tbl())) return(NULL)
      #============================================================================
      #sub.param <- param.tbl()[param.tbl()$SITE %in% input$SITE & param.tbl()$PARAMETER %in% input$PARAM, ]
      sub.param <- param.tbl()[param.tbl()$SITE %in% input$SITE & param.tbl()$ICPRB_NAME %in% input$PARAM, ]
      sub.outliers <- outliers[outliers$PARAMETER %in% input$PARAM, ]
      #============================================================================
      outlier.count <- nrow(sub.param[sub.param$REPORTED_VALUE >= sub.outliers$UP_FENCE_4.5 |
                                        sub.param$REPORTED_VALUE <= sub.outliers$LOW_FENCE_4.5, ])
      #============================================================================
      HTML(paste("<strong>Outliers Removed:</strong>", outlier.count, sep = " "))
    }) # End output$OUTLIERS
    #============================================================================= 
    # A count of how many non-detects are present in the data used to create the figures.
    #=============================================================================
    output$CENSORED <- renderUI({
      # Prevent red error message from appearing while data is loading.
      if(is.null(param.tbl())) return(NULL)
      #============================================================================
      #sub.param <- param.tbl()[param.tbl()$SITE %in% input$SITE & param.tbl()$PARAMETER %in% input$PARAM, ]
      sub.param <- param.tbl()[param.tbl()$SITE %in% input$SITE & 
                                 param.tbl()$ICPRB_NAME %in% input$PARAM, ]
      sub.censored <- sub.param[sub.param$CENSORED %in% "Censored", ]
      #sub.censored <- sub.param[!is.na(sub.param$ResultDetectionConditionText) &
      #                             !sub.param$ResultDetectionConditionText %in% "", ]
      #============================================================================
      count.censored <- nrow(sub.censored)
      count.all <- nrow(sub.param)
      #============================================================================
      pct.censored <- round(count.censored / count.all * 100, 2)
      pct.censored <- paste0(pct.censored, "%")
      pct.censored <- paste0(pct.censored, " (n = ", count.censored, ")")
      #============================================================================
      HTML(paste("<strong>Censored Data:</strong>", pct.censored, sep = " "))
    }) # End output$CENSORED
    #============================================================================= 
    # A list of the parameters available for the selected site.
    #=============================================================================
    output$PARAM_LIST <- renderUI({
      # Prevent red error message from appearing while data is loading.
      if(is.null(final.param)) return(NULL)
      param.list <- paste(final.param, collapse = ",  ")
      HTML(paste("<strong>Available Parameters:</strong>", param.list, sep = " "))
    }) # End output$PARAM_LIST
    #--------------------------------------------------------------------------
    output$SEL_SITE <- renderUI({
      # Prevent red error message from appearing while data is loading.
      if(is.null(param.tbl())) return(NULL)
      sel.site <- unique(param.tbl()$SITE)[1]
      HTML(paste("<strong>Site:</strong>", sel.site, sep = " "))
    }) # End output$SEL_SITE
    #--------------------------------------------------------------------------
    output$AGENCY <- renderUI({
      # Prevent red error message from appearing while data is loading.
      if(is.null(param.tbl())) return(NULL)
      #agency <- unique(param.tbl()$AGENCY.x)
      agency <- unique(param.tbl()$AGENCY)
      HTML(paste("<strong>Agency:</strong>", agency, sep = " "))
    }) # End output$AGENCY
    #--------------------------------------------------------------------------
    output$FIRST_DATE <- renderUI({
      # Prevent red error message from appearing while data is loading.
      if(is.null(param.tbl())) return(NULL)
      min.date <- min(param.tbl()$DATE)
      min.date <- format(min.date, "%m/%d/%Y")
      HTML(paste("<strong>First Date:</strong>", min.date, sep = " "))
    }) # End output$FIRST_DATE
    #--------------------------------------------------------------------------
    output$LAST_DATE <- renderUI({
      # Prevent red error message from appearing while data is loading.
      if(is.null(param.tbl())) return(NULL)
      max.date <- max(param.tbl()$DATE)
      max.date <- format(max.date, "%m/%d/%Y")
      HTML(paste("<strong>Last Date:</strong>", max.date, sep = " "))
    }) # End output$LAST_DATE
    #--------------------------------------------------------------------------
    output$STATE <- renderUI({
      uni.list <- uni.func(param.tbl()$STATE)
      HTML(paste("<strong>State:</strong>", uni.list, sep = " "))
    }) # End output$STATE
    #--------------------------------------------------------------------------
    output$LAT <- renderUI({
      uni.list <- uni.func(param.tbl()$LATITUDE)
      HTML(paste("<strong>Latitude:</strong>", uni.list, sep = " "))
    }) # End output$LAT
    #--------------------------------------------------------------------------
    output$LONG <- renderUI({
      uni.list <- uni.func(param.tbl()$LONGITUDE)
      HTML(paste("<strong>Longitude:</strong>", uni.list, sep = " "))
    }) # End output$LONG
    #--------------------------------------------------------------------------
    output$DEPTH <- renderUI({
      uni.list <- uni.func(param.tbl()$DEPTH)
      HTML(paste("<strong>Depth (m):</strong>", uni.list, sep = " "))
    }) # End output$DEPTH
    #--------------------------------------------------------------------------
    output$REPLICATE <- renderUI({
      uni.list <- uni.func(param.tbl()$Replicate.Number)
      HTML(paste("<strong>Replicate:</strong>", uni.list, sep = " "))
    }) # End output$REPLICATE
    #--------------------------------------------------------------------------
    output$COMPOSITE <- renderUI({
      uni.list <- uni.func(param.tbl()$COMPOSITE)
      HTML(paste("<strong>Composite:</strong>", uni.list, sep = " "))
    }) # End output$COMPOSITE
    #--------------------------------------------------------------------------
    output$GAGE <- renderUI({
      uni.list <- uni.func(param.tbl()$GAGE)
      HTML(paste("<strong>Flow Gage:</strong>", uni.list, sep = " "))
    }) # End output$GAGE
    #--------------------------------------------------------------------------
    output$GAGE.AGENCY <- renderUI({
      HTML(paste("<strong>Agency:</strong>", "USGS", sep = " "))
    }) # End output$GAGE.AGENCY
    #--------------------------------------------------------------------------
    output$GAGE.LOC <- renderUI({
      uni.list <- uni.func(param.tbl()$GAGE_LOCATION)
      HTML(paste("<strong>Flow Gage Location:</strong>", uni.list, sep = " "))
    })# End output$GAGE.LOC
    #--------------------------------------------------------------------------
    output$GAGE.LAT <- renderUI({
      uni.list <- uni.func(param.tbl()$LAT_DD)
      HTML(paste("<strong>Latitude:</strong>", uni.list, sep = " "))
    }) # End ouput$GAGE.LAT
    #--------------------------------------------------------------------------
    output$GAGE.LONG <- renderUI({
      uni.list <- uni.func(param.tbl()$LONG_DD)
      HTML(paste("<strong>Longitude:</strong>", uni.list, sep = " "))
    }) # End output$GAGE.LONG
  }) # End observeEvent(c(input$SITE)
  #******************************************************************************
  # Tab Figures Script (Tab 1)
  #******************************************************************************
  prep.react <- reactive({
    if(is.null(input$SITE)) return(NULL)
    site.char <- as.character(unique(input$SITE)[1])
    if(is.null(input$PARAM)) return(NULL)
    param.char <- as.character(unique(input$PARAM)[1])
    # Prevent red error message from appearing while data is loading.
    if(is.null(param.tbl())) return(NULL)
    # Prep data when data is available
    final.df <- prep_plot(param.tbl(), site.char, param.char, outliers)
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
  #============================================================================
  # Download the data table as a csv.
  #============================================================================
  output$plots.download <- downloadHandler(
    filename = function(){
      paste0(paste("WQT", input$SITE, input$PARAM, sep = "_"), "_",
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
  
  
  #******************************************************************************
  # Tab Data Script (Tab 2)
  #******************************************************************************
  #============================================================================= 
  # Subset the data to only represent the selected Site and Parameter.
  input.react <- reactive({
    # Prevent red error message from appearing while data is loading.
    if(is.null(param.tbl())) return(NULL)
    sites <- param.tbl()
    #final.df <- wqt[wqt$SITE %in% input$SITE, ]
    final.df <- sites[sites$SITE %in% input$SITE, ]
    #final.df <- unique(final.df[final.df$PARAMETER %in% input$PARAM, ])
    final.df <- unique(final.df[final.df$ICPRB_NAME %in% input$PARAM, ])
    final.df <- final.df[order(final.df$DATE), ]
    return(final.df)
  }) # End input.react
  #============================================================================= 
  # Generate a table representing the selected Site and Parameter.
  output$param_table <- DT::renderDataTable({
    sel.param <- input$PARAM
    
    if (is.null(sel.param)) return(NULL)
    
    # Prevent red error message from appearing while data is loading.
    if(is.null(input.react())) return(NULL)
    
    datatable(input.react(), options = list(
      scrollX = 2000, 
      scrollY = 700,
      autoWidth = TRUE,
      
      columnDefs = list(list(width = '300px', targets = c(4, 65)),
                        
                        list(className = 'dt-center', targets = 1:ncol(input.react()))),
      
      #list(list(width = '2000px', targets = "AGENCY_NAME"))
      #),
      pageLength = 10,
      #paging = FALSE,
      color = "black")) %>%
      formatDate(columns = "DATE", method = 'toLocaleDateString')
    
  }, options = list(paging = TRUE, color = "black")) # End output$param_table
  
  #============================================================================
  # Download the data table as a csv.
  #============================================================================
  output$param.tbl.download <- downloadHandler(
    filename = function() {
      paste0(paste("WQT", input$SITE, input$PARAM, sep = "_"), "_",
             Sys.Date(), ".csv")
    },
    content = function(con) {
      write.csv(input.react(), con, row.names = FALSE)
    }
  ) # End ouput$param.tbl.download
  #============================================================================
  #****************************************************************************
  # Tab Maps Script (Tab 3)
  #****************************************************************************
  # Exract Lat/Long of selected site for plotting.
  points <- eventReactive(input$SITE, {
    # Prevent red error message from appearing while data is loading.
    if(is.null(param.tbl())) return(NULL)
    
    sites <- param.tbl()
    sites[sites$SITE %in% input$SITE, c("SITE", "LATITUDE", "LONGITUDE")]
  }, ignoreNULL = FALSE) # End points
  #----------------------------------------------------------------------------
  # Exract Lat/Long of selected site for plotting.
  points.gage <- eventReactive(input$SITE, {
    # Prevent red error message from appearing while data is loading.
    if(is.null(param.tbl())) return(NULL)
    
    sites <- param.tbl()
    sites <- sites[sites$SITE %in% input$SITE, c("LAT_DD", "LONG_DD")]
    final.df <- data.frame(lapply(sites, as.numeric))
    return(final.df)
  }, ignoreNULL = FALSE) # End points.gage
  #============================================================================ 
  # Plot the Site on the map.
  output$mymap <- renderLeaflet({
    longitude <- mean(as.numeric(points()$LONGITUDE), na.rm = TRUE)
    latitude <- mean(as.numeric(points()$LATITUDE), na.rm = TRUE)
    #long.gage <- mean(points.gage()$LONG_DD, na.rm = TRUE)
    #lat.gage <- mean(points.gage()$LAT_DD, na.rm = TRUE)
    icprb.map <- "https://api.mapbox.com/styles/v1/skaisericprb/cizok18ny00302spia5zhre3o/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2thaXNlcmljcHJiIiwiYSI6ImNpa2U3cGN1dDAwMnl1cm0yMW94bWNxbDEifQ.pEG_X7fqCAowSN8Xr6rX8g"
    
    leaflet() %>%
      #addTiles() %>%
      addTiles(urlTemplate = icprb.map, options = tileOptions(minZoom = 7, maxZoom = 18)) %>%
      setMaxBounds(lng1 = -90, lat1 = 34, lng2 = -64, lat2 = 45) %>%
      #setView(lng = -77.5, lat = 39.65305556, zoom = 7) %>% 
      setView(lng = longitude, lat = latitude, zoom = 7) %>% 
      
      #addProviderTiles("Hydda.Full",
      #                 options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(#data = points()[, c("LATITUDE", "LONGITUDE")],
        lng = longitude, lat = latitude,
        fillColor = "#E69F00",
        stroke = FALSE,
        color = "black",
        weight = 3,
        fillOpacity = 0.5,
        #label = points()$SITE,
        popup = paste("<strong>Site:</strong>", points()$SITE)) %>%
      #addCircleMarkers(lng = points.gage()$LONG_DD, lat = points.gage()$LAT_DD,
      #                 fillColor = "#0072B2",
      #                 stroke = FALSE,
      #                 color = "black",
      #                 weight = 3,
      #                 fillOpacity = 0.5) %>%
      
      addLegend(position = "topright",
                title = "Legend",
                labels = c("Site", "Flow Gage"),
                colors = c("#E69F00", "#0072B2"),
                opacity = 1)
    #addMarkers(data = points.gage(), lng = long.gage, lat = lat.gage)
  }) # End output$MAP
  #----------------------------------------------------------------------------
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
  #----------------------------------------------------------------------------
  observeEvent(c(input$SITE, input$PARAM), {
    # Prevent red error message from appearing while data is loading.
    if(is.null(param.react())) return(NULL)
    sub.param <- param.react()
    
    final.param <- unique(sort(as.character(sub.param)))
    
    # When a new site is selected, the parameters are modified to reflect only
    # the parameters that have been measured at that site. If the currently
    # selected parameter was not measured at the newly selected site, then the
    # parameters are sorted in alphabetical order and the first parameter is 
    # selected. However, if the currently selected parameter was also measured
    # at the newly selected site, the parametes are sorted in alphabetical order
    # but the currently selected parameter does not change.
    if (input$PARAM %in% final.param) {
      select.this <- input$PARAM
    } else {
      select.this <- final.param[1]
    }
    
    #**************************************************************************
    # Tab Metadata Script (Tab 4)
    #**************************************************************************
    #==========================================================================
    # Identifying Outliers Heading
    #==========================================================================
    #sub.param <- param.tbl()[param.tbl()$SITE %in% input$SITE & param.tbl()$PARAMETER %in% input$PARAM, ]
    sub.param <- param.tbl()[param.tbl()$SITE %in% input$SITE & 
                               param.tbl()$ICPRB_NAME %in% input$PARAM, ]
    sub.outliers <- outliers[outliers$PARAMETER %in% input$PARAM, ]
    #--------------------------------------------------------------------------
    outlier.count <- nrow(sub.param[sub.param$REPORTED_VALUE >= sub.outliers$UP_FENCE_4.5 |
                                      sub.param$REPORTED_VALUE <= sub.outliers$LOW_FENCE_4.5, ])
    #--------------------------------------------------------------------------
    output$META_OUTLIER_INFO <- renderUI({
      HTML(paste(paste("<strong>Lower Fence:</strong>", unique(sub.outliers$LOW_FENCE_4.5), sep = " "),
                 paste("<strong>Upper Fence:</strong>", unique(sub.outliers$UP_FENCE_4.5), sep = " "),
                 paste("<strong>Outliers Removed:</strong>", outlier.count, sep = " "),
                 "<br/>", sep = "<br/>"))
      
    }) #End output$MEAT_OUTLIER_INFO
    #==========================================================================
    # Parameter Standardization Heading
    #==========================================================================
    sub.param_stand <- lapply(unique(sub.param$ProviderName), function(x){
      keep.cols <- c("ProviderName", "PARAMETER",
                     "REPORTED_UNITS", "USGSPCode",
                     "DetectionQuantitationLimitTypeName",
                     "DetectionQuantitationLimitMeasure.MeasureValue",
                     "DetectionQuantitationLimitMeasure.MeasureUnitCode",
                     "ICPRB_CONVERSION")
      keep.cols[!keep.cols %in% names(sub.param)]
      
      return(unique(sub.param[sub.param$ProviderName %in% x, keep.cols]))
      
    }) # End sub.param_stand
    #--------------------------------------------------------------------------
    output$PARAM_STAND_LOOP <- renderUI({
      #uni.list <- uni.func(sub.param$PARAMETER)
      #test.list <- list()
      param.list <- lapply(1:length(sub.param_stand), function(i) {
        
        sub.i <- data.frame(sub.param_stand[i], stringsAsFactors = FALSE)
        sub.i$final.detect <- paste(sub.i$DetectionQuantitationLimitMeasure.MeasureValue, 
                                    sub.i$DetectionQuantitationLimitMeasure.MeasureUnitCode, sep = " ")
        #----------------------------------------------------------------------
        detect.cols <- c("DetectionQuantitationLimitMeasure.MeasureValue",
                         "DetectionQuantitationLimitMeasure.MeasureUnitCode")
        detect.list <- lapply(1:nrow(unique(sub.i[, detect.cols])), function(j) {
          detect.this <- unique(sub.i[, c(detect.cols)])
          sub.j <- data.frame(detect.this[j, ], stringsAsFactors = FALSE)
          final.df <- paste(sub.j$DetectionQuantitationLimitMeasure.MeasureValue, 
                            sub.j$DetectionQuantitationLimitMeasure.MeasureUnitCode, sep = " ")
          return(final.df)
        }) # End detec.list
        detect.final <- do.call(rbind, detect.list)
        #----------------------------------------------------------------------
        final.i <- paste(paste("<strong>Data Provider:</strong>", uni.func(sub.i$ProviderName)), 
                         paste("<strong>Reported Parameter:</strong>",
                               uni.func(sub.i$PARAMETER), sep = " "),
                         paste("<strong>Reported Units:</strong>", uni.func(sub.i$REPORTED_UNITS), sep = " "),
                         paste("<strong>USGS Code:</strong>", uni.func(sub.i$USGSPCode), sep = " "),
                         paste("<strong>Detection Limit Type:</strong>", uni.func(sub.i$DetectionQuantitationLimitTypeName), sep = " "),
                         paste("<strong>Detection Limit Value:</strong>", uni.func(sub.i$final.detect), sep = " "),
                         paste("<strong>Conversion Applied:</strong>", uni.func(sub.i$ICPRB_CONVERSION), sep = " "),
                         
                         #paste(uni.func(sub.i$DetectionQuantitationLimitMeasure.MeasureValue),
                         #uni.func(sub.i$DetectionQuantitationLimitMeasure.MeasureUnitCode)),
                         
                         
                         "<br/>", sep = "<br/>")
      }) # End param.list
      final.test <- do.call(rbind, param.list)
      HTML(paste(final.test[, 1], sep = "<br/>"))
    }) # End output$PARAM_STAND_LOOP
    
    #output$PARAM_REPORTED <- renderUI({
    #  uni.list <- uni.func(sub.param$PARAMETER)
    #  HTML(paste("<strong>Reported Parameter:</strong>", uni.list, sep = " "))
    #})
    
    #==========================================================================
    # Site Information Heading
    #==========================================================================
    sub.site_info <- lapply(unique(sub.param$ProviderName), function(x){
      keep.cols <- c("ProviderName", "AGENCY_NAME", "SITE_NAME", "DEPTH",
                     "ActivityMediaName", "ACTIVITY_TYPE",
                     "SampleCollectionMethod.MethodIdentifier",
                     "SampleCollectionMethod.MethodIdentifierContext")
      keep.cols[!keep.cols %in% names(sub.param)]
      
      return(unique(sub.param[sub.param$ProviderName %in% x, keep.cols]))
      
    }) # End sub.site_info
    #--------------------------------------------------------------------------
    output$SITE_INFO_LOOP <- renderUI({
      site.list <- lapply(1:length(sub.site_info), function(i) {
        sub.i <- data.frame(sub.site_info[i], stringsAsFactors = FALSE)
        sub.vec <- paste(paste("<strong>Data Provider:</strong>", uni.func(sub.i$ProviderName)), 
                         paste("<strong>Agency Formal Name:</strong>", uni.func(sub.i$AGENCY_NAME)),  
                         paste("<strong>Site Name:</strong>", uni.func(sub.i$SITE_NAME)), 
                         paste("<strong>Activity Name:</strong>", uni.func(sub.i$ActivityMediaName)),
                         paste("<strong>Activity Type Code:</strong>", uni.func(sub.i$ActivityTypeCode)),
                         "<br/>", sep = "<br/>")
        return(sub.vec)
      }) # End site.list
      final.vec <- do.call(rbind, site.list)
      HTML(paste(final.vec[,1], sep = "<br/>"))
    }) # End output$SITE_INFO_LOOP
  }) # End observe event
}) # End Shiny Server
