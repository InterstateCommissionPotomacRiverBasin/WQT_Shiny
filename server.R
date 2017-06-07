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
 
  #****************************************************************************
  # Sidebar Script
  # Dynamic reaction to changing the selected site.
  #****************************************************************************
  #============================================================================
  # If switching to Query by Paramter.
  #============================================================================
  observeEvent(sel.param(), {
    updateSelectInput(session, "PARAM.param",
                      choices = sort(unique(param.range$ICPRB_NAME)),
                      selected = sel.param())
  })
  #----------------------------------------------------------------------------
  observeEvent(c(sel.param(), input$query == "Parameter",
                 sel.huc()), {
    # Prevent red error message from appearing while data is loading.
    if(is.null(sel.param())) return(NULL)
    unique.huc8 <- unique(huc8[huc8$ICPRB_NAME %in% sel.param(), "HUC_8"])
    final.huc8 <- c("All HUCs", sort(unique.huc8))
    
    # When a new HUC is selected, the sites are modified to reflect only the 
    # sites within the HUC. If the currently selected site was not in the newly
    # selected HUC, then the sites are sorted in alphabetical order and the first
    # site is  selected. However, if the currently selected site was in the newly
    # selected HUC, the sites are sorted in alphabetical order but the currently
    # selected site does not change.
    if (sel.huc() %in% final.huc8) {
      select.this <- input$HUC_8.param
    } else {
      select.this <- final.huc8[1]
    }

    
    observeEvent(c(sel.param()), {
                   updateSelectInput(session, "HUC_8.param",
                                     choices = final.huc8,
                                     selected = select.this)
                   })

  })
  #----------------------------------------------------------------------------
  # Provide a dropdown menu with all of the available site selections.
  # Filters according to the HUC8 selected.
  site.react.param <- reactive({
    # Prevent red error message from appearing while data is loading.
    if (is.null(sel.huc())) {
      return(NULL)
    }   
    
    if (sel.huc() %in% "All HUCs") {
      final.vec <- sort(unique(huc8[huc8$ICPRB_NAME %in% sel.param(), "SITE"]))
    } else {
      final.vec <- sort(unique(huc8[huc8$HUC_8 %in% sel.huc() &
                                      huc8$ICPRB_NAME %in% sel.param(), "SITE"]))
    }
    
    return(final.vec)
  }) # End site.react.site
  #----------------------------------------------------------------------------
  observeEvent(c(sel.huc(), sel.param()), {
    # Prevent red error message from appearing while data is loading.
    if(is.null(site.react.param())) return(NULL)
    final.site <- site.react.param()
    # When a new HUC is selected, the sites are modified to reflect only the 
    # sites within the HUC. If the currently selected site was not in the newly
    # selected HUC, then the sites are sorted in alphabetical order and the first
    # site is  selected. However, if the currently selected site was in the newly
    # selected HUC, the sites are sorted in alphabetical order but the currently
    # selected site does not change.
    if (sel.site() %in% final.site) {
      select.this <- sel.site()
    } else {
      select.this <- final.site[1]
    }
    #==========================================================================
    # The sites in the dropdown menu will only represent the sites within the
    # selected HUC.
    #==========================================================================
    updateSelectInput(session, "SITE.param",
                      choices = as.character(final.site),
                      selected = select.this)
  }) # End observeEvent(c(input$HUC_8.site)
  #----------------------------------------------------------------------------
  #============================================================================
  # Query by Site
  #============================================================================
  # If switching to Query by Site.
#  observeEvent(sel.param(), {
#    updateSelectInput(session, "PARAM.site",
#                      choices = sort(unique(param.range$ICPRB_NAME)),
#                      selected = input$PARAM.param)
#  })
  #----------------------------------------------------------------------------
  observeEvent(c(sel.huc(), sel.param()), {
    updateSelectInput(session, "HUC_8.site",
                      choices = c("All HUCs", sort(unique(huc8$HUC_8))),
                      selected = sel.huc())
  })
  #----------------------------------------------------------------------------
#  observeEvent(sel.site(), {
#    updateSelectInput(session, "SITE.site",
#                      choices = input$SITE.param,
#                      selected = input$SITE.param)
#  })
  #----------------------------------------------------------------------------
  # Provide a dropdown menu with all of the available site selections.
  # Filters according to the HUC8 selected.
  site.react.site <- reactive({
    # Prevent red error message from appearing while data is loading.
    if (is.null(sel.huc())) {
      return(NULL)
    }   
    if (sel.huc() %in% "All HUCs") {
      final.vec <- sort(unique(huc8$SITE))
    } else {
      final.vec <- sort(unique(huc8[huc8$HUC_8 %in% sel.huc(), "SITE"]))
    }
    
    return(final.vec)
  }) # End site.react.site
  #----------------------------------------------------------------------------
  observeEvent(c(sel.huc(), sel.site()), {
    # Prevent red error message from appearing while data is loading.
    if(is.null(site.react.site())) return(NULL)
    final.site <- site.react.site()
    # When a new HUC is selected, the sites are modified to reflect only the 
    # sites within the HUC. If the currently selected site was not in the newly
    # selected HUC, then the sites are sorted in alphabetical order and the first
    # site is  selected. However, if the currently selected site was in the newly
    # selected HUC, the sites are sorted in alphabetical order but the currently
    # selected site does not change.
    if (sel.site() %in% final.site) {
      select.this <- sel.site()
    } else {
      select.this <- final.site[1]
    }
    #==========================================================================
    # The sites in the dropdown menu will only represent the sites within the
    # selected HUC.
    #==========================================================================
    updateSelectInput(session, "SITE.site",
                      choices = as.character(final.site),
                      selected = select.this)
  }) # End observeEvent(c(input$HUC_8.site)


  observeEvent(c(sel.site(), sel.param()), {
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
    if (sel.param() %in% final.param) {
      select.this <- sel.param()
    } else {
      select.this <- final.param[1]
    }
    #============================================================================= 
    # The parameters in the dropdown menu only represent parameters measured
    # at the selected site.
    #=============================================================================
    updateSelectInput(session, "PARAM.site",
                      choices = as.character(final.param),
                      selected = select.this)
  })
  #----------------------------------------------------------------------------
  observeEvent(sel.site(), {
    # Prevent red error message from appearing while data is loading.
    if(is.null(param.react())) return(NULL)
    sub.param <- param.react()
    
    final.param <- unique(sort(as.character(sub.param)))
    #============================================================================= 
    # A count of how many outliers were removed from the data used to create the figures.
    # The outliers will still appear in the "Data" tab.
    #=============================================================================
    output$OUTLIERS <- renderUI({
      # Prevent red error message from appearing while data is loading.
      if(is.null(param.tbl())) return(NULL)
      #============================================================================
      #sub.param <- param.tbl()[param.tbl()$SITE %in% input$SITE.site & param.tbl()$PARAMETER %in% input$PARAM.site, ]
      sub.param <- param.tbl()[param.tbl()$SITE %in% sel.site() & param.tbl()$ICPRB_NAME %in% sel.param(), ]
      sub.outliers <- outliers[outliers$PARAMETER %in% sel.param(), ]
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
      #sub.param <- param.tbl()[param.tbl()$SITE %in% input$SITE.site & param.tbl()$PARAMETER %in% input$PARAM.site, ]
      sub.param <- param.tbl()[param.tbl()$SITE %in% sel.site() & 
                                 param.tbl()$ICPRB_NAME %in% sel.param(), ]
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
      if(is.null(param.tbl())) {
        min.date <- NULL
      } else {
        min.date <- min(param.tbl()$DATE, na.rm = TRUE)
        min.date <- format(min.date, "%m/%d/%Y")
      }
      HTML(paste("<strong>First Date:</strong>", min.date, sep = " "))
    }) # End output$FIRST_DATE
    #--------------------------------------------------------------------------
    output$LAST_DATE <- renderUI({
      # Prevent red error message from appearing while data is loading.
      if(is.null(param.tbl())) {
        max.date <- NULL
      } else {
        max.date <- max(param.tbl()$DATE, na.rm = TRUE)
        max.date <- format(max.date, "%m/%d/%Y")
      }
      
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
      comp.vec <- c("Samples are collected as a grab",
                   "Samples collected continuously")
      uni.list <- uni.func(param.tbl()$COMPOSITE_METHOD)
      uni.list <- ifelse(any(comp.vec %in% uni.list), "Present", "Absent")
      HTML(paste("<strong>Composite:</strong>", uni.list, sep = " "))
    }) # End output$COMPOSITE
    #--------------------------------------------------------------------------
    output$GAGE <- renderUI({
      uni.list <- uni.func(param.tbl()$GAGE_ID)
      HTML(paste("<strong>Flow Gage:</strong>", uni.list, sep = " "))
    }) # End output$GAGE
    #--------------------------------------------------------------------------
    output$GAGE.AGENCY <- renderUI({
      if (is.null(gage.info.react())) {
        uni.list <- "<em>Blank</em>"
      } else {
        uni.list <- "USGS"
      }
      HTML(paste("<strong>Agency:</strong>", uni.list, sep = " "))
    }) # End output$GAGE.AGENCY
    #--------------------------------------------------------------------------
    output$GAGE.LOC <- renderUI({
      if (is.null(gage.info.react())) {
        uni.list <- "<em>Blank</em>"
      } else {
        uni.list <- uni.func(gage.info.react()$GAGE_NAME)
      }
      HTML(paste("<strong>Flow Gage Location:</strong>", uni.list, sep = " "))
    })# End output$GAGE.LOC
    #--------------------------------------------------------------------------
    output$GAGE.LAT <- renderUI({
      if (is.null(gage.info.react())) {
        uni.list <- "<em>Blank</em>"
      } else {
        uni.list <- uni.func(gage.info.react()$LAT_DD)
      }
      HTML(paste("<strong>Latitude:</strong>", uni.list, sep = " "))
    }) # End ouput$GAGE.LAT
    #--------------------------------------------------------------------------
    output$GAGE.LONG <- renderUI({
      if (is.null(gage.info.react())) {
        uni.list <- "<em>Blank</em>"
      } else {
        uni.list <- uni.func(gage.info.react()$LONG_DD)
      }
      HTML(paste("<strong>Longitude:</strong>", uni.list, sep = " "))
    }) # End output$GAGE.LONG
  }) # End observeEvent(c(input$SITE.site)
  #******************************************************************************
  # Tab Figures Script (Tab 1)
  #******************************************************************************
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
  
  
  #******************************************************************************
  # Tab Data Script (Tab 2)
  #******************************************************************************
  #============================================================================= 
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
  #****************************************************************************
  # Tab Maps Script (Tab 3)
  #****************************************************************************
  # Exract Lat/Long of selected site for plotting.
  points <- eventReactive(input$SITE.site, {
    # Prevent red error message from appearing while data is loading.
    if(is.null(param.tbl())) return(NULL)
    
    sites <- param.tbl()
    final.df <- sites[, c("SITE", "LATITUDE", "LONGITUDE",
                          "ProviderName", "SITE_NAME_EDIT")]
    names(final.df)[4:5] <- c("PROVIDER", "SITE_NAME")
    final.df$GAGE <- FALSE
    return(final.df)
  }, ignoreNULL = FALSE) # End points
  #----------------------------------------------------------------------------
  # Exract Lat/Long of selected site for plotting.
  points.gage <- eventReactive(sel.site(), {
    # Prevent red error message from appearing while data is loading.
    if(is.null(gage.info.react())) return(NULL)
    
    sites <- gage.info.react()
    final.df <- sites[, c("GAGE_ID", "LAT_DD", "LONG_DD", "GAGE_NAME")]
    names(final.df) <- c("SITE", "LATITUDE", "LONGITUDE", "SITE_NAME")
    final.df$PROVIDER <- "USGS"
    final.df[, 2:3] <- sapply(final.df[, 2:3], as.numeric)
    final.df$GAGE <- TRUE
    return(final.df)
  }, ignoreNULL = FALSE) # End points.gage
  #============================================================================ 
  # Plot the Site on the map.
  output$mymap <- renderLeaflet({
    map.df <- rbind(points(), points.gage())
    map.df$LATITUDE <- jitter(map.df$LATITUDE, factor = 0.000001)
    map.df$LONGITUDE <- jitter(map.df$LONGITUDE, factor = 0.000001)
    longitude <- mean(as.numeric(points()$LONGITUDE), na.rm = TRUE)
    latitude <- mean(as.numeric(points()$LATITUDE), na.rm = TRUE)
    #long.gage <- mean(points.gage()$LONG_DD, na.rm = TRUE)
    #lat.gage <- mean(points.gage()$LAT_DD, na.rm = TRUE)
    icprb.map <- "https://api.mapbox.com/styles/v1/skaisericprb/cizok18ny00302spia5zhre3o/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2thaXNlcmljcHJiIiwiYSI6ImNpa2U3cGN1dDAwMnl1cm0yMW94bWNxbDEifQ.pEG_X7fqCAowSN8Xr6rX8g"
    
    
    pal <- colorFactor(c("#E69F00", "#0072B2"), domain = c(FALSE, TRUE))
    
    leaflet() %>%
      #addTiles() %>%
      addTiles(urlTemplate = icprb.map, options = tileOptions(minZoom = 7, maxZoom = 18)) %>%
      setMaxBounds(lng1 = -90, lat1 = 34, lng2 = -64, lat2 = 45) %>%
      #setView(lng = -77.5, lat = 39.65305556, zoom = 7) %>% 
      setView(lng = longitude, lat = latitude, zoom = 7) %>% 
      
      #addProviderTiles("Hydda.Full",
      #                 options = providerTileOptions(noWrap = TRUE)) %>%
    addCircleMarkers(
      data = map.df[, c("LATITUDE", "LONGITUDE")],
      #lng = longitude, lat = latitude,
      fillColor = ~pal(map.df$GAGE),
      fill = TRUE,
      stroke = FALSE,
      #color = ~pal(map.df$GAGE),
      weight = 3,
      fillOpacity = 0.5,
      popup = paste("<strong>Site:</strong>", map.df$SITE, "<br/>",
                    "<strong>Data Provider:</strong>", map.df$PROVIDER, "<br/>",
                    "<strong>Latitude:</strong>", map.df$LATITUDE, "<br/>",
                    "<strong>Longitude:</strong>", map.df$LONGITUDE, "<br/>",
                    "<strong>Site Description:</strong>", map.df$SITE_NAME)) %>%
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
  observeEvent(c(input$SITE.site, input$PARAM.site), {
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
    if (input$PARAM.site %in% final.param) {
      select.this <- input$PARAM.site
    } else {
      select.this <- final.param[1]
    }
    
    #**************************************************************************
    # Tab Metadata Script (Tab 4)
    #**************************************************************************
    #==========================================================================
    # Identifying Outliers Heading
    #==========================================================================
    #sub.param <- param.tbl()[param.tbl()$SITE %in% input$SITE.site & param.tbl()$PARAMETER %in% input$PARAM.site, ]
    sub.param <- param.tbl()[param.tbl()$SITE %in% input$SITE.site & 
                               param.tbl()$ICPRB_NAME %in% input$PARAM.site, ]
    sub.outliers <- outliers[outliers$PARAMETER %in% input$PARAM.site, ]
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
