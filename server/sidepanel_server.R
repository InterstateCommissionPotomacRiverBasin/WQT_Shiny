observeEvent(sel.site(), {
  # Prevent red error message from appearing while data is loading.
  if(is.null(param.react())) return(NULL)
  sub.param <- param.react()
  
  final.param <- unique(sort(as.character(sub.param)))
  #============================================================================= 
  # Report ICPRB_UNITS
  #=============================================================================
  output$ICPRB_UNIT <- renderUI({
    # Prevent red error message from appearing while data is loading.
    if(is.null(param.tbl())) return(NULL)
    #============================================================================
    sub.param <- param.tbl()[param.tbl()$SITE %in% sel.site() & param.tbl()$ICPRB_NAME %in% sel.param(), ]
    units.vec <- unique(sub.param$ICPRB_UNITS)
    #============================================================================
    HTML(paste("<strong>Units:</strong>", units.vec, sep = " "))
  }) # End output$OUTLIERS
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
    #agency <- unique(param.tbl()$AGENCY)
    #HTML(paste("<strong>Agency:</strong>", agency, sep = " "))
    uni.list <- uni.func(param.tbl()$AGENCY)
    HTML(paste("<strong>Agency:</strong>", uni.list, sep = " "))
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
    if (is.null(param.tbl())) {
      sub.depth <- NULL
    } else {
      sub.depth <- param.tbl() %>% 
        filter(DEPTH <= 1 | is.na(DEPTH)) %>% 
        .$DEPTH
    }
    uni.list <- uni.func(sub.depth)
    HTML(paste("<strong>Depth (m):</strong>", uni.list, sep = " "))
  }) # End output$DEPTH
  #--------------------------------------------------------------------------
  output$REPLICATE <- renderUI({
    uni.list <- uni.func(param.tbl()$REPLICATE_NUMBER)
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