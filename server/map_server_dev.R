# Exract Lat/Long of selected site for plotting.
points <- eventReactive(input$SITE.site, {
  # Prevent red error message from appearing while data is loading.
  if(is.null(param.tbl())) return(NULL)
  
  sites <- param.tbl()
  final.df <- sites[, c("SITE", "LATITUDE", "LONGITUDE",
                        "ProviderName", "SITE_NAME_EDIT")]
  names(final.df)[4:5] <- c("PROVIDER", "SITE_NAME")
  #final.df$GAGE <- FALSE
  final.df$TYPE <- "selected"
  return(final.df)
}, ignoreNULL = FALSE) # End points
#----------------------------------------------------------------------------
# Exract Lat/Long of selected site for plotting.
points.gage <- eventReactive(sel.site(), {
  # Prevent red error message from appearing while data is loading.
  if(is.null(gage.info.react())) return(NULL)
  
  sites <- gage.info.react()
  final.df <- sites[, c("GAGE_ID", "LAT_DD", "LONG_DD", "GAGE_NAME")]
  names(final.df) <- c("SITE", "LATITUDE", "LONGITUDE", "SITE_NAME_EDIT")
  final.df$PROVIDER <- "USGS"
  final.df[, 2:3] <- sapply(final.df[, 2:3], as.numeric)
  #final.df$GAGE <- TRUE
  final.df$ICPRB_NAME <- "NA"
  final.df$HUC_8 <- "NA"
  final.df$PROVIDERNAME <- "USGS"
  final.df$TYPE <- "Gage"
  return(final.df)
}, ignoreNULL = FALSE) # End points.gage
#============================================================================ 
# Plot the Site on the map.
output$mymap <- renderLeaflet({
  withProgress(message = 'Gathering points...',  value = 0, {
    if(is.null(param.tbl())) return(NULL)
#  wilcox.df <- wilcox.react() %>% 
#    mutate(trend = factor(.$trend, levels = c("none_not_significant",
#                                            "weak_negative", "weak_positive",
#                                            "strong_negative", "strong_positive"))) %>% 
#    arrange(trend) %>% 
#    mutate(trend = factor(.$trend, levels = c("strong_positive", "weak_positive",
#                                            "none_not_significant",
#                                            "weak_negative", "strong_negative")))
  #----------------------------------------------------------------------------
#  labels.vec <- c("Strong Positive", "Weak Positive", "None",
#                  "Weak Negative", "Strong Negative")
  #--------------------------------------------------------------------------
  if (sel.huc() != "All HUCs") {
    new.query <- paste(
      'SELECT DISTINCT "ICPRB_NAME", "SITE", "LATITUDE", "LONGITUDE", "PROVIDERNAME", "SITE_NAME_EDIT", "HUC_8"',
      'FROM "wq_data"',
      'WHERE "ICPRB_NAME" =', paste0("'", sel.param(), "'"),
      'AND "HUC_8 = ', paste0("'", sel.huc(), "'"))
  } else {
    new.query <- paste(
      'SELECT DISTINCT "ICPRB_NAME", "SITE", "LATITUDE", "LONGITUDE", "PROVIDERNAME", "SITE_NAME_EDIT", "HUC_8"',
      'FROM "wq_data"',
      'WHERE "ICPRB_NAME" =', paste0("'", sel.param(), "'"))
  }
    incProgress(1/3)
  #--------------------------------------------------------------------------
  # Create a seperate table in the data base, so that this moves faster.
  map.df <- dbGetQuery(pool, new.query) %>% 
    mutate(TYPE = if_else(SITE == sel.site(),
           "Selected Site", "Other Sites")) %>% 
    dplyr::bind_rows(points.gage()) %>% 
    mutate(TYPE = factor(TYPE, levels = c("Other Sites", "Gage", "Selected Site"))) %>% 
    arrange(TYPE)
  incProgress(1/3)
  #--------------------------------------------------------------------------
  icprb.map <- "https://api.mapbox.com/styles/v1/skaisericprb/cizok18ny00302spia5zhre3o/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2thaXNlcmljcHJiIiwiYSI6ImNpa2U3cGN1dDAwMnl1cm0yMW94bWNxbDEifQ.pEG_X7fqCAowSN8Xr6rX8g"
  #--------------------------------------------------------------------------
  leaflet.map <- leaflet(map.df) %>%
    #addTiles() %>%
    addTiles(urlTemplate = icprb.map, options = tileOptions(minZoom = 7, maxZoom = 18)) %>%
    setMaxBounds(lng1 = -90, lat1 = 34, lng2 = -64, lat2 = 45) %>%
    setView(lng = -77.5, lat = 39.65305556, zoom = 7) %>% 
    #setView(lng = longitude, lat = latitude, zoom = 7) %>%
    addCircleMarkers(
      color = ~case_when(map.df$TYPE == "Other Sites" ~ "#999999",
                         map.df$TYPE == "Gage" ~ "#0072B2",
                         map.df$TYPE == "Selected Site" ~ "#E69F00",
                         TRUE ~ "pink"),
      radius = ~case_when(map.df$TYPE == "Other Sites" ~ 6,
                          map.df$TYPE == "Gage" ~ 8,
                          map.df$TYPE == "Selected Site" ~ 10,
                          TRUE ~ 25),
      opacity = ~case_when(map.df$TYPE == "Other Sites" ~ 0.75,
                           map.df$TYPE == "Gage" ~ 1,
                           map.df$TYPE == "Selected Site" ~ 1,
                           TRUE ~ 25),
      fillOpacity = ~case_when(map.df$TYPE == "Other Sites" ~ 0.5,
                           map.df$TYPE == "Gage" ~ 1,
                           map.df$TYPE == "Selected Site" ~ 1,
                           TRUE ~ 25),
      stroke = FALSE,
      data = map.df[, c("LATITUDE", "LONGITUDE")],
      popup = paste("<strong>Site:</strong>", map.df$SITE, "<br/>",
                    "<strong>Data Provider:</strong>", map.df$PROVIDERNAME, "<br/>",
                    "<strong>Latitude:</strong>", map.df$LATITUDE, "<br/>",
                    "<strong>Longitude:</strong>", map.df$LONGITUDE, "<br/>",
                    "<strong>Site Description:</strong>", map.df$SITE_NAME_EDIT)) %>%
    addLegend(position = "topright",
              title = "Legend",
              labels = c( "Selected Site", "Flow Gage", "All Sites"),
              colors = c("#E69F00", "#0072B2", "#999999"),
              opacity = 1)
  
  #addProviderTiles("Hydda.Full",
  #                 options = providerTileOptions(noWrap = TRUE)) %>%
  incProgress(1/3)
  return(leaflet.map)
  })
  #addMarkers(data = points.gage(), lng = long.gage, lat = lat.gage)
}) # End output$MAP