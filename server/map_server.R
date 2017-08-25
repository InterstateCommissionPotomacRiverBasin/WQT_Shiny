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
  names(final.df) <- c("SITE", "LATITUDE", "LONGITUDE", "SITE_NAME")
  final.df$PROVIDER <- "USGS"
  final.df[, 2:3] <- sapply(final.df[, 2:3], as.numeric)
  #final.df$GAGE <- TRUE
  final.df$TYPE <- "gage"
  return(final.df)
}, ignoreNULL = FALSE) # End points.gage
#============================================================================ 
# Plot the Site on the map.
output$mymap <- renderLeaflet({
  #--------------------------------------------------------------------------
  # Create a seperate table in the data base, so that this moves faster.
  map.df <- dbGetQuery(pool, paste(
    'SELECT DISTINCT "ICPRB_NAME", "SITE", "LATITUDE", "LONGITUDE", "SITE_NAME_EDIT"',
    'FROM "wq_data"',
    'WHERE "ICPRB_NAME" =', paste0("'", sel.param(), "'"))) %>% 
    dplyr::mutate(TYPE = "base")
 # map.df$TYPE <- "base"
#   map.df <- map.points.react() %>% 
#     dplyr::mutate(TYPE = "base")
  #--------------------------------------------------------------------------
#  map.df <- dplyr::bind_rows(all.points, points(), points.gage()) %>% 
#    mutate(TYPE = factor(TYPE, levels = c("base", "selected", "gage")))
  
  #map.df$LATITUDE <- jitter(map.df$LATITUDE, factor = 0.000001)
  #map.df$LONGITUDE <- jitter(map.df$LONGITUDE, factor = 0.000001)
  #longitude <- mean(as.numeric(points()$LONGITUDE), na.rm = TRUE)
  #latitude <- mean(as.numeric(points()$LATITUDE), na.rm = TRUE)
  #long.gage <- mean(points.gage()$LONG_DD, na.rm = TRUE)
  #lat.gage <- mean(points.gage()$LAT_DD, na.rm = TRUE)
  icprb.map <- "https://api.mapbox.com/styles/v1/skaisericprb/cizok18ny00302spia5zhre3o/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2thaXNlcmljcHJiIiwiYSI6ImNpa2U3cGN1dDAwMnl1cm0yMW94bWNxbDEifQ.pEG_X7fqCAowSN8Xr6rX8g"
  
  pal <- colorFactor(c("#999999", "#E69F00", "#0072B2"),
                     domain = c("base", "selected", "gage"))
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = ~pal(map.df$TYPE)
  )
  
  
  
  leaflet() %>%
    #addTiles() %>%
    addTiles(urlTemplate = icprb.map, options = tileOptions(minZoom = 7, maxZoom = 18)) %>%
    setMaxBounds(lng1 = -90, lat1 = 34, lng2 = -64, lat2 = 45) %>%
    #setView(lng = -77.5, lat = 39.65305556, zoom = 7) %>% 
    #setView(lng = longitude, lat = latitude, zoom = 7) %>%
    addCircleMarkers(
      data = map.df[, c("LATITUDE", "LONGITUDE")],
      #lng = longitude, lat = latitude,
      fillColor = ~pal(map.df$TYPE),
      fill = TRUE,
      stroke = FALSE,
      color = ~pal(map.df$TYPE),
      weight = 3,
      fillOpacity = 1,
      popup = paste("<strong>Site:</strong>", map.df$SITE, "<br/>",
                    "<strong>Data Provider:</strong>", map.df$PROVIDER, "<br/>",
                    "<strong>Latitude:</strong>", map.df$LATITUDE, "<br/>",
                    "<strong>Longitude:</strong>", map.df$LONGITUDE, "<br/>",
                    "<strong>Site Description:</strong>", map.df$SITE_NAME)) %>%
    addLegend(position = "topright",
              title = "Legend",
              labels = c( "Selected Site", "Flow Gage", "All Sites"),
              colors = c("#E69F00", "#0072B2", "#999999"),
              opacity = 1)
  
    #addProviderTiles("Hydda.Full",
    #                 options = providerTileOptions(noWrap = TRUE)) %>%
   
  
  #addMarkers(data = points.gage(), lng = long.gage, lat = lat.gage)
}) # End output$MAP