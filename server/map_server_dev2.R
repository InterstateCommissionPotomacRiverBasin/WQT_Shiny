points <- reactive({
  # Prevent red error message from appearing while data is loading.
  req(param.tbl())
  #sites <- 
  final.df <- param.tbl() %>% 
    select(SITE, LATITUDE, LONGITUDE,
           ProviderName, SITE_NAME_EDIT) %>% 
    rename(PROVIDER = ProviderName,
           SITE_NAME = SITE_NAME_EDIT) %>% 
    mutate(TYPE = "selected")

  return(final.df)
}) # End points
#----------------------------------------------------------------------------
# Exract Lat/Long of selected site for plotting.
points.gage <- reactive({
  # Prevent red error message from appearing while data is loading.
  if(is.null(gage.info.react())) return(NULL)
  sites <- gage.info.react()
  
  final.df <- sites %>% 
    select(GAGE_ID, LAT_DD, LONG_DD, GAGE_NAME) %>% 
    rename(SITE = GAGE_ID,
           LATITUDE = LAT_DD,
           LONGITUDE = LONG_DD,
           SITE_NAME_EDIT = GAGE_NAME) %>% 
    mutate(PROVIDER = "USGS",
           ICPRB_NAME = NA, 
           HUC_8 = NA,
           PROVIDERNAME = "USGS",
           TYPE = "Gage", 
           LATITUDE = as.numeric(LATITUDE),
           LONGITUDE = as.numeric(LONGITUDE))

  return(final.df)
}) # End points.gage

#------------------------------------------------------------------------------
map.reac <- reactive({#eventReactive(c(sel.param(), sel.site()), {
  #req(param.tbl(), sel.param(), sel.huc())

  if (sel.huc() == "All HUCs") {
    huc <- huc8 %>% 
      dplyr::filter(ICPRB_NAME == sel.param())

  } else {
    huc <- huc8 %>% 
      dplyr::filter(ICPRB_NAME == sel.param(),
                    HUC_8 == sel.huc())
  }
  
  #--------------------------------------------------------------------------
  # Create a seperate table in the data base, so that this moves faster.
  map.df <- huc %>% 
    mutate(TYPE = if_else(SITE == sel.site(),
                          "Selected Site", "Other Sites"))
  
  if (!is.null(points.gage())) {
    map.df <- map.df %>% 
      dplyr::bind_rows(points.gage())
  }
  final.df <- map.df %>% 
    mutate(TYPE = factor(TYPE, levels = c("Other Sites", "Gage", "Selected Site"))) %>% 
    arrange(TYPE)

  return(final.df)
  
})

#============================================================================ 
# Generate Map.

icprb.map <- "https://api.mapbox.com/styles/v1/skaisericprb/cizok18ny00302spia5zhre3o/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2thaXNlcmljcHJiIiwiYSI6ImNpa2U3cGN1dDAwMnl1cm0yMW94bWNxbDEifQ.pEG_X7fqCAowSN8Xr6rX8g"
epa3.states <- maps::map("state", fill = TRUE) %>% 
  fortify(region = "region") %>% 
  filter(region %in% c("delaware", "district of columbia", "maryland",
                       "pennsylvania", "virginia", "west virginia"))
#--------------------------------------------------------------------------

output$mymap <- renderLeaflet({
leaflet.map <- leaflet::leaflet() %>%
  #addProviderTiles(providers$OpenStreetMap.Mapnik, options = tileOptions(minZoom = 7, maxZoom = 18)) %>%
  leaflet::addTiles(urlTemplate = icprb.map, options = tileOptions(minZoom = 7, maxZoom = 18)) %>%
  leaflet::setMaxBounds(lng1 = -90, lat1 = 34, lng2 = -64, lat2 = 45) %>%
  leaflet::setView(lng = -77.5, lat = 39.65305556, zoom = 7) %>% 
  leaflet::addLegend(position = "topright",
                     title = "Legend",
                     labels = c( "Selected Site", "Flow Gage", "All Sites"),
                     colors = c("#E69F00", "#0072B2", "#999999"),
                     opacity = 1,
                     labFormat = labelFormat(style = list(
                       "color" = "red",
                       "font-family" = "serif",
                       "font-style" = "italic",
                       "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                       "font-size" = "12px",
                       "border-color" = "rgba(0,0,0,0.5)"
                     )))
})

#============================================================================ 
# Plot the Site on the map.
observeEvent(map.reac(), {
  #observe({
  #invalidateLater(1000)
  map.df <- map.reac()
  print(head(map.df))
  proxy <- leafletProxy("mymap") %>%
    clearMarkers() %>%
    leaflet::addCircleMarkers(
      color = ~case_when(map.df$TYPE == "Other Sites" ~ "#999999",
                         map.df$TYPE == "Gage" ~ "#0072B2",
                         map.df$TYPE == "Selected Site" ~ "#E69F00",
                         TRUE ~ "pink"),
      radius = ~case_when(map.df$TYPE == "Other Sites" ~ 6,
                          map.df$TYPE == "Gage" ~ 8,
                          map.df$TYPE == "Selected Site" ~ 10,
                          TRUE ~ 25),
      opacity = ~case_when(map.df$TYPE == "Other Sites" ~ 1,
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
                    "<strong>Site Description:</strong>", map.df$SITE_NAME_EDIT))
  
  return(proxy)
  
}, ignoreNULL = FALSE, ignoreInit = FALSE, priority = 1)
#  })
#============================================================================ 
# use point symbols from base R graphics as icons
pchIcons <- function(pch = 0:14, width = 30, height = 30, col = 1:15, ...) {
  pchLength <- length(pch)
  pchFiles <- character(pchLength)
  
  # create a sequence of png images
  for (i in seq_len(pchLength)) {
    pchTempFile <- tempfile(fileext = '.png')
    png::png(pchTempFile, width = width, height = height, bg = 'transparent')
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = pch[i], cex = 1.5, col = col[i], ...)
    dev.off()
    pchFiles[i] = pchTempFile
  }
  pchFiles
}