raw.df <- dbGetQuery(conn, paste(
  'SELECT * FROM',  paste0('"site_', "GEO0009", '"'),
  'WHERE "ICPRB_NAME" =', paste0("'", "PH", "'"))) %>%  
  filter(REPORTED_VALUE < 11)

gage.df <- dbGetQuery(conn, paste(
  'SELECT * FROM "gage_flow"',
  'WHERE "GAGE_ID" =', paste0("'1599000'")))

raw.loess.plot <- raw_loess_plot(raw.df)
flow.correct.loess.plot <- flow_correct_loess_plot(raw.df, gage.df)

ggsave(raw.loess.plot, "GEO0009_ph.png", width = 1500)
ggsave("GEO0009_ph.png", raw.loess.plot, width = 18, height = 5.335, dpi = 300)
ggsave("GEO0009_ph_fc.png", flow.correct.loess.plot, width = 18, height = 5.335, dpi = 300)


site.vec <- c("NBP0689", "NBP0534", "SAV0000", "GEO0009", "NBP0461")

nbp.df <- purrr::map(site.vec, function(site.i) {
  dbGetQuery(conn, paste(
    'SELECT * FROM',  paste0('"site_', site.i, '"')))
}) %>% 
  bind_rows() %>% 
  select(SITE, SITE_NAME_EDIT, AGENCY, LATITUDE, LONGITUDE) %>% 
  distinct() %>% 
  filter(AGENCY == "MDDNR",
         SITE != "SAV0000")

leaflet::leaflet() %>%
  addProviderTiles(providers$Hydda.Full, options = tileOptions(minZoom = 7, maxZoom = 18)) %>%
  #leaflet::addTiles(urlTemplate = icprb.map, options = tileOptions(minZoom = 7, maxZoom = 18)) %>%
  leaflet::setMaxBounds(lng1 = -90, lat1 = 34, lng2 = -64, lat2 = 45) %>%
  leaflet::setView(lng = -77.5, lat = 39.65305556, zoom = 7) %>% 
  leaflet::addCircleMarkers(
                            lng = nbp.df$LONGITUDE,
                            lat = nbp.df$LATITUDE,
                            fill = "#0072B2",
                            fillOpacity = 0.8,
                            stroke = FALSE,
                            data = nbp.df[, c("LATITUDE", "LONGITUDE")],
                            popup = paste("<strong>Site:</strong>", nbp.df$SITE, "<br/>",
                                          "<strong>Agency:</strong>", nbp.df$AGENCY, "<br/>",
                                          "<strong>Latitude:</strong>", nbp.df$LATITUDE, "<br/>",
                                          "<strong>Longitude:</strong>", nbp.df$LONGITUDE, "<br/>",
                                          "<strong>Site Description:</strong>", nbp.df$SITE_NAME_EDIT))
