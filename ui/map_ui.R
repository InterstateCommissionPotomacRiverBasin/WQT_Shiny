tabPanel("Map",
         tags$style(type="text/css",
                    "#mymap.recalculating { opacity: 1.0; }"),
         leafletOutput("mymap", height = 800, width = "100%"),
         tags$em("*Hold shift, click, and drag to utilize the box-zoom tool.")
) # End tabPanel