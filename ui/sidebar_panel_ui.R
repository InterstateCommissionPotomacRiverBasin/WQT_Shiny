sidebarPanel(
  uiOutput("fixed"),
  width = 2,
  #checkboxInput("check_site", "Site"),
  #checkboxInput("check_param", "Parameter"),
  radioButtons("query", "Query by:", c("Parameter", "Site")),
  #------------------------------------------------------------------------
  # Query by Parameter.
  conditionalPanel(
    condition = "input.query == 'Parameter'",
    selectInput("PARAM.param", "Parameter",
                choices = sort(unique(param.range$ICPRB_NAME)),
                selectize = TRUE, selected = "TEMP"),
    selectInput("HUC_8.param", "HUC 8", choices = list.huc,
                selected = "All HUCs", selectize = TRUE),
    selectInput("SITE.param", "Site", choices = unique(sites), selectize = TRUE)
  ), # End conditional panel for Parameter.
  #------------------------------------------------------------------------
  # Query by Site.
  conditionalPanel(
    condition = "input.query == 'Site'",
    selectInput("HUC_8.site", "HUC 8", choices = list.huc,
                selected = "All HUCs", selectize = TRUE),
    selectInput("SITE.site", "Site", choices = unique(sites), selectize = TRUE),
    selectInput("PARAM.site", "Parameter", choices = "TEMP", selectize = TRUE)
  ), # End conditional panel for Site.
  #------------------------------------------------------------------------
  htmlOutput("ICPRB_UNIT"),
  htmlOutput("OUTLIERS"),
  htmlOutput("CENSORED"),
  htmlOutput("PARAM_LIST"),
  tags$hr(), # End Dropdown section
  #------------------------------------------------------------------------
  tags$div(class = "header", checked = NA,
           tags$h3("Site Information"), align = "center"),
  htmlOutput("SEL_SITE"),
  htmlOutput("AGENCY"),
  htmlOutput("FIRST_DATE"),
  htmlOutput("LAST_DATE"),
  htmlOutput("STATE"),
  htmlOutput("LAT"),
  htmlOutput("LONG"),
  htmlOutput("DEPTH"),
  htmlOutput("REPLICATE"),
  htmlOutput("COMPOSITE"),
  tags$hr(), # End Site Information section
  #------------------------------------------------------------------------
  tags$div(class = "header", checked = NA,
           tags$h3("Gage Information"), align = "center"),
  htmlOutput("GAGE"),
  htmlOutput("GAGE.AGENCY"),
  htmlOutput("GAGE.LOC"),
  htmlOutput("GAGE.LAT"),
  htmlOutput("GAGE.LONG"),
  tags$hr(), # End Nearest Gage Information Section
  #------------------------------------------------------------------------
  tags$div(class = "header", checked = NA,
           tags$h3("Download"), align = "center"),
  # style font family as well in addition to background and font color
  tags$head(tags$style(".button{background-color:#56B4E9;} .button{color: black;}
                       .button{font-family: Courier New;} .button{width: 250px;}
                       .button{text-align: center;}")),
  # .button{font-weight: bold;}
  div( fluidRow(p(downloadButton('plots.download', 'Download Plots', class = "button"))),
       style = "text-align: center;"),
  div(fluidRow(p(downloadButton('param.tbl.download', 'Download Table', class = "button"))),
      style = "text-align: center;"),
  tags$hr(), # End Download Data Section
  #------------------------------------------------------------------------
  div(img(src = "epa_logo.png", height = 180, width = 182), style = "text-align: center;"),
  tags$div(tags$b("United States Environmental Protection Agency"), align = "center"),
  tags$div(em("This tool was developed with support from U. S. Environmental
              Protection Agency (CWA §106 I-98339413) and the Interstate
              Commission on the Potomac River Basin."), align = "center"),
  tags$hr(), # End Support Section
  #------------------------------------------------------------------------
  #tags$div(class = "header", checked = NA, tags$h3("ICPRB"), align = "center"),
  div(img(src = "logo.png", height = 220, width = 200), style = "text-align: center;"),
  tags$div(tags$b("Interstate Commission on the Potomac River Basin"), align = "center"),
  tags$div(em("To enhance, protect, and conserve the water and associated land resources of the
              Potomac River and its tributaries through regional and interstate cooperation."), align = "center"),
  tags$div(a("https://www.potomacriver.org",
             href = "https://www.potomacriver.org",
             target = "_blank"),
           align = "center") # End ICPRB section
  
  )