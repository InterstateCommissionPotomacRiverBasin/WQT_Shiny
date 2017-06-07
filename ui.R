#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Maintained: Zachary M. Smith
# Created: January 2017
# Updated: 2/28/17
#==============================================================================
#==============================================================================

library(shiny)
library(shinythemes)
library(leaflet)
library(rsconnect)
library(DT)

shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("EPA Region 3 Water Quality Trends"),
  sidebarLayout(
    #******************************************************************************
    # Sidebar Script
    #******************************************************************************
    sidebarPanel(
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
      htmlOutput("ICPRB_UNITS"),
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
               tags$h3("Download Data"), align = "center"),
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
   
      ), 

    #==========================================================================
    mainPanel(tabsetPanel(
      #************************************************************************
      # Tab Figures Script (Tab 1)
      #************************************************************************
      tabPanel(
        "Figures",
        fluidRow(plotOutput("PLOTS", height = 1200, width = 1500))
        #fluidRow(plotOutput("LOESS", height = 400, width = 1530))#,
        #fluidRow(column(
        #  12, leafletOutput("mymap", height = 500, width = 1530), p()
        #))
      ),
      #************************************************************************
      # Tab Data Script (Tab 2)
      #************************************************************************
      tabPanel("Data",
               #tableOutput('tbl'))
               #column(11, dataTableOutput('param_table')),
               #column(1, p(class = 'text-center', downloadButton('param.tbl.download', 'Download Table')))
               
               fluidRow(dataTableOutput('param_table'))
      ),
      #************************************************************************
      # Tab Map Script (Tab 3)
      #************************************************************************
      tabPanel("Map",
               leafletOutput("mymap", height = 800, width = 1500)
      ),
      #************************************************************************
      # Tab Metadata Script (Tab 4)
      #************************************************************************
      tabPanel("Metadata",
               tags$hr(),
               tags$div(class = "header", checked = NA,
                        tags$h3("Identifying Outliers")),
               "All of data within the database was divided into subsets
               based on parameter. For each parameter subset, the Interquartile 
               Range (IQR) was multiplied by 4.5 to obtain a standard 
               value for assigning outlier fences. The standard value was 
               subtracted from the lower quartile, to define the lower outlier
               fence, and added to the upper quartile, to define the upper 
               outlier fence.",
               
               htmlOutput("META_OUTLIER_INFO"),
               tags$hr(),
               tags$div(class = "header", checked = NA,
                        tags$h3("Censored Data")),
               "Censored data refers to data that was above or below a detection limit.",
               tags$hr(),
               tags$div(class = "header", checked = NA,
                        tags$h3("Parameter Standardization")),
               htmlOutput("PARAM_STAND_LOOP"),
               
               tags$hr(),
               tags$div(class = "header", checked = NA,
                        tags$h3("Site Information")),
               htmlOutput("SITE_INFO_LOOP")
               
      ),
      #************************************************************************
      # Tab Downloads Script (Tab 5)
      #************************************************************************
      tabPanel("Download",
               div(fluidRow(p(selectInput("huc8.download", "HUC 8",
                           choices = sort(unique(param.range$ICPRB_NAME)),
                           selectize = TRUE, selected = "All HuCs"))),
                   style = "text-align: center;"),
               div(fluidRow(p(selectInput("site.download", "Site", choices = list.huc,
                           selected = "All Sites", selectize = TRUE))),
                   style = "text-align: center;"),
               div(fluidRow(p(selectInput("param.download", "Parameter",
                                          choices = unique(param.range$ICPRB_NAME),
                                          selectize = TRUE))),
                   style = "text-align: center;")
  
               
      )
    ))
    #==========================================================================
      )
  #==========================================================================
  
))
