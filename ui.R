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
      selectInput("HUC_8", "HUC 8", choices = list.huc,
                  selected = "All HUCS", selectize = TRUE),
      selectInput("SITE", "Site", choices = unique(sites), selectize = TRUE),
      selectInput("PARAM", "Parameter", choices = "DO", selectize = TRUE),
      htmlOutput("ICPRB_UNITS"),
      htmlOutput("OUTLIERS"),
      htmlOutput("CENSORED"),
      htmlOutput("PARAM_LIST"),
      tags$hr(),
      
      tags$div(class = "header", checked = NA,
               tags$h3("Site Information")),
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
      tags$hr(),
      
      tags$div(class = "header", checked = NA,
               tags$h3("Nearest Gage Information")),
      htmlOutput("GAGE"),
      htmlOutput("GAGE.AGENCY"),
      htmlOutput("GAGE.LOC"),
      #htmlOutput("GAGE.LAT"),
      htmlOutput("GAGE.LONG"),
      tags$hr(),
      
      tags$div(class = "header", checked = NA,
               tags$h3("Download Data")),
      # style font family as well in addition to background and font color
      tags$head(tags$style(".button{background-color:#56B4E9;} .button{color: black;}
                           .button{font-family: Courier New;} .button{width: 250px;}
                           .button{text-align: left;}")),
      # .button{font-weight: bold;}
      
      fluidRow(p(downloadButton('plots.download', 'Download Plots', class = "button"))),
      
      
      fluidRow(p(downloadButton('param.tbl.download', 'Download Table', class = "button")))#,
      #fluidRow(p(downloadButton('map.download', 'Download Map', class = "button"))),
      #fluidRow(p(downloadButton('meta.download', 'Download Metadata', class = "button")))
      ),
    #==========================================================================
    mainPanel(tabsetPanel(
      #******************************************************************************
      # Tab Figures Script (Tab 1)
      #******************************************************************************
      tabPanel(
        "Figures",
        fluidRow(plotOutput("PLOTS", height = 1200, width = 1500))
        #fluidRow(plotOutput("LOESS", height = 400, width = 1530))#,
        #fluidRow(column(
        #  12, leafletOutput("mymap", height = 500, width = 1530), p()
        #))
      ),
      #******************************************************************************
      # Tab Data Script (Tab 2)
      #******************************************************************************
      tabPanel("Data",
               #tableOutput('tbl'))
               #column(11, dataTableOutput('param_table')),
               #column(1, p(class = 'text-center', downloadButton('param.tbl.download', 'Download Table')))
               
               fluidRow(dataTableOutput('param_table'))
      ),
      #******************************************************************************
      # Tab Map Script (Tab 3)
      #******************************************************************************
      tabPanel("Map",
               leafletOutput("mymap", height = 800, width = 1500)
      ),
      #******************************************************************************
      # Tab Metadata Script (Tab 4)
      #******************************************************************************
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
               
      )
    ))
    #==========================================================================
      )
  #==========================================================================
  
))
