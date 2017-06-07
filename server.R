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
  #----------------------------------------------------------------------------
  # Selection and import script
  source("select_import.R", local = TRUE)
  #----------------------------------------------------------------------------
  # If switching to Query by Paramter
  source("query_by_param.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Query by Site
  source("query_by_site.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Side panel script excluding dropdown menues (See "select_import.R")
  source("sidepanel.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Tab Figures Script (Tab 1)
  source("plots.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Tab Data Script (Tab 2)
  source("dt.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Tab Maps Script (Tab 3)
  source("map.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Tab Metadata Script (Tab 4)
  source("metadata.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Tab Downloads Script (Tab 5)
  source("downloads.R", local = TRUE)
}) # End Shiny Server
