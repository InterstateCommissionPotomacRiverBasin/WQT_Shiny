#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Maintained: Zachary M. Smith
# Created: January 2017
# Updated: 2/28/17
#==============================================================================
#==============================================================================
shinyServer(function(input, output, session) {
  #----------------------------------------------------------------------------
  # Stop App when window is closed.
  session$onSessionEnded(stopApp)
  

  #----------------------------------------------------------------------------
  # Selection and import script
  source("server/select_import_server.R", local = TRUE)
  #----------------------------------------------------------------------------
  # If switching to Query by Paramter
  source("server/query_by_param_server.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Query by Site
  source("server/query_by_site_server.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Side panel script excluding dropdown menues (See "select_import.R")
  source("server/sidepanel_server.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Tab Figures Script (Tab 1)
  source("server/plots_server.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Tab Data Script (Tab 2)
  source("server/dt_server.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Tab Maps Script (Tab 3)
  source("server/map_server_dev2.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Tab Metadata Script (Tab 4)
  source("server/metadata_server.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Tab Downloads Script (Tab 5)
  source("server/downloads_server.R", local = TRUE)
  #----------------------------------------------------------------------------
  # Tab References Script (Tab 6)
  source("server/relevant_pubs_server.R", local = TRUE)
  
  outputOptions(output, "mymap", suspendWhenHidden = FALSE)

#  lapply(c("PLOTS", "param_table", #"mymap",
           #"META_OUTLIER_INFO", "PARAM_STAND_LOOP",
           #"SITE_INFO_LOOP", "ALL_DEPTH",
#           "relevant_pubs_table"),
#         function(x) outputOptions(output, x, suspendWhenHidden = FALSE))
}) # End Shiny Server
