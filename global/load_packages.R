# Load packages using script copied from:
# https://www.r-bloggers.com/install-and-load-missing-specifiedneeded-packages-on-the-fly/
# needed packages for a job
need <- c("shiny",
          "shinythemes",
          "ggplot2",
          "gridExtra",
          "grid",
          "dplyr",
          #"DBI",
          "RPostgreSQL",
          #"pool",
          "data.table",
          "DT",
          #"Cairo",
          #"keyring",
          "png",
          "leaflet")#,
          #"rsconnect"
          #)#,
          #"RcppRoll",
          #"zoo") 
# load the needed packages
eval(parse(text = paste("library(", need, ")")))
#devtools::install_github("rstudio/pool")
#------------------------------------------------------------------------------
options(shiny.usecairo = TRUE)