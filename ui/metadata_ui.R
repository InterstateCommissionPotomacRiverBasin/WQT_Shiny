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
         outlier fence. Parameter values below the lower fence or 
         above the upper fence were omitted from the Figures tab. 
         The remaining tabs are not influenced by the removal of outliers.",
         
         htmlOutput("META_OUTLIER_INFO"),
         tags$hr(),
         tags$div(class = "header", checked = NA,
                  tags$h3("Censored Data")),
         "Censored data refers to data that was below a detection limit. 
         If the parameter value was indicated as below detection limit, 
         then the parameter value was represented by half the value of 
         the detection limit.  Any site/parameter where more than 50% censored 
         data, was omitted from the database.  These steps occurred during 
         database organization, and therefore, have the potential to influence 
         information in the sidebar or any of the tabs.",
         tags$hr(),
         tags$div(class = "header", checked = NA,
                  tags$h3("Parameter Standardization")),
         htmlOutput("PARAM_STAND_LOOP"),
         
         tags$hr(),
         tags$div(class = "header", checked = NA,
                  tags$h3("Site Information")),
         htmlOutput("SITE_INFO_LOOP"), 
         tags$hr(),
         tags$div(class = "header", checked = NA,
                  tags$h3("Depth")),
         "For this analysis, only samples collected at the surface (0 m)
               or near the surface (<= 1 m) were utilized. Samples with missing
               depth values were assumed to be collected at the surface. This
               assumption may not always be correct. Therefore, the user should 
               be cautious whenever the depth is reported as ",tags$em('"Blank"'), ".",
         br(),
         br(),
         "Depths greater than 1 m were excluded to create a more 
               standardized output for all stations and parameters. Including 
               all samples collected at depths below 1 m may skew the data.",
         br(),
         br(),
         "Below are all of the depths found in the database. When the 
               data table is downloaded all of the available depths will be 
               included.",
         br(),
         htmlOutput("ALL_DEPTH")
) # End tabPanel