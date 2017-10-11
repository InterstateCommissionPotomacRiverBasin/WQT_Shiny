#==========================================================================
# Identifying Outliers Heading
#==========================================================================
observeEvent(c(input$SITE.site, input$PARAM.site), {
  # Prevent red error message from appearing while data is loading.
  req(param.react(),
      param.tbl())
  sub.param <- param.react()
  
  final.param <- as.character(sub.param) %>% 
    sort() %>% 
    unique()
  
  # When a new site is selected, the parameters are modified to reflect only
  # the parameters that have been measured at that site. If the currently
  # selected parameter was not measured at the newly selected site, then the
  # parameters are sorted in alphabetical order and the first parameter is 
  # selected. However, if the currently selected parameter was also measured
  # at the newly selected site, the parametes are sorted in alphabetical order
  # but the currently selected parameter does not change.
  if (input$PARAM.site %in% final.param) {
    select.this <- input$PARAM.site
  } else {
    select.this <- final.param[1]
  }
param.df <- param.tbl()
sub.param <- param.df %>% 
  filter(SITE %in% input$SITE.site,
         ICPRB_NAME %in% input$PARAM.site)

sub.outliers <- outliers %>% 
  filter(PARAMETER %in% input$PARAM.site)
#--------------------------------------------------------------------------
outlier.count <- sub.param %>% 
  filter(REPORTED_VALUE >= sub.outliers$UP_FENCE_4.5,
         REPORTED_VALUE <= sub.outliers$LOW_FENCE_4.5) %>% 
  nrow()

#--------------------------------------------------------------------------
output$META_OUTLIER_INFO <- renderUI({
  HTML(paste(paste("<strong>Lower Fence:</strong>", unique(sub.outliers$LOW_FENCE_4.5), sep = " "),
             paste("<strong>Upper Fence:</strong>", unique(sub.outliers$UP_FENCE_4.5), sep = " "),
             paste("<strong>Outliers Removed:</strong>", outlier.count, sep = " "),
             "<br/>", sep = "<br/>"))
  
}) #End output$MEAT_OUTLIER_INFO
#==========================================================================
# Parameter Standardization Heading
#==========================================================================
sub.param_stand <- lapply(unique(sub.param$PROVIDERNAME), function(x){
  if (is.null(sub.param)) return(NULL)
  keep.cols <- c("PROVIDERNAME", "PARAMETER",
                 "REPORTED_UNITS", "USGSPCODE",
                 "DETECTIONQUANTITATIONLIMITTYPENAME",
                 "DETECTIONQUANTITATIONLIMITMEASURE_MEASUREVALUE",
                 "DETECTIONQUANTITATIONLIMITMEASURE_MEASUREUNITCODE",
                 "ICPRB_CONVERSION")
  keep.cols[!keep.cols %in% names(sub.param)]
  
  return(unique(sub.param[sub.param$PROVIDERNAME %in% x, keep.cols]))

}) # End sub.param_stand
#--------------------------------------------------------------------------
output$PARAM_STAND_LOOP <- renderUI({
  #uni.list <- uni.func(sub.param$PARAMETER)
  #test.list <- list()
  req(param.react(), sub.param_stand)
  param.list <- lapply(1:length(sub.param_stand), function(i) {
    
    sub.i <- data.frame(sub.param_stand[i], stringsAsFactors = FALSE)
    sub.i$FINAL_DETECT <- paste(sub.i$DETECTIONQUANTITATIONLIMITMEASURE_MEASUREVALUE, 
                                sub.i$DETECTIONQUANTITATIONLIMITMEASURE_MEASUREUNITCODE, sep = " ")
    #----------------------------------------------------------------------
    detect.cols <- c("DETECTIONQUANTITATIONLIMITMEASURE_MEASUREVALUE",
                     "DETECTIONQUANTITATIONLIMITMEASURE_MEASUREUNITCODE")
    detect.list <- lapply(1:nrow(unique(sub.i[, detect.cols])), function(j) {
      detect.this <- unique(sub.i[, c(detect.cols)])
      sub.j <- data.frame(detect.this[j, ], stringsAsFactors = FALSE)
      final.df <- paste(sub.j$DETECTIONQUANTITATIONLIMITMEASURE_MEASUREVALUE, 
                        sub.j$DETECTIONQUANTITATIONLIMITMEASURE_MEASUREUNITCODE, sep = " ")
      return(final.df)
    }) # End detec.list
    detect.final <- do.call(rbind, detect.list)
    #----------------------------------------------------------------------
    final.i <- paste(paste("<strong>Data Provider:</strong>", uni.func(sub.i$PROVIDERNAME)), 
                     paste("<strong>Reported Parameter:</strong>",
                           uni.func(sub.i$PARAMETER), sep = " "),
                     paste("<strong>Reported Units:</strong>", uni.func(sub.i$REPORTED_UNITS), sep = " "),
                     paste("<strong>USGS Code:</strong>", uni.func(sub.i$USGSPCODE), sep = " "),
                     paste("<strong>Detection Limit Type:</strong>", uni.func(sub.i$DETECTIONQUANTITATIONLIMITTYPENAME), sep = " "),
                     paste("<strong>Detection Limit Value:</strong>", uni.func(sub.i$FINAL_DETECT), sep = " "),
                     paste("<strong>Conversion Applied:</strong>", uni.func(sub.i$ICPRB_CONVERSION), sep = " "),
                     
                     #paste(uni.func(sub.i$DETECTIONQUANTITATIONLIMITMEASURE_MEASUREVALUE),
                     #uni.func(sub.i$DETECTIONQUANTITATIONLIMITMEASURE_MEASUREUNITCODE)),
                     
                     
                     "<br/>", sep = "<br/>")
  }) # End param.list
  final.test <- do.call(rbind, param.list)
  HTML(paste(final.test[, 1], sep = "<br/>"))
}) # End output$PARAM_STAND_LOOP

#output$PARAM_REPORTED <- renderUI({
#  uni.list <- uni.func(sub.param$PARAMETER)
#  HTML(paste("<strong>Reported Parameter:</strong>", uni.list, sep = " "))
#})

#==========================================================================
# Site Information Heading
#==========================================================================
sub.site_info <- lapply(unique(sub.param$PROVIDERNAME), function(x){
  keep.cols <- c("PROVIDERNAME", "AGENCY_NAME", "SITE_NAME", "DEPTH",
                 "ACTIVITYMEDIANAME", "ACTIVITY_TYPE",
                 "SAMPLECOLLECTIONMETHOD_METHODIDENTIFIER",
                 "SAMPLECOLLECTIONMETHOD_METHODIDENTIFIERCONTEXT")
  keep.cols[!keep.cols %in% names(sub.param)]
  
  return(unique(sub.param[sub.param$PROVIDERNAME %in% x, keep.cols]))
  
}) # End sub.site_info
#--------------------------------------------------------------------------
output$SITE_INFO_LOOP <- renderUI({
  site.list <- lapply(1:length(sub.site_info), function(i) {
    sub.i <- data.frame(sub.site_info[i], stringsAsFactors = FALSE)
    sub.vec <- paste(paste("<strong>Data Provider:</strong>", uni.func(sub.i$PROVIDERNAME)), 
                     paste("<strong>Agency Formal Name:</strong>", uni.func(sub.i$AGENCY_NAME)),  
                     paste("<strong>Site Name:</strong>", uni.func(sub.i$SITE_NAME)), 
                     paste("<strong>Activity Name:</strong>", uni.func(sub.i$ACTIVITYMEDIANAME)),
                     paste("<strong>Activity Type Code:</strong>", uni.func(sub.i$ACTIVITY_TYPE)),
                     "<br/>", sep = "<br/>")
    return(sub.vec)
  }) # End site.list
  final.vec <- do.call(rbind, site.list)
  HTML(paste(final.vec[,1], sep = "<br/>"))
}) # End output$SITE_INFO_LOOP
})

#--------------------------------------------------------------------------
output$ALL_DEPTH <- renderUI({
  all.depth <- as.vector(depth.react())
  uni.list <- uni.func(all.depth)
  HTML(paste("<strong>Depth (m):</strong>", uni.list, sep = " "))
}) # End output$DEPTH