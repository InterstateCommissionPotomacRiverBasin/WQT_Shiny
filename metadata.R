#==========================================================================
# Identifying Outliers Heading
#==========================================================================
observeEvent(c(input$SITE.site, input$PARAM.site), {
  # Prevent red error message from appearing while data is loading.
  if(is.null(param.react())) return(NULL)
  sub.param <- param.react()
  
  final.param <- unique(sort(as.character(sub.param)))
  
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
#sub.param <- param.tbl()[param.tbl()$SITE %in% input$SITE.site & param.tbl()$PARAMETER %in% input$PARAM.site, ]
sub.param <- param.tbl()[param.tbl()$SITE %in% input$SITE.site & 
                           param.tbl()$ICPRB_NAME %in% input$PARAM.site, ]
sub.outliers <- outliers[outliers$PARAMETER %in% input$PARAM.site, ]
#--------------------------------------------------------------------------
outlier.count <- nrow(sub.param[sub.param$REPORTED_VALUE >= sub.outliers$UP_FENCE_4.5 |
                                  sub.param$REPORTED_VALUE <= sub.outliers$LOW_FENCE_4.5, ])
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
sub.param_stand <- lapply(unique(sub.param$ProviderName), function(x){
  keep.cols <- c("ProviderName", "PARAMETER",
                 "REPORTED_UNITS", "USGSPCode",
                 "DetectionQuantitationLimitTypeName",
                 "DetectionQuantitationLimitMeasure.MeasureValue",
                 "DetectionQuantitationLimitMeasure.MeasureUnitCode",
                 "ICPRB_CONVERSION")
  keep.cols[!keep.cols %in% names(sub.param)]
  
  return(unique(sub.param[sub.param$ProviderName %in% x, keep.cols]))
  
}) # End sub.param_stand
#--------------------------------------------------------------------------
output$PARAM_STAND_LOOP <- renderUI({
  #uni.list <- uni.func(sub.param$PARAMETER)
  #test.list <- list()
  param.list <- lapply(1:length(sub.param_stand), function(i) {
    
    sub.i <- data.frame(sub.param_stand[i], stringsAsFactors = FALSE)
    sub.i$final.detect <- paste(sub.i$DetectionQuantitationLimitMeasure.MeasureValue, 
                                sub.i$DetectionQuantitationLimitMeasure.MeasureUnitCode, sep = " ")
    #----------------------------------------------------------------------
    detect.cols <- c("DetectionQuantitationLimitMeasure.MeasureValue",
                     "DetectionQuantitationLimitMeasure.MeasureUnitCode")
    detect.list <- lapply(1:nrow(unique(sub.i[, detect.cols])), function(j) {
      detect.this <- unique(sub.i[, c(detect.cols)])
      sub.j <- data.frame(detect.this[j, ], stringsAsFactors = FALSE)
      final.df <- paste(sub.j$DetectionQuantitationLimitMeasure.MeasureValue, 
                        sub.j$DetectionQuantitationLimitMeasure.MeasureUnitCode, sep = " ")
      return(final.df)
    }) # End detec.list
    detect.final <- do.call(rbind, detect.list)
    #----------------------------------------------------------------------
    final.i <- paste(paste("<strong>Data Provider:</strong>", uni.func(sub.i$ProviderName)), 
                     paste("<strong>Reported Parameter:</strong>",
                           uni.func(sub.i$PARAMETER), sep = " "),
                     paste("<strong>Reported Units:</strong>", uni.func(sub.i$REPORTED_UNITS), sep = " "),
                     paste("<strong>USGS Code:</strong>", uni.func(sub.i$USGSPCode), sep = " "),
                     paste("<strong>Detection Limit Type:</strong>", uni.func(sub.i$DetectionQuantitationLimitTypeName), sep = " "),
                     paste("<strong>Detection Limit Value:</strong>", uni.func(sub.i$final.detect), sep = " "),
                     paste("<strong>Conversion Applied:</strong>", uni.func(sub.i$ICPRB_CONVERSION), sep = " "),
                     
                     #paste(uni.func(sub.i$DetectionQuantitationLimitMeasure.MeasureValue),
                     #uni.func(sub.i$DetectionQuantitationLimitMeasure.MeasureUnitCode)),
                     
                     
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
sub.site_info <- lapply(unique(sub.param$ProviderName), function(x){
  keep.cols <- c("ProviderName", "AGENCY_NAME", "SITE_NAME", "DEPTH",
                 "ActivityMediaName", "ACTIVITY_TYPE",
                 "SampleCollectionMethod.MethodIdentifier",
                 "SampleCollectionMethod.MethodIdentifierContext")
  keep.cols[!keep.cols %in% names(sub.param)]
  
  return(unique(sub.param[sub.param$ProviderName %in% x, keep.cols]))
  
}) # End sub.site_info
#--------------------------------------------------------------------------
output$SITE_INFO_LOOP <- renderUI({
  site.list <- lapply(1:length(sub.site_info), function(i) {
    sub.i <- data.frame(sub.site_info[i], stringsAsFactors = FALSE)
    sub.vec <- paste(paste("<strong>Data Provider:</strong>", uni.func(sub.i$ProviderName)), 
                     paste("<strong>Agency Formal Name:</strong>", uni.func(sub.i$AGENCY_NAME)),  
                     paste("<strong>Site Name:</strong>", uni.func(sub.i$SITE_NAME)), 
                     paste("<strong>Activity Name:</strong>", uni.func(sub.i$ActivityMediaName)),
                     paste("<strong>Activity Type Code:</strong>", uni.func(sub.i$ActivityTypeCode)),
                     "<br/>", sep = "<br/>")
    return(sub.vec)
  }) # End site.list
  final.vec <- do.call(rbind, site.list)
  HTML(paste(final.vec[,1], sep = "<br/>"))
}) # End output$SITE_INFO_LOOP
})