#============================================================================
# If switching to Query by Paramter.
#============================================================================
observeEvent(sel.param(), {
  updateSelectInput(session, "PARAM.param",
                    choices = sort(unique(param.range$ICPRB_NAME)),
                    selected = sel.param())
})
#----------------------------------------------------------------------------
observeEvent(c(sel.param(), input$query == "Parameter",
               sel.huc()), {
                 # Prevent red error message from appearing while data is loading.
                 #if(is.null(sel.param())) return(NULL)
                 req(sel.param())
                 unique.huc8 <- unique(huc8[huc8$ICPRB_NAME %in% sel.param(), "HUC_8"])
                 final.huc8 <- c("All HUCs", sort(unique.huc8))
                 
                 # When a new HUC is selected, the sites are modified to reflect only the 
                 # sites within the HUC. If the currently selected site was not in the newly
                 # selected HUC, then the sites are sorted in alphabetical order and the first
                 # site is  selected. However, if the currently selected site was in the newly
                 # selected HUC, the sites are sorted in alphabetical order but the currently
                 # selected site does not change.
                 if (sel.huc() %in% final.huc8) {
                   select.this <- input$HUC_8.param
                 } else {
                   select.this <- final.huc8[1]
                 }
                 
                 
                 observeEvent(sel.param(), {
                   updateSelectInput(session, "HUC_8.param",
                                     choices = final.huc8,
                                     selected = select.this)
                 })
                 
               })
#----------------------------------------------------------------------------
# Provide a dropdown menu with all of the available site selections.
# Filters according to the HUC8 selected.
site.react.param <- reactive({
  # Prevent red error message from appearing while data is loading.
  if (is.null(sel.huc())) {
    return(NULL)
  }   
  
  if (sel.huc() %in% "All HUCs") {
    final.vec <- sort(unique(huc8[huc8$ICPRB_NAME %in% sel.param(), "SITE"]))
  } else {
    final.vec <- sort(unique(huc8[huc8$HUC_8 %in% sel.huc() &
                                    huc8$ICPRB_NAME %in% sel.param(), "SITE"]))
  }
  
  return(final.vec)
}) # End site.react.site
#----------------------------------------------------------------------------
observeEvent(c(sel.huc(), sel.param()), {
  # Prevent red error message from appearing while data is loading.
  if(is.null(site.react.param())) return(NULL)
  final.site <- site.react.param()
  # When a new HUC is selected, the sites are modified to reflect only the 
  # sites within the HUC. If the currently selected site was not in the newly
  # selected HUC, then the sites are sorted in alphabetical order and the first
  # site is  selected. However, if the currently selected site was in the newly
  # selected HUC, the sites are sorted in alphabetical order but the currently
  # selected site does not change.
  if (sel.site() %in% final.site) {
    select.this <- sel.site()
  } else {
    select.this <- final.site[1]
  }
  #==========================================================================
  # The sites in the dropdown menu will only represent the sites within the
  # selected HUC.
  #==========================================================================
  updateSelectInput(session, "SITE.param",
                    choices = as.character(final.site),
                    selected = select.this)
}) # End observeEvent(c(input$HUC_8.site)