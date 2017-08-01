#============================================================================
# Query by Site
#============================================================================
# If switching to Query by Site.
#  observeEvent(sel.param(), {
#    updateSelectInput(session, "PARAM.site",
#                      choices = sort(unique(param.range$ICPRB_NAME)),
#                      selected = input$PARAM.param)
#  })
#----------------------------------------------------------------------------
observeEvent(c(sel.huc(), sel.param()), {
  updateSelectInput(session, "HUC_8.site",
                    choices = c("All HUCs", sort(unique(huc8$HUC_8))),
                    selected = sel.huc())
})
#----------------------------------------------------------------------------
#  observeEvent(sel.site(), {
#    updateSelectInput(session, "SITE.site",
#                      choices = input$SITE.param,
#                      selected = input$SITE.param)
#  })
#----------------------------------------------------------------------------
# Provide a dropdown menu with all of the available site selections.
# Filters according to the HUC8 selected.
site.react.site <- reactive({
  # Prevent red error message from appearing while data is loading.
  if (is.null(sel.huc())) {
    return(NULL)
  }   
  if (sel.huc() %in% "All HUCs") {
    final.vec <- sort(unique(huc8$SITE))
  } else {
    final.vec <- sort(unique(huc8[huc8$HUC_8 %in% sel.huc(), "SITE"]))
  }
  
  return(final.vec)
}) # End site.react.site
#----------------------------------------------------------------------------
observeEvent(c(sel.huc(), sel.site()), {
  # Prevent red error message from appearing while data is loading.
  if(is.null(site.react.site())) return(NULL)
  final.site <- site.react.site()
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
  updateSelectInput(session, "SITE.site",
                    choices = as.character(final.site),
                    selected = select.this)
}) # End observeEvent(c(input$HUC_8.site)


observeEvent(c(sel.site(), sel.param()), {
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
  if (sel.param() %in% final.param) {
    select.this <- sel.param()
  } else {
    select.this <- final.param[1]
  }
  #============================================================================= 
  # The parameters in the dropdown menu only represent parameters measured
  # at the selected site.
  #=============================================================================
  updateSelectInput(session, "PARAM.site",
                    choices = as.character(final.param),
                    selected = select.this)
})