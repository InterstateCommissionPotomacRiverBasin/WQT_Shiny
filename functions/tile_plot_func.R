tile_plot <- function(plot.me, param.range){
  # Use data.table functions to find the mean of reporting value. More than
  # one value per month/year/parameter would cause this function to fail
  # at a later stage.
  plot.me <- as.data.frame(plot.me)
  plot.me <- plot.me[, !names(plot.me) %in% "CENSORED"]
  long.df <- data.table::data.table(plot.me)
  #long.df <- plot.me[, "CENSORED":= NULL]
  plot.me <- unique(long.df[, REPORTED_VALUE := mean(REPORTED_VALUE),
                            by = list(SITE, MONTH, YEAR, #PARAMETER,
                                      ICPRB_NAME, ICPRB_UNITS)])
  #============================================================================
  param.range <- param.range[param.range$ICPRB_NAME%in% plot.me$ICPRB_NAME, ]
  median.value <- median(plot.me$REPORTED_VALUE, na.rm = TRUE)
  #----------------------------------------------------------------------------
  if (median.value >= 5) {
    plot.me$REPORTED_VALUE <- round(plot.me$REPORTED_VALUE, 0)
  } 
  #----------------------------------------------------------------------------
  if (median.value < 5 & median.value >= 0.09) {
    plot.me$REPORTED_VALUE <- sprintf("%.3f", round(plot.me$REPORTED_VALUE, 1))
  }
  #----------------------------------------------------------------------------
  if (median.value < 0.09 & median.value >= 0.009) {
    plot.me$REPORTED_VALUE <- sprintf("%.3f", round(plot.me$REPORTED_VALUE, 2))
  } 
  #----------------------------------------------------------------------------
  if (median.value < 0.009 & median.value >= 0.0009) {
    plot.me$REPORTED_VALUE <- sprintf("%.3f", round(plot.me$REPORTED_VALUE, 3))
  } 
  #============================================================================
  # Extreme Values to colored red...
  if("DO" %in% plot.me$ICPRB_NAME){
    bad.plot <- plot.me[plot.me$REPORTED_VALUE < 2 | plot.me$REPORTED_VALUE > 25, ]
    bad.plot$OUTLIER <- factor("tomato3")
    param.range$MIN <- 2
    param.range$MAX <- 25
  } 
  if("PH" %in% plot.me$ICPRB_NAME){
    bad.plot <- plot.me[plot.me$REPORTED_VALUE < 5 | plot.me$REPORTED_VALUE > 9, ]
    param.range$MIN <- 5
    param.range$MAX <- 9
  } 
  if("TALK" %in% plot.me$ICPRB_NAME){
    bad.plot <- plot.me[plot.me$REPORTED_VALUE > 250, ]
    param.range$MAX <- 250
  } 
  if("HARDNESS" %in% plot.me$ICPRB_NAME){
    bad.plot <- plot.me[plot.me$REPORTED_VALUE > 250, ]
    param.range$MAX <- 250
  } 
  if("TEMP" %in% plot.me$ICPRB_NAME){
    bad.plot <- plot.me[plot.me$REPORTED_VALUE < 0 | plot.me$REPORTED_VALUE > 29, ]
    param.range$MIN <- 0
    param.range$MAX <- 29
  } 
  if("SPCOND" %in% plot.me$ICPRB_NAME){
    bad.plot <- plot.me[plot.me$REPORTED_VALUE >= 750, ]
    param.range$MAX <- 750
  } 
  if(!unique(plot.me$ICPRB_NAME) %in% c("DO", "PH", "TALK", "HARDNESS", "TEMP", "SPCOND")){
    bad.plot <- plot.me[plot.me$REPORTED_VALUE < -1000000000, ]
  } 
  if (exists("bad.plot")) {
    bad.plot$OUTLIER <- "tomato3"
  }
  #============================================================================
  final.plot <- ggplot2::ggplot(data = plot.me, aes(x = YEAR, y = MONTH, group = c(SITE))) +
    #labs(title = paste("Site:", long.df$SITE, sep = " ")) +
    labs(title = "Heatmap", #subtitle = "", #title = paste("Site:", plot.me$SITE, sep = " "),
         #     subtitle = paste("ICPRB_NAME:", unique(plot.me$ICPRB_NAME),
         #                      unique((plot.me$ICPRB_UNITS)), sep = " "),
         x = "Year",
         y = "Month") +
    geom_tile(aes(fill = as.numeric(REPORTED_VALUE),
                  color = as.numeric(REPORTED_VALUE)),
              colour = "white") +
    geom_text(aes(label = ifelse(is.na(as.character(REPORTED_VALUE)) | 
                                   REPORTED_VALUE %in% "NA", "", REPORTED_VALUE)),
              #sprintf("%s", as.character(REPORTED_VALUE)))),
              size = 3) +
    scale_x_date(date_breaks = "2 year",
                 date_labels = "%Y",
                 limits = c(as.Date("1972-01-01", format = "%Y-%m-%d"),
                            as.Date("2018-01-01", format = "%Y-%m-%d")),
                 expand = c(0, 0)) +

    theme(#plot.title = element_text(hjust = 0.5, vjust = 8, face = "bold"),
      plot.title = element_blank(),
      #text = element_text(size = 10),
      axis.text.x = element_text(size = 11, hjust = -0.3, vjust = 0),
      #axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_blank(),
      legend.position = c(0.5, 1),
      legend.direction = "horizontal",
      legend.justification = c(0.6, 0), 
      legend.key.width = unit(2, "lines"), 
      legend.key.height = unit(1.2, "lines"),
      #legend.key = element_blank(),
      legend.box = "horizontal",
      #legend.text = element_text(face = "bold"),
      #legend.background = element_rect(size = 0.5,
      #                                 linetype = "solid", 
      #                                 colour = "black"),
      axis.line = element_line(colour = "black"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      #panel.border = element_blank(),
      #panel.border = element_rect(colour = "black", fill = NA, size = 5),
      panel.background = element_blank(),
      plot.margin = unit(c(20, 0, 15, 10), "mm")
    ) 
  #----------------------------------------------------------------------------
  if ("DO" %in% plot.me$ICPRB_NAME) {
    final.plot <- final.plot +
      scale_fill_gradientn(colors = c("tomato3",  "#E69F00",
                                      "#56B4E9", "#56B4E9", "#E69F00",
                                      "tomato3"),
                           breaks = c(1.99, 2, 8,
                                      20, 25, 25.01),
                           limits = c(1, 26),
                           values = scales::rescale(c(1.99, 2, 8,
                                                      20, 25, 25.01)),
                           labels = c("",  2, 8,
                                      20, 25, ""),
                           guide = "colorbar",
                           na.value = "white", oob = scales::squish)  +
      guides(fill = guide_colorbar(ticks = FALSE))
  }
  #----------------------------------------------------------------------------
  if ("PH" %in% plot.me$ICPRB_NAME) {
    #    final.plot <- final.plot +
    #      scale_fill_gradient2(midpoint = 7, limits = c(5, 9),
    #                           low = "#E69F00", mid = "#56B4E9",
    #                           high = "#E69F00", na.value = "white")
    final.plot <- final.plot +
      scale_fill_gradientn(colors = c("tomato3",  "#E69F00",
                                      "#56B4E9", "#E69F00",
                                      "tomato3"),
                           breaks = c(4.99, 5, 7,
                                      9, 9.01),
                           limits = c(4, 10),
                           values = scales::rescale(c(4.99, 5, 7,
                                                      9, 9.01)),
                           labels = c("", 5, 7, 9, ""),
                           guide = "colorbar",
                           na.value = "white", 
                           oob = scales::squish) +
      guides(fill = guide_colorbar(ticks = FALSE))
  }
  #----------------------------------------------------------------------------
  if ("TALK" %in% plot.me$ICPRB_NAME) {
    #    final.plot <- final.plot +
    #      scale_fill_gradient2(midpoint = 100, limits = c(0, 250),
    #                           low = "#E69F00", mid = "#56B4E9",
    #                           high = "#E69F00", na.value = "white")
    final.plot <- final.plot +
      scale_fill_gradientn(colors = c("#E69F00", "#56B4E9", "#E69F00", "tomato3"),
                           breaks = c(0, 100, 250, 250.01),
                           limits = c(0, 260),
                           values = scales::rescale(c(0, 100, 250, 250.01)),
                           labels = c(0, 100, 250, ""),
                           guide = "colorbar",
                           na.value = "white",
                           oob = scales::squish) +
      guides(fill = guide_colorbar(ticks = FALSE))
  }
  #----------------------------------------------------------------------------
  if ("HARDNESS" %in% plot.me$ICPRB_NAME) {
    final.plot <- final.plot +
      scale_fill_gradientn(colors = c("#E69F00", "#56B4E9", "#E69F00", "tomato3"),
                           breaks = c(0, 100, 250, 250.01),
                           limits = c(0, 260),
                           values = scales::rescale(c(0, 100, 250, 250.01)),
                           labels = c(0, 100, 250, ""),
                           guide = "colorbar",
                           na.value = "white",
                           oob = scales::squish) +
      guides(fill = guide_colorbar(ticks = FALSE))
  }
  #----------------------------------------------------------------------------
  if ("TEMP" %in% plot.me$ICPRB_NAME) {
    temp.breaks <- c(-1, -0.01, 0, 29, 29.01, 30)
    final.plot <- final.plot +
      scale_fill_gradientn(colors = c("tomato3","tomato3",
                                      "#56B4E9", "#E69F00",
                                      "tomato3", "tomato3"),
                           breaks = temp.breaks,
                           limits = c(-2, 31),
                           values = scales::rescale(temp.breaks),
                           labels = c("", "", 0, 29, "", ""),
                           guide = "colorbar",
                           na.value = "white",
                           oob = scales::squish) +
      guides(fill = guide_colorbar(ticks = FALSE))
  }
  #----------------------------------------------------------------------------
  if ("SPCOND" %in% plot.me$ICPRB_NAME) {
    final.plot <- final.plot +
      scale_fill_gradientn(colors = c("#56B4E9", "#E69F00", "tomato3", "tomato3"),
                           breaks = c(0, 750, 750.01, 760),
                           limits = c(0, 750),
                           values = scales::rescale(c(0, 750, 750.01, 751)),
                           labels = c(0, 750, "", ""),
                           #guide = FALSE,
                           na.value = "white",
                           oob = scales::squish) +
      guides(fill = guide_colorbar(ticks = FALSE))
    
  }
  #----------------------------------------------------------------------------
  if (!unique(plot.me$ICPRB_NAME) %in% c("DO", "PH", "TALK", "HARDNESS",
                                         "TEMP", "SPCOND")) {
    final.plot <- final.plot +
      scale_fill_gradientn(#colours = cbPalette,
        colours = c("#56B4E9", "#E69F00"),
        breaks = c(param.range$MIN - 0.001,  param.range$MAX),
        #breaks = c(param.range$'05th',  param.range$'95th'),
        labels = c(round(param.range$MIN, 2),  round(param.range$MAX, 2)),
        limits = c(param.range$MIN - 0.001,  param.range$MAX),
        guide = "colorbar",
        na.value = "white"#,
        #label = ""
      ) +
      guides(fill = guide_colorbar(ticks = FALSE))
  }
  #----------------------------------------------------------------------------
  #  if (nrow(bad.plot) > 0){
  #    final.plot <- final.plot +
  #      geom_point(data = bad.plot, aes(x = YEAR, y = MONTH, 
  #                                      #width=0.9, height=0.9
  #                                      color = OUTLIER), 
  #                 size = 12,
  #                 pch = 18,
  #                 show.legend = TRUE) +
  #      guides(colour = guide_legend(label.position = "bottom", override.aes = list(size = 8.2),
  #                                   hjust = 0.5)) +
  #      #guides(color = guide_legend(label.position = "bottom")) +
  #      scale_color_manual(label = "Outlier", values = "tomato3") +
  #      geom_text(aes(label = ifelse(is.na(as.character(REPORTED_VALUE)) | 
  #                                     REPORTED_VALUE %in% "NA", "", REPORTED_VALUE)), size = 3)
  #  }
  #----------------------------------------------------------------------------
  final.plot <- final.plot +
    guides(fill = guide_colorbar(title = "Heatmap",
                                 title.position = "top",
                                 title.hjust = 0.5,
                                 title.theme = element_text(face = "bold",
                                                            size = 15,
                                                            angle = 0),
                                 ticks = FALSE))
  return(final.plot)
}