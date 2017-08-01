raw_loess_plot <- function(plot.me){
  plot.me$CENSORED <- factor(plot.me$CENSORED, levels = c("Uncensored", "Censored"))
  final.plot <- ggplot2::ggplot(plot.me, aes(x = DATE,
                                             #ggplot2::ggplot(plot.me, aes(x = DATE,
                                             y = REPORTED_VALUE,
                                             fill = CENSORED)) + 
    
    #geom_line(color = "#56B4E9", size = 1)  + # "royalblue3"
    #geom_point(color = "#56B4E9", size = 2, na.rm = TRUE) +
    geom_point(color = "black", aes(shape = CENSORED), size = 3, na.rm = TRUE, alpha = 0.6) +
    scale_fill_manual(values = c("#56B4E9", "#580058"), drop = FALSE) + # "#009E73"
    scale_shape_manual(values = c(21, 22), drop = FALSE) +
    #geom_point(size = 2, na.rm = TRUE) +
    stat_smooth(method = 'loess', fill = "#999999", color = "#E69F00",
                size = 1.2, na.rm = TRUE, show.legend = FALSE) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y", expand = c(0.012, 0), 
                 limits = c(as.Date("1972-01-01", format = "%Y-%m-%d"),
                            as.Date("2016-01-01", format = "%Y-%m-%d"))) +
    ggtitle("Primary Data") +
    labs(x = "Date",
         y = paste(unique(plot.me$ICPRB_NAME),
                   " (", unique(plot.me$ICPRB_UNITS), ")", sep = ""),
         size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0.93, 1),
          legend.direction = "horizontal",
          legend.justification = c(0.6, 0), 
          legend.key.width = unit(3, "lines"), 
          legend.key.height = unit(1, "lines"),
          legend.key = element_blank(),
          legend.box = "horizontal",
          legend.text = element_text(face = "bold"),
          #panel.border = element_blank(),
          panel.background = element_blank(),
          #panel.border = element_rect(colour = "black", fill = NA, size = 1),
          plot.margin = unit(c(5, 0, 5, 0), "mm")
    )
  return(final.plot)
}