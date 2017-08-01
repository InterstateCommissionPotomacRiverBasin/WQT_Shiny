blank_plot <- function(plot.me){
  #----------------------------------------------------------------------------
  final.plot <- ggplot2::ggplot(plot.me, aes(x = DATE,
                                             #ggplot2::ggplot(plot.me, aes(x = DATE,
                                             y = REPORTED_VALUE,
                                             fill = "white")) + 
    
    #geom_point(color = "white", size = 3, na.rm = TRUE, alpha = 0.6) +
    theme(plot.title = element_blank(),
          text = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          #legend.position = element_blank(),
          #legend.direction = element_blank(),
          #legend.justification = element_blank(), 
          #legend.key.width = unit(3, "lines"), 
          #legend.key.height = unit(1, "lines"),
          legend.key = element_blank(),
          #legend.box = "horizontal",
          legend.text = element_blank(),
          #panel.border = element_blank(),
          panel.background = element_blank(),
          #panel.border = element_rect(colour = "black", fill = NA, size = 1),
          plot.margin = unit(c(5, 0, 5, 0), "mm")
    )
  return(final.plot)
}