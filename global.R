#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Maintained: Zachary M. Smith
# Created: January 2017
# Updated: 2/28/17
#==============================================================================
#==============================================================================
library(RPostgreSQL)
library(pool)
#==============================================================================
pool <- dbPool("PostgreSQL",
               dbname = "WQT_May",
               #host = "192.168.1.214",
               host = "localhost",
               user = "guest",
               password = "guest",
               port = 5432
)

#------------------------------------------------------------------------------
# Import tables necessary for app.
param.range <- dbReadTable(pool, "param_range")
outliers <- dbReadTable(pool, "Outliers")
huc8 <- dbReadTable(pool, "huc_8")
list.huc <- c("All HUCs", sort(unique(huc8$HUC_8)))
sites <- huc8[order(huc8$SITE), ]$SITE
#------------------------------------------------------------------------------
# OLD??????????????
#sites <- dbGetQuery(conn, paste("SELECT table_name
#    FROM information_schema.tables
#    WHERE table_schema='public'
#    AND table_type='BASE TABLE';"))
#sites <- sites[order(sites$table_name),]
#==============================================================================

#==============================================================================
prep_plot <- function(long, site, param, outlier.table){
  #============================================================================
  # Prepare Date
  #============================================================================
  long <- long[long$SITE %in% site &
                 long$ICPRB_NAME %in% param, ]
  sub.outlier <- outlier.table[outlier.table$PARAMETER %in% param, ]
  #============================================================================
  long <- long[long$REPORTED_VALUE < sub.outlier$UP_FENCE_4.5 &
                 long$REPORTED_VALUE > sub.outlier$LOW_FENCE_4.5, ]
  #============================================================================
  # This is written for shiny. The app was working too quickly. If a new site
  # was selected and the currently selected parameter was not measured at the
  # new site, shiny first attempted to run a blank dataframe throught the 
  # functions.  The data frame was blank becuase the previously selected 
  # parameter had not been updated yet, and therefore, there were no rows to
  # keep during the subset process. The code below returns null during these
  # events but will quickly update after the applicable parameters have been
  # selected.
  if(nrow(long) < 1) return(NULL)
  #============================================================================
  # Identify column "DATE" as class date
  long$DATE <- as.Date(long$DATE, "%Y-%m-%d")
  # Create a new column by extracting the year from date
  long$YEAR <- format(long$DATE, "%Y")
  # Create a new column by extracting the month from date
  long$MONTH <- format(long$DATE, "%m")
  #============================================================================
  # Identify Censored Data
  #============================================================================
  #long$CENSORED <- ifelse(is.na(long$ResultDetectionConditionText) |
  #                             long$ResultDetectionConditionText %in% "",
  #                          "Uncensored", "Censored")
  #============================================================================
  # Sequence Dates
  #============================================================================
  long$REPORTED_VALUE <- as.numeric(as.character(long$REPORTED_VALUE))
  long <- long[, c("SITE", "MONTH", "YEAR", "ICPRB_NAME", #PARAMETER",
                   "REPORTED_VALUE", "ICPRB_UNITS", "CENSORED")]
  long <- long[long$YEAR >= 1972, ]
  # Use data.table functions to find the mean of reporting value. More than
  # one value per month/year/parameter would cause this function to fail
  # at a later stage.
  long.df <- data.table::data.table(long)
  plot.me <- long.df[, REPORTED_VALUE := mean(REPORTED_VALUE),
                     by = list(SITE, MONTH, YEAR, #PARAMETER,
                               ICPRB_NAME, ICPRB_UNITS, CENSORED)]
  
  
  plot.me$DATE <- as.Date(paste(plot.me$YEAR, plot.me$MONTH, 01, sep = "-"),
                          format = "%Y-%m-%d")
  #========================================================================
  new.df <- data.frame(YEAR = as.character(sort(rep(1972:2016, 12))))
  new.df$MONTH <- c("01", "02", "03", "04", "05", "06",
                    "07", "08", "09", "10", "11", "12")
  merged <- merge(plot.me, new.df, by = c("MONTH", "YEAR"), all = TRUE)
  merged$REPORTED_VALUE <- ifelse(is.na(merged$SITE), "", merged$REPORTED_VALUE)
  merged$REPORTED_VALUE <- as.numeric(merged$REPORTED_VALUE)
  merged$SITE <- unique(plot.me$SITE)
  #merged$PARAMETER <- unique(plot.me$PARAMETER)
  merged$ICPRB_NAME <- unique(plot.me$ICPRB_NAME)
  merged$ICPRB_UNITS <- unique(plot.me$ICPRB_UNITS)[1]
  merged$YEAR <- as.numeric(as.character(merged$YEAR))
  final.df <- unique(merged[order(merged$YEAR), ])
  final.df$YEAR <- as.character(final.df$YEAR)
  final.df$CENSORED <- ifelse(is.na(final.df$CENSORED), "Uncensored",
                              as.character(final.df$CENSORED))
  final.df$CENSORED <- factor(final.df$CENSORED, levels = c("Uncensored", "Censored"))
  return(final.df)
}
#==============================================================================
param_range <- function(site){
  #============================================================================
  # Prepare Date
  #============================================================================
  # Identify column "DATE" as class date
  site$DATE <- as.Date(site$DATE, "%Y-%m-%d")
  # Create a new column by extracting the year from date
  site$YEAR <- format(site$DATE, "%Y")
  # Create a new column by extracting the month from date
  site$MONTH <- format(site$DATE, "%m")
  #============================================================================
  # Sequence Dates
  #============================================================================
  site$REPORTED_VALUE <- as.numeric(as.character(site$REPORTED_VALUE))
  site <- site[,c("SITE", "MONTH", "YEAR",# "PARAMETER",
                  "ICPRB_NAME",
                  "REPORTED_VALUE", "ICPRB_UNITS")]
  site <- site[site$YEAR >= start.year, ]
  # Use data.table functions to find the mean of reporting value. More than
  # one value per month/year/parameter would cause this function to fail
  # at a later stage.
  long.df <- data.table::data.table(site)
  long.df <- long.df[, REPORTED_VALUE := mean(REPORTED_VALUE),
                     by = list(SITE, MONTH, YEAR, #PARAMETER,
                               ICPRB_NAME, ICPRB_UNITS)]
  
  
  #============================================================================
  print("Calculating Paramter Min/Max...")
  max.df <- aggregate(REPORTED_VALUE ~ ICPRB_NAME, data = long.df, max)
  names(max.df) <- c("ICPRB_NAME", "MAX")
  ninteyfive.df <- aggregate(REPORTED_VALUE ~ ICPRB_NAME, data = long.df, function(x) quantile(x, 0.95))
  names(ninteyfive.df) <- c("ICPRB_NAME", "95th")
  min.df <- aggregate(REPORTED_VALUE ~ ICPRB_NAME, data = long.df, min)
  names(min.df) <- c("ICPRB_NAME", "MIN")
  five.df <- aggregate(REPORTED_VALUE ~ ICPRB_NAME, data = long.df, function(x) quantile(x, 0.05))
  names(five.df) <- c("ICPRB_NAME", "05th")
  final.df <- plyr::join_all(c(min.df, max.df, ninteyfive.df, five.df), by = "ICPRB_NAME")
  #============================================================================
  
  return(final.df)
}
#==============================================================================
raw_loess_plot <- function(plot.me){
  
  final.plot <- ggplot2::ggplot(plot.me, aes(x = DATE,
                                             #ggplot2::ggplot(plot.me, aes(x = DATE,
                                             y = REPORTED_VALUE,
                                             fill = CENSORED)) + 
    
    #geom_line(color = "#56B4E9", size = 1)  + # "royalblue3"
    #geom_point(color = "#56B4E9", size = 2, na.rm = TRUE) +
    geom_point(color = "black", pch = 21, size = 3, na.rm = TRUE, alpha = 0.6) +
    scale_fill_manual(values = c("#56B4E9", "#009E73"), drop = FALSE) +
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
#==============================================================================
flow_correct_loess_plot <- function(plot.me){
  final.plot <- ggplot2::ggplot(plot.me, aes(x = DATE, y = REPORTED_VALUE)) + 
    labs(x = "Date",
         y = paste(unique(plot.me$ICPRB_NAME),
                   " (", unique(plot.me$ICPRB_UNITS), ")", sep = ""),
         size = 12) +
    #geom_line(color = "#56B4E9", size = 1)  + # "royalblue3"
    geom_point(color = "#56B4E9", size = 2, na.rm = TRUE) +
    stat_smooth(method = 'loess', color = "#E69F00", size = 1.2, na.rm = TRUE) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y", expand = c(0.012, 0), 
                 limits = c(as.Date("1972-01-01", format = "%Y-%m-%d"),
                            as.Date("2016-01-01", format = "%Y-%m-%d"))) +
    ggtitle("Flow Corrected Data") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.border = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          plot.margin = unit(c(5, 0, 5, 0), "mm")
    ) 
  return(final.plot)
}
#==============================================================================
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
    labs(title = "", subtitle = "", #title = paste("Site:", plot.me$SITE, sep = " "),
         #     subtitle = paste("ICPRB_NAME:", unique(plot.me$ICPRB_NAME),
         #                      unique((plot.me$ICPRB_UNITS)), sep = " "),
         x = "Year",
         y = "Month") +
    geom_tile(aes(fill = as.numeric(REPORTED_VALUE)), colour = "white") +
    geom_text(aes(label = ifelse(is.na(as.character(REPORTED_VALUE)) | 
                                   REPORTED_VALUE %in% "NA", "", REPORTED_VALUE)),
              #sprintf("%s", as.character(REPORTED_VALUE)))),
              size = 3) +
    
    scale_fill_gradientn(#colours = cbPalette,
      colours = c("#56B4E9", "#E69F00"),
      breaks = c(param.range$MIN - 0.001,  param.range$MAX),
      #breaks = c(param.range$'05th',  param.range$'95th'),
      labels = c(param.range$MIN,  param.range$MAX),
      limits = c(param.range$MIN - 0.001,  param.range$MAX),
      guide = "colorbar",
      na.value = "white"#,
      #label = ""
    ) +
    guides(color = guide_legend(label.position = "bottom")) +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          plot.subtitle = element_text(hjust = 0.5, size = 11),
          text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          #legend.position = "top",
          legend.title = element_blank(),
          legend.position = c(0.5, 1),
          legend.direction = "horizontal",
          legend.justification = c(0.6, 0), 
          legend.key.width = unit(3, "lines"), 
          legend.key.height = unit(1.5, "lines"),
          legend.key = element_blank(),
          legend.box = "horizontal",
          legend.text = element_text(face = "bold"),
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
          plot.margin = unit(c(10, 0, 5, 0), "mm")
    ) 
  
  if ("PH" %in% plot.me$ICPRB_NAME) {
    final.plot <- final.plot +
      scale_fill_gradient2(midpoint = 7, limits = c(5, 9),
                           low = "#E69F00", mid = "#56B4E9", high = "#E69F00", na.value = "white")
  }
  
  if ("TALK" %in% plot.me$ICPRB_NAME) {
    final.plot <- final.plot +
      scale_fill_gradient2(midpoint = 100, limits = c(0, 250),
                           low = "#E69F00", mid = "#56B4E9", high = "#E69F00", na.value = "white")
  }
  
  if (nrow(bad.plot) > 0){
    final.plot <- final.plot +
      geom_point(data = bad.plot, aes(x = YEAR, y = MONTH, 
                                      #width=0.9, height=0.9
                                      color = OUTLIER), 
                 
                 #fill = "tomato3",
                 size = 12,
                 pch = 18,
                 show.legend = TRUE) +
      guides(colour = guide_legend(label.position = "bottom", override.aes = list(size = 8.2),
                                   hjust = 0.5)) +
      #guides(color = guide_legend(label.position = "bottom")) +
      scale_color_manual(label = "Outlier", values = "tomato3") +
      geom_text(aes(label = ifelse(is.na(as.character(REPORTED_VALUE)) | 
                                     REPORTED_VALUE %in% "NA", "", REPORTED_VALUE)), size = 3)
  }
  
  return(final.plot)
}
#==============================================================================

uni.func <- function(my.col){
  uni.vec <- trimws(unique(my.col))
  uni.vec[uni.vec %in% c("NA", "", "NA NA") | is.na(uni.vec)] <- "<em>Blank</em>"
  final.vec <- paste(uni.vec, collapse = ",  ")
  return(final.vec)
}


