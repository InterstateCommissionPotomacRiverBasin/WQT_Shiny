prep_plot <- function(long, site, param, outlier.table, monthly.mean){
  if (any(is.null(long), is.null(site), is.null(param),
          is.null(outlier.table), is.null(monthly.mean))) return(null)
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
  long <- long[, c("SITE", "MONTH", "YEAR", "ICPRB_NAME", "DEPTH",
                   "REPORTED_VALUE", "ICPRB_UNITS", "CENSORED")]
  long <- long[long$YEAR >= 1972, ]
  long <- long %>% 
    filter(YEAR >= 1972,
           DEPTH <= 1 | is.na(DEPTH))
  if (monthly.mean == TRUE){
    # Use data.table functions to find the mean of reporting value. More than
    # one value per month/year/parameter would cause this function to fail
    # at a later stage.
    long.df <- data.table::data.table(long)
    plot.me <- long.df[, REPORTED_VALUE := mean(REPORTED_VALUE),
                       by = list(SITE, MONTH, YEAR, #PARAMETER,
                                 ICPRB_NAME, ICPRB_UNITS, CENSORED)]
  } else {
    plot.me <- long
  }
  
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