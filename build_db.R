con = dbConnect("PostgreSQL", user = "Z_Smith", password = "Hexapoda27",
                dbname = "WQT_May", host = "localhost", port = 5432)
on.exit(dbDisconnect(con), add = TRUE)
#==============================================================================
# Aggregate data by Site. 
lapply (seq_along(unique(final.site$SITE)), function(i) {
  print(paste(i, "/", length(unique(final.site$SITE)), sep = ""))
  sub.final <- final.site[final.site$SITE %in% unique(final.site$SITE)[i], ]
  sub.final <- sub.final[, !names(sub.final) %in% "EVENT_ID"]
  dbWriteTable(conn = con,
               name = paste0("SITE_", unique(final.site$SITE)[i]),
               value = sub.final,
               overwrite = TRUE,
               row.names = FALSE)
  #for (j in 1:10) print(j)
})

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
  site <- site[site$YEAR >= 1972, ]
  # Use data.table functions to find the mean of reporting value. More than
  # one value per month/year/parameter would cause this function to fail
  # at a later stage.
  long.df <- data.table::data.table(site)
  long.df <- long.df[, REPORTED_VALUE := mean(REPORTED_VALUE),
                     by = list(SITE, MONTH, YEAR,# PARAMETER,
                               ICPRB_NAME, ICPRB_UNITS)]
  
  
  #============================================================================
  print("Calculating Paramter Min/Max...")
  #max.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, max)
  max.df <- aggregate(REPORTED_VALUE ~ ICPRB_NAME, data = long.df, max)
  #function(x) quantile(x, 0.95))
  #names(max.df) <- c("PARAMETER", "MAX")
  names(max.df) <- c("ICPRB_NAME", "MAX")
  #min.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, min)
  min.df <- aggregate(REPORTED_VALUE ~ ICPRB_NAME, data = long.df, min)
  #function(x) quantile(x, 0.05))
  #names(min.df) <- c("PARAMETER", "MIN")
  names(min.df) <- c("ICPRB_NAME", "MIN")
  #final.df <- merge(min.df, max.df, by = "PARAMETER")
  final.df <- merge(min.df, max.df, by = "ICPRB_NAME")
  #============================================================================
  
  return(final.df)
}
#==============================================================================
param.range <- param_range(final.site)
#Updated on 5/19/17
dbWriteTable(con, "Outliers", outliers, overwrite = TRUE , row.names = FALSE)
# Using the outlier range to create the parameter gradient.
param.range <- outliers[, c("PARAMETER", "LOW_FENCE_4.5", "UP_FENCE_4.5")]
names(param.range) <- c("ICPRB_NAME", "MIN", "MAX")
param.range$MIN <- ifelse(param.range$ICPRB_NAME %in% "TEMP", 0, param.range$MIN)
param.range$MIN <- ifelse(param.range$MIN <= 0, 0, param.range$MIN)
# Export the merged file to the WQT database.
dbWriteTable(con, "param_range", param.range, overwrite = TRUE , row.names = FALSE)
#==============================================================================
huc <- unique(final.site[, c("SITE", "ICP_ID", "HUC_8")])
# Export the merged file to the WQT database.
dbWriteTable(con, "huc_8", huc, overwrite = TRUE , row.names = FALSE)
#==============================================================================
write.csv(head(final.site, 10000), "param_data_5_22_17.csv", row.names = FALSE)
write.csv(outliers, "outliers_5_22_17.csv", row.names = FALSE)
write.csv(param.range, "param_range_5_22_17.csv", row.names = FALSE)
write.csv(huc, "huc_5_22_17.csv", row.names = FALSE)
