# Import tables necessary for app.
param.range <- dbReadTable(pool, "param_range")
outliers <- dbReadTable(pool, "Outliers")
huc8 <- dbReadTable(pool, "huc_8")
list.huc <- c("All HUCs", sort(unique(huc8$HUC_8)))
sites <- unique(huc8[order(huc8$SITE), ]$SITE)
