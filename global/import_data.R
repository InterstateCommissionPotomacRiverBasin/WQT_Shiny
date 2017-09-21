# Import tables necessary for app.
param.range <- dbReadTable(pool, "param_range")
outliers <- dbReadTable(pool, "Outliers")
huc8 <- dbReadTable(pool, "huc_8")
list.huc <- c("All HUCs", sort(unique(huc8$HUC_8)))
sites <-  dbGetQuery(pool, paste('SELECT DISTINCT "SITE"',
                                 'FROM "wq_data"',
                                 'WHERE "ICPRB_NAME" =', "'TEMP'",
                                 'ORDER BY "SITE"')) %>% 
  pull(SITE)
RPostgreSQL::dbReadTable(pool, "param_range")