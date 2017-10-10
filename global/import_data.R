#==============================================================================
conn <- pool::poolCheckout(pool)
#==============================================================================
# Import tables necessary for app.
param.names <- dbReadTable(conn, "param_full_names")
param.range <- dbReadTable(conn, "param_range")
outliers <- dbReadTable(conn, "Outliers")
huc8 <- dbReadTable(conn, "huc_8")
list.huc <- c("All HUCs", sort(unique(huc8$HUC_8)))
sites <- huc8 %>% 
  filter(ICPRB_NAME == "TEMP") %>% 
  select(SITE) %>% 
  arrange(SITE) %>% 
  pull(SITE)
#==============================================================================
pool::poolReturn(conn)
#==============================================================================


