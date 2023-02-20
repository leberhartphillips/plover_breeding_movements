library(nestR)

nestR_CN0423 <- 
  tag_breeding_data_ceuta$tagging %>% 
  filter(ring == "CN0423") %>% 
  mutate(burst = paste(ring, year(timestamp_local), sep = "-")) %>% 
  dplyr::select(burst, timestamp_local, lon, lat) %>% 
  rename(date = timestamp_local,
         long = lon,
         lat = lat)

nestR_CN0423_out <- 
  find_nests(nestR_CN0423, 
             buffer = 20, 
             sea_start = "04-01", 
             sea_end = "08-01", 
             min_pts = 5,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 10,
             min_days_att = 2)
             # 
nestR_CN0423_out
explore(nestR_CN0423_out)

nestR_CN0423_out$nests %>% 
  st_as_sf(x = .,                         
           coords = c("long", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  mapview()
  
