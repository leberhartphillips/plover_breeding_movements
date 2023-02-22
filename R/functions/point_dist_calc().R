point_dist_calc <- function(df, fix_sched = 24){
  # res <- spTransform(xy, CRS("+proj=utm +zone=51 ellps=WGS84"))
  tr <- as.ltraj(xy = coordinates(df),
                 date = df@data$timestamp_local,
                 id = df@data$ring)
  refda <- min(df@data$timestamp_local)
  tr <- sett0(tr, refda, dt = fix_sched, units = "hour")
  tr <- setNA(tr, refda, dt = fix_sched, units = "hour")
  path <- ld(tr)
  path$date_simp <- 
    as.Date(str_sub(as.character(path$date), start = 1, end = 10), 
            format = "%Y-%m-%d")
  df@data <- mutate(df@data, pkey = paste0(tag_ID, ".", as.character(timestamp_local)))
  df@data <- suppressWarnings(left_join(df@data, 
                                        path[which(grepl(paste(c("pkey", "dist"), 
                                                               collapse = "|"), names(path)))], 
                                        by = c("pkey")))
  return(df)
}