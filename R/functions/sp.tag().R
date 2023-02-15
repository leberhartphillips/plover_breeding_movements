sp.tag <- function(tag_df, projection, UTM = FALSE, lat, long){
  tag_df <- 
    as.data.frame(tag_df) %>%
    `coordinates<-` (c(long, lat)) %>% 
    `proj4string<-` (projection)
  if(UTM == TRUE){
    tag_df <- spTransform(tag_df, CRS("+proj=utm +zone=13 ellps=WGS84"))
  }
  else{
    tag_df
  }
  tag_df
}
