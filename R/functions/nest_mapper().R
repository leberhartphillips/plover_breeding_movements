nest_mapper <- 
  function(nestR_out, time_zone_local = "America/Mazatlan",
           tag_and_breeding_data, bird_ring, map_year = NULL){
    
    nestR_out$visits$date <- 
      ymd_hms(nestR_out$visits$date, tz = time_zone_local)
    
    if(is.null(map_year)){
      map_year = tag_and_breeding_data$tagging %>% 
        dplyr::filter(ring == bird_ring) %>% 
        dplyr::pull(timestamp_local) %>% 
        min(.) %>% year()
    } else {map_year = map_year}
    
    pal <- 
      colorNumeric(
        palette = "Blues", 
        domain = nestR_out$visits %>% dplyr::pull(date))
    
    title = paste0("
          function() {
              $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">", 
                   tag_and_breeding_data$tagging %>% 
                     dplyr::filter(ring == bird_ring & 
                                     year(timestamp_local) == map_year) %>% dplyr::pull(species) %>% unique(), ", ",
                   tag_and_breeding_data$tagging %>% 
                     dplyr::filter(ring == bird_ring & 
                                     year(timestamp_local) == map_year) %>% dplyr::pull(sex) %>% unique(), ", ",
                   bird_ring, ", ",
                   tag_and_breeding_data$tagging %>% 
                     dplyr::filter(ring == bird_ring & 
                                     year(timestamp_local) == map_year) %>% dplyr::pull(code) %>% unique(), ", ",
                   map_year, 
                   "</label>');
          }
      ")
    
    leaflet() %>% 
      addProviderTiles('Esri.WorldImagery') %>% 
      # tag data
      addCircleMarkers(data = nestR_out$visits, 
                       ~long, ~lat,
                       radius = ~6,
                       color = "white",
                       fillColor = ~pal(date),
                       stroke = FALSE, fillOpacity = 0.75,
                       popup = ~as.character(date),
                       label = ~as.character(date), 
                       group = "Tag locations") %>% 
      
      # known nests
      addCircleMarkers(data = tag_and_breeding_data$nests %>% 
                         dplyr::filter(ring == bird_ring & 
                                         year(nest_initiation_date) == map_year), 
                       ~lon, ~lat, 
                       radius = ~10,
                       color = "black",
                       fillColor = ~pal(ymd(as.character(nest_initiation_date))),
                       stroke = TRUE, fillOpacity = 0.75,
                       popup = ~paste(sep = "<br/>", paste0("<br>", as.character(family_ID)),
                                      paste0("<br>lay date = ", as.character(nest_initiation_date)), 
                                      paste0("<br>end date = ", as.character(end_date)),
                                      paste0("<br>fate = ", as.character(fate))),
                       group = "Known nests (black)") %>%
    
      # nestR nests
      addCircleMarkers(data = nestR_out$nests, 
                       ~long, ~lat, 
                       radius = ~10,
                       color = "red",
                       fillColor = ~pal(ymd(as.character(attempt_start))),
                       stroke = TRUE, fillOpacity = 0.75,
                       popup = ~paste(sep = "<br/>", paste0("<br>", as.character(loc_id)),
                                      paste0("<br>start date = ", as.character(attempt_start)), 
                                      paste0("<br>end date = ", as.character(attempt_end)),
                                      paste0("<br>days visited = ", as.character(tot_vis))), 
                       group = "nestR (red)") %>% 
      
      # plover_nest_finder nests
      # addCircleMarkers(data = plover_nest_finder_out$likely_nests, 
      #                  ~lon, ~lat, 
      #                  radius = ~10,
      #                  color = "green",
      #                  # fillColor = ~pal(min(ymd(as.character(revisitStats$entranceTime)))),
      #                  stroke = TRUE, fillOpacity = 0.75,
      #                  # popup = ~paste0(as.character(timestamp_resight), 
      #                  #                 ", comments = ", as.character(comments)), 
      #                  group = "plover_nest_finder  (green)") %>% 
      # Layers control
      addLayersControl(
        overlayGroups = c("Known nests (black)", "nestR (red)", "Tag locations"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      htmlwidgets::onRender(title) %>%
      addMeasure(primaryLengthUnit = "meters")
  }
