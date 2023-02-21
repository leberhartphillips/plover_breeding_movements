tag_and_nest_data_mapper <- 
  function(tag_and_nest_data, time_zone_local = "America/Mazatlan",
           bird_ring, map_year = NULL, 
           breeding = TRUE){
    
    tag_and_nest_data$tagging$timestamp_local <- 
      ymd_hms(tag_and_nest_data$tagging$timestamp_local, tz = time_zone_local)
    
    if(is.null(map_year)){
      map_year = tag_and_nest_data$tagging %>% 
        dplyr::filter(ring == bird_ring) %>% 
        dplyr::pull(timestamp_local) %>% 
        min(.) %>% year()
    } else {map_year = map_year}
    
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = tag_and_nest_data$tagging %>% 
            dplyr::filter(ring == bird_ring & 
                            year(timestamp_local) == map_year) %>% dplyr::pull(timestamp_local))
   
    title = paste0("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">", 
                   tag_and_nest_data$tagging %>% 
                     dplyr::filter(ring == bird_ring & 
                                     year(timestamp_local) == map_year) %>% dplyr::pull(species) %>% unique(), ", ",
                   tag_and_nest_data$tagging %>% 
                     dplyr::filter(ring == bird_ring & 
                                     year(timestamp_local) == map_year) %>% dplyr::pull(sex) %>% unique(), ", ",
                   bird_ring, ", ",
                   tag_and_nest_data$tagging %>% 
                     dplyr::filter(ring == bird_ring & 
                                     year(timestamp_local) == map_year) %>% dplyr::pull(code) %>% unique(), ", ",
                   map_year, 
                   "</label>');
        }
    ")
    
    leaflet() %>% 
      addProviderTiles('Esri.WorldImagery') %>% 
      # tag data
      addCircleMarkers(data = tag_and_nest_data$tagging %>% 
                         dplyr::filter(ring == bird_ring & 
                                         year(timestamp_local) == map_year), 
                       ~lon, ~lat,
                       radius = ~6,
                       color = "white",
                       fillColor = ~pal(timestamp_local),
                       stroke = FALSE, fillOpacity = 0.75,
                       popup = ~as.character(timestamp_local),
                       label = ~as.character(timestamp_local), 
                       group = "Tag locations") %>% 
      
      # nest data
      addCircleMarkers(data = tag_and_nest_data$nests %>% 
                         dplyr::filter(ring == bird_ring & 
                                         year(nest_initiation_date) == map_year), 
                       ~lon, ~lat, 
                       radius = ~10,
                       color = "red",
                       fillColor = ~pal(ymd_hms(paste(as.character(nest_initiation_date), 
                                                      "00:00:00"), 
                                                tz = "America/Mazatlan") %>% 
                                          subset(!is.na(.))),
                       stroke = TRUE, fillOpacity = 0.75,
                       popup = ~paste(sep = "<br/>", paste0("<br>", as.character(family_ID)),
                                      paste0("<br>lay date = ", as.character(nest_initiation_date)), 
                                      paste0("<br>end date = ", as.character(end_date)),
                                      paste0("<br>fate = ", as.character(fate))), 
                       group = "Nests") %>% 
      
      # Layers control
      addLayersControl(
        overlayGroups = c("Tag locations", "Nests"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      htmlwidgets::onRender(title) %>%
      addMeasure(primaryLengthUnit = "meters")
  }
