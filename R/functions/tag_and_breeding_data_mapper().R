tag_and_breeding_data_mapper <- 
  function(tag_data, nest_data, brood_data, resight_data, 
           # tag = FALSE, nest = FALSE, brood = FALSE, resight = FALSE,
           bird_ring, map_year = NULL, 
           breeding = TRUE){
    
    if(is.null(map_year)){
      map_year = tag_data %>% 
        filter(ring == bird_ring) %>% 
        pull(timestamp_local) %>% 
        min(.) %>% year()
    } else {map_year = map_year}
    
    
    if (nrow(tag_data) > nrow(brood_data)){
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = tag_data %>% 
            filter(ring == bird_ring & 
                     year(timestamp_local) == map_year) %>% pull(timestamp_local))
    } else if (nrow(brood_data) > nrow(resight_data)){
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = brood_data %>% 
            filter(ring == bird_ring & 
                     year(timestamp_brood) == map_year) %>% pull(timestamp_brood))
    } else if (nrow(resight_data) > nrow(nest_data)){
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = resight_data %>% 
            filter(ring == bird_ring & 
                     year(timestamp_resight) == map_year) %>% pull(timestamp_resight))
    } else {
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = nest_data %>% 
            filter(ring == bird_ring & 
                     year(nest_initiation_date) == map_year) %>% pull(nest_initiation_date) %>% 
            ymd_hms(paste(as.character(.), "00:00:00"), tz = "America/Mazatlan") %>% 
            subset(!is.na(.)))
    } 
    
    title = paste0("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">", 
                   tag_data %>% 
                     filter(ring == bird_ring & 
                              year(timestamp_local) == map_year) %>% pull(species) %>% unique(), ", ",
                   tag_data %>% 
                     filter(ring == bird_ring & 
                              year(timestamp_local) == map_year) %>% pull(sex) %>% unique(), ", ",
                   bird_ring, ", ", 
                   map_year, 
                   "</label>');
        }
    ")
    
    leaflet() %>% 
      addProviderTiles('Esri.WorldImagery') %>% 
      # tag data
      addCircleMarkers(data = tag_data %>% 
                         filter(ring == bird_ring & 
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
      addCircleMarkers(data = nest_data %>% 
                         filter(ring == bird_ring & 
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
      
      # brood data
      addCircleMarkers(data = brood_data %>% 
                         filter(ring == bird_ring & 
                                  year(timestamp_brood) == map_year), 
                       ~lon, ~lat, 
                       radius = ~10,
                       color = "black",
                       fillColor = ~pal(timestamp_brood),
                       stroke = TRUE, fillOpacity = 0.75,
                       popup = ~paste(sep = "<br/>", paste0("<br>", as.character(family_ID)),
                                      paste0("no. chicks = ", as.character(chicks))), 
                       group = "Brood observations") %>%
      
      # resight data
      addCircleMarkers(data = resight_data %>% 
                         filter(ring == bird_ring & 
                                  year(timestamp_resight) == map_year), 
                       ~lon, ~lat, 
                       radius = ~10,
                       color = "green",
                       fillColor = ~pal(timestamp_resight),
                       stroke = TRUE, fillOpacity = 0.75,
                       popup = ~paste0(as.character(timestamp_resight), 
                                       ", comments = ", as.character(comments)), 
                       group = "Resight observations") %>% 
      # Layers control
      addLayersControl(
        overlayGroups = c("Tag locations", "Nests", 
                          "Brood observations", "Resight observations"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      htmlwidgets::onRender(title) %>%
      addMeasure(primaryLengthUnit = "meters")
  }