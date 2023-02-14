tag_and_breeding_data_mapper <- 
  function(tag_and_breeding_data, 
           # tag = FALSE, nest = FALSE, brood = FALSE, resight = FALSE,
           bird_ring, map_year = NULL, 
           breeding = TRUE){
    
    if(is.null(map_year)){
      map_year = tag_and_breeding_data$tagging %>% 
        dplyr::filter(ring == bird_ring) %>% 
        dplyr::pull(timestamp_local) %>% 
        min(.) %>% year()
    } else {map_year = map_year}
    
    
    if (nrow(tag_and_breeding_data$tagging) > nrow(tag_and_breeding_data$broods)){
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = tag_and_breeding_data$tagging %>% 
            dplyr::filter(ring == bird_ring & 
                     year(timestamp_local) == map_year) %>% dplyr::pull(timestamp_local))
    } else if (nrow(tag_and_breeding_data$broods) > nrow(tag_and_breeding_data$resights)){
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = tag_and_breeding_data$broods %>% 
            dplyr::filter(ring == bird_ring & 
                     year(timestamp_brood) == map_year) %>% dplyr::pull(timestamp_brood))
    } else if (nrow(tag_and_breeding_data$resights) > nrow(tag_and_breeding_data$nests)){
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = tag_and_breeding_data$resights %>% 
            dplyr::filter(ring == bird_ring & 
                     year(timestamp_resight) == map_year) %>% dplyr::pull(timestamp_resight))
    } else {
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = tag_and_breeding_data$nests %>% 
            dplyr::filter(ring == bird_ring & 
                     year(nest_initiation_date) == map_year) %>% dplyr::pull(nest_initiation_date) %>% 
            ymd_hms(paste(as.character(.), "00:00:00"), tz = "America/Mazatlan") %>% 
            subset(!is.na(.)))
    } 
    
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
      addCircleMarkers(data = tag_and_breeding_data$tagging %>% 
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
      addCircleMarkers(data = tag_and_breeding_data$nests %>% 
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
      
      # brood data
      addCircleMarkers(data = tag_and_breeding_data$broods %>% 
                         dplyr::filter(ring == bird_ring & 
                                  year(timestamp_brood) == map_year), 
                       ~lon, ~lat, 
                       radius = ~10,
                       color = "black",
                       fillColor = ~pal(timestamp_brood),
                       stroke = TRUE, fillOpacity = 0.75,
                       popup = ~paste(sep = "<br/>", paste0("<br>", as.character(family_ID)),
                                      paste0("<br>", as.character(timestamp_brood)),
                                      paste0("no. chicks = ", as.character(chicks))), 
                       group = "Brood observations") %>%
      
      # resight data
      addCircleMarkers(data = tag_and_breeding_data$resights %>% 
                         dplyr::filter(ring == bird_ring & 
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
