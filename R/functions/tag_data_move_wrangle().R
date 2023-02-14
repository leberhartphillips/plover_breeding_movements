tag_data_move_wrangle <- 
  function(formatted_tag_data, 
           temporal_res, 
           temporal_unit,
           projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
           longitude_name, latitude_name,
           timestamp_name, ind_name){
    
    # make move object for animation
    move_object <- 
      df2move(df = arrange(as.data.frame(formatted_tag_data), timestamp_name), 
              track_id = ind_name, x = longitude_name, y = latitude_name, 
              time = timestamp_name,
              proj = projection)
    
    # check the amount of time between fixes. The tracks in move_data have 
    # irregular timestamps and sampling rates. print unique timestamps and 
    # timeLag (might vary due to different tagging schedules and models)
    sampling_times <- 
      unique(timestamps(move_object))
    
    sampling_time_lags <- 
      timeLag(move_object, unit = temporal_unit)
    
    # use align_move to correct move_data to a uniform time scale and lag 
    # using interpolation
    aligned_move_object <- 
      suppressWarnings(align_move(move_object, 
                                  res = temporal_res, 
                                  unit = temporal_unit))
    
    # double check the lag of the aligned data (should be one value)
    aligned_sampling_time_lag <- 
      unique(unlist(timeLag(aligned_move_object, units = temporal_unit)))
    
    # check number of fixes collected
    number_of_fixes <- 
      nrow(as.data.frame(formatted_tag_data))
    
    sampling_start <- 
      formatted_tag_data %>% 
      as.data.frame() %>% 
      pull(timestamp_name) %>% 
      min()

    sampling_end <- 
      formatted_tag_data %>% 
      as.data.frame() %>% 
      pull(timestamp_name) %>% 
      max()    
    
    timespan_of_sampling <- 
      sampling_end - sampling_start
    
    aligned_move_object_list <- 
      list(raw_move_object = move_object,
           aligned_move_object = aligned_move_object,
           sampling_times = sampling_times,
           sampling_time_lags = sampling_time_lags,
           aligned_sampling_time_lag = aligned_sampling_time_lag,
           number_of_fixes = number_of_fixes,
           sampling_start = sampling_start,
           sampling_end = sampling_end,
           timespan_of_sampling = timespan_of_sampling)
    
    aligned_move_object_list
  }