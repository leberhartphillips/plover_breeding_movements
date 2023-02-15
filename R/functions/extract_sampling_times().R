extract_sampling_times <- 
  function(df){
    
    times <- 
      round(df$timestamp_local, units = "hours") %>% 
      format(format="%H:%M") %>% 
      unique()
    
    for(i in 1:length(times)){
      if(i == 1){
        first_element <- 
          list(#time_1 = 
            df %>% 
              mutate(hour = format(round(timestamp_local, units = "hours"), format = "%H:%M")) %>% 
              filter(hour == times[1]))
      }
      else{
        subsequent_element <- 
          list(#time_x <-  
            df %>% 
              mutate(hour = format(round(timestamp_local, units = "hours"), format = "%H:%M")) %>% 
              filter(hour == times[i]))
        if(i == 2){
          times_list_sub <- 
            append(first_element, subsequent_element, after = (i-1))
        }
        else{
          times_list_sub <- 
            append(times_list_sub, subsequent_element, after = (i-1))
        }
      }
    }
    times_list_sub
  }

df = CN0161$tagging
