step_length_calculator <- 
  function(df, longitude_col, latitude_col, ind_col){
    if(length(unique(df[, ind_col])) == 1){
      
      dist_vec <- numeric(nrow(df)-1)
      
      df <- as.data.frame(df) %>% arrange(timestamp_local)
      
      for(i in 1:(nrow(df) - 1)){
        dist_vec[i] = 
          distGeo(p1 = matrix(c(df[i, longitude_col], 
                                df[i, latitude_col]), 
                              ncol = 2),
                  p2 = matrix(c(df[i+1, longitude_col], 
                                df[i+1, latitude_col]), 
                              ncol = 2))
      }
      
      dist_vec2 <- data.frame(distance = c(dist_vec, NA))
    
      return(bind_cols(df, dist_vec2))
    }
    else{
      print("more than one individual detected, please seperate dfs by individual")
    }
    }
