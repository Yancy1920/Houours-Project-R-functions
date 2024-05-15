baseline_raw_profile <- function(raw_profile){

  number_dyes <- dim(raw_profile)[2] - 1
  return_profile <- raw_profile

  # a bit of a more sophisticated function that uses lowess
  for (idye in 2:(number_dyes+1)){
    #gets the baseline
    lowess_smooth <- lowess(y = raw_profile[,idye], x = raw_profile[,1], f = 0.05)
    # removes it from the raw profile
    return_profile[[idye]] <- raw_profile[[idye]] - lowess_smooth$y
  }
  
  
  return(return_profile)
  
}


