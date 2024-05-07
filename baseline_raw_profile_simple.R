baseline_raw_profile_simple <- function(raw_profile){
  source('getmode.R')
  number_dyes <- dim(raw_profile)[2] - 1
  
  return_profile <- raw_profile
  
  #very basic baselining function
  for (idye in 2:(number_dyes+1)){
    #gets the mode
    mode = getmode(raw_profile[[idye]])
    #minus the modes
    return_profile[[idye]] <- raw_profile[[idye]] - mode
  }
  
  return(return_profile)
  
}


