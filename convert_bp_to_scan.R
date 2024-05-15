convert_bp_to_scan <- function(bp_array){
  
  # the size is generated in base pairs we we want it to be in scan points so that it matches the data in the EPG format
  # without any justification I provide a distribution for the number of bp to scan conversion
  bp_to_scan_mean <- 11.20068182
  bp_to_scan_sd <- 0.079615827
  
  # I was originally letting this wiggle a little bit but the results that came out were wacky so now
  # I am just using a fixed linear conversion
  
  #random_conversions <- rnorm(n = length(bp_array), mean = bp_to_scan_mean, sd = bp_to_scan_sd)
  random_conversions <- bp_to_scan_mean
  
  Scans <- round(as.numeric(bp_array) * random_conversions, 0) + 3500
  
  return(Scans)
}
