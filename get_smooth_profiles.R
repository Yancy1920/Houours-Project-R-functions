get_smooth_profiles <- function(profile_dir_smooth, settings){
  
  number_of_directories_smooth <- length(profile_dir_smooth)
  
  smooth_profiles_paths <- NULL
  
  ### smooth
  
  for (iDir in 1:number_of_directories_smooth){
    sampleNames <- list.files(path = profile_dir_smooth[iDir], pattern="\\.csv") 
    
    #the samples with "smooth" in the title
    smoothed_sample_indices <- grep(pattern = "smooth", sampleNames) # Fetch the smooth file in this directory
    #partition the samples
    non_smoothed_samples <- sampleNames[-smoothed_sample_indices] # nonsmooth profiles
    smoothed_samples <- sampleNames[smoothed_sample_indices] # smooth profile
    #for each sample make sure a smoothed sample exists and then build up arrays of paths
    for (iSample in 1:length(non_smoothed_samples)){
      sampleName <- non_smoothed_samples[iSample]
      sample_name_without_suffix <- substr(x = sampleName, start = 1, stop = nchar(sampleName) - 4) # remove ".csv"
      corresponding_smoothed_sample_index <- grep(pattern = sample_name_without_suffix, x = smoothed_samples) 
      if (length(corresponding_smoothed_sample_index) == 1){ # if smooth profiles exist
        smooth_profiles_paths <- c(smooth_profiles_paths, paste(profile_dir_smooth[iDir], smoothed_samples[corresponding_smoothed_sample_index], sep=""))# input is smooth
      }
    }
  }
  
  number_sample_with_input_and_output <- length(smooth_profiles_paths) 
  #the number of dyes 
  number_of_dyes <- settings$number_of_dyes
  #the starting scan point
  startScan <- settings$startScan 
  #the number of scan points to be trained / generated
  number_of_scanpoints <- settings$number_of_scanpoints 
  #the end scan point
  endScan <- startScan + number_of_scanpoints - 1 #need to minus 1 so that startScan:endScan gives number_of_scanpoints data points
  #now go through and read in files to create input and target arrays
  smooth_profiles <- array(0, dim = c(number_sample_with_input_and_output, number_of_dyes, number_of_scanpoints))
  for (iProfile in 1:number_sample_with_input_and_output){
    #loads the smooth profile
    smooth_profile <- read.csv(file = smooth_profiles_paths[iProfile], header = TRUE, stringsAsFactors = FALSE)
    
    
    #conducts simple profile baselining
    # baselined_smooth_profile <- baseline_smooth_profile_simple(smooth_profile)
    #stores the scans in the target_array
    smooth_profiles[iProfile,,] <- t(smooth_profile[startScan:endScan,2:(number_of_dyes + 1)])
  }
  
  #the final step is to apply the saturation threshold 
  saturation <- settings$saturation 
  return(list(smooth_profiles = smooth_profiles/saturation,
              smooth_paths = smooth_profiles_paths))
  
}