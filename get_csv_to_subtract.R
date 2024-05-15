get_csv_to_subtract <- function(profile_dir_true, profile_dir_fake,settings){
  
  number_of_directories_true <- length(profile_dir_true)
  number_of_directories_fake <- length(profile_dir_fake)
  
  true_profiles_paths <- NULL
  fake_profiles_paths <- NULL
  
  ### true
  
  for (iDir in 1:number_of_directories_true){
    trueNames <- list.files(path = profile_dir_true[iDir], pattern="\\.csv")
    
    #the samples with "smooth" in the title
    smoothed_sample_indices <- grep(pattern = "smooth", trueNames) # Fetch the smooth file in this directory
    #partition the samples
    non_smoothed_samples <- trueNames[-smoothed_sample_indices] # true profiles
    #for each sample make sure a smoothed sample exists and then build up arrays of paths
    for (iSample in 1:length(non_smoothed_samples)){
      trueName <- non_smoothed_samples[iSample]
      true_profiles_paths <- c(true_profiles_paths, paste(profile_dir_true[iDir], trueName, sep=""))

    }
  }
  
  number_sample_with_input_and_output <- length(true_profiles_paths) #83
  #the number of dyes 
  number_of_dyes <- settings$number_of_dyes
  #the starting scan point
  startScan <- settings$startScan
  #the number of scan points to be trained / generated
  number_of_scanpoints <- settings$number_of_scanpoints
  #the end scan point
  endScan <- startScan + number_of_scanpoints - 1 #need to minus 1 so that startScan:endScan gives number_of_scanpoints data points
  #now go through and read in files to create input and target arrays
  true_profiles <- array(0, dim = c(number_sample_with_input_and_output, number_of_dyes, number_of_scanpoints))
  for (iProfile in 1:number_sample_with_input_and_output){
    #loads the true profile
    true_profile <- read.csv(file = true_profiles_paths[iProfile], header = TRUE, stringsAsFactors = FALSE)
    
    
    #conducts simple profile baselining
    # baselined_true_profile <- baseline_true_profile_simple(true_profile)
    #stores the scans in the target_array
    true_profiles[iProfile,,] <- t(true_profile[startScan:endScan,2:(number_of_dyes + 1)])
  }
  # The above steps end up with input_profiles to store smoothed data and target_profiles for non-smoothed data
  
  
  ### fake
  
  for (iDir in 1:number_of_directories_fake){
    fakeNames <- list.files(path = profile_dir_fake[iDir], pattern="\\.csv") 
    
    non_smoothed_samples <- fakeNames
    #for each sample make sure a smoothed sample exists and then build up arrays of paths
    for (iSample in 1:length(non_smoothed_samples)){
      fakeName <- non_smoothed_samples[iSample]
      fake_profiles_paths <- c(fake_profiles_paths, paste(profile_dir_fake[iDir], fakeName, sep="")) 
      
    }
  }
  
  number_sample_with_input_and_output <- length(fake_profiles_paths) 
  #now go through and read in files to create input and target arrays
  fake_profiles <- array(0, dim = c(number_sample_with_input_and_output, number_of_dyes, number_of_scanpoints))
  for (iProfile in 1:number_sample_with_input_and_output){
    #loads the fake profile
    fake_profile <- read.csv(file = fake_profiles_paths[iProfile], header = TRUE, stringsAsFactors = FALSE)
    # conducts simple profile baselining
    # baselined_fake_profile <- baseline_true_profile_simple(fake_profile)
    #stores the scans in the target_array
    fake_profiles[iProfile,,] <- t(fake_profile[1:number_of_scanpoints,2:(number_of_dyes + 1)])
  }
  # The above steps end up with input_profiles to store smoothed data and target_profiles for non-smoothed data
  
  #the final step is to apply the saturation threshold 
  saturation <- settings$saturation 
  return(list(true_profiles = true_profiles/saturation,
              fake_profiles = fake_profiles/saturation,
              true_paths = true_profiles_paths,
              fake_paths = fake_profiles_paths))
  
}