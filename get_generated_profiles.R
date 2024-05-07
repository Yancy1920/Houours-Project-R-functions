get_generated_profiles <- function(profile_dir_gen, settings){
  
  number_of_directories_gen <- length(profile_dir_gen)
  
  gen_profiles_paths <- NULL
  
  ### smooth
  
  for (iDir in 1:number_of_directories_gen){
    sampleNames <- list.files(path = profile_dir_gen[iDir], pattern="\\.csv") # pick all csv files
    
    for (iSample in 1:length(sampleNames)){
      sampleName <- sampleNames[iSample]
      sample_name_without_suffix <- substr(x = sampleName, start = 1, stop = nchar(sampleName) - 4) # remove ".csv"
      sample_index <- grep(pattern = sample_name_without_suffix, x = sampleNames) 
      gen_profiles_paths <- c(gen_profiles_paths, paste(profile_dir_gen[iDir], sampleNames[sample_index], sep=""))# input is smooth

    }
  }
  
  number_sample_with_input_and_output <- length(gen_profiles_paths) #83
  #the number of dyes 
  number_of_dyes <- settings$number_of_dyes
  #the starting scan point
  startScan <- settings$startScan # 4000
  #the number of scan points to be trained / generated
  number_of_scanpoints <- settings$number_of_scanpoints # 5000
  #the end scan point
  endScan <- startScan + number_of_scanpoints - 1 #need to minus 1 so that startScan:endScan gives number_of_scanpoints data points
  #now go through and read in files to create input and target arrays
  gen_profiles <- array(0, dim = c(number_sample_with_input_and_output, number_of_dyes, number_of_scanpoints)) # 83*6*5000
  for (iProfile in 1:number_sample_with_input_and_output){
    #loads the smooth profile
    gen_profile <- read.csv(file = gen_profiles_paths[iProfile], header = TRUE, stringsAsFactors = FALSE)
    gen_profiles[iProfile,,] <- t(gen_profile[,2:(number_of_dyes + 1)])
  }
  
  #the final step is to apply the saturation threshold （饱和度阈值）
  saturation <- settings$saturation 
  return(list(gen_profiles = gen_profiles/saturation,
              gen_paths = gen_profiles_paths))
  
}