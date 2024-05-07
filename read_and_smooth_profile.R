#clears work environment
rm(list=ls())
#libraries
library('here')
#functions
source('convert_xml_to_csv.R')
source('convert_bp_to_scan.R')
source('baseline_raw_profile.R')
source('get_mock_EPG_from_scans.R')
source('smooth_profile.R')
source('compare_raw_v_smoothed_from_scans.R')
source('baseline_raw_profile_simple.R')

# profiles_to_convert_dir <- c(paste(here(), "\\ProvedIT_profiles\\PROVEDIt_2-5-Person Profiles_3500 5sec_GF29cycles\\5 sec\\RD14-0003(020316ADG_5sec)\\", sep=""),
#                              paste(here(), "\\ProvedIT_profiles\\PROVEDIt_2-5-Person Profiles_3500 5sec_GF29cycles\\5 sec\\RD14-0003(020516ADG_5sec)\\", sep=""),
#                              paste(here(), "\\ProvedIT_profiles\\PROVEDIt_2-5-Person Profiles_3500 5sec_GF29cycles\\5 sec\\RD14-0003(021016ADG_5sec)\\", sep=""),
#                              paste(here(), "\\ProvedIT_profiles\\PROVEDIt_2-5-Person Profiles_3500 5sec_GF29cycles\\5 sec\\RD14-0003(021516ADG_5sec)\\", sep=""),
#                              paste(here(), "\\ProvedIT_profiles\\PROVEDIt_2-5-Person Profiles_3500 5sec_GF29cycles\\5 sec\\RD14-0003(021716ADG_5sec)\\", sep=""),
#                              paste(here(), "\\ProvedIT_profiles\\PROVEDIt_2-5-Person Profiles_3500 5sec_GF29cycles\\5 sec\\RD14-0003(022316ADG_5sec)\\", sep=""),
#                              paste(here(), "\\ProvedIT_profiles\\PROVEDIt_2-5-Person Profiles_3500 5sec_GF29cycles\\5 sec\\RD14-0003(022516ADG_5sec)\\", sep=""),
#                              paste(here(), "\\ProvedIT_profiles\\PROVEDIt_2-5-Person Profiles_3500 5sec_GF29cycles\\5 sec\\RD14-0003(022616ADG_5sec)\\", sep=""),
#                              paste(here(), "\\ProvedIT_profiles\\PROVEDIt_2-5-Person Profiles_3500 5sec_GF29cycles\\5 sec\\RD14-0003(022916ADG_5sec)\\", sep=""),
#                              paste(here(), "\\ProvedIT_profiles\\PROVEDIt_2-5-Person Profiles_3500 5sec_GF29cycles\\5 sec\\RD14-0003(030216ADG_5sec)\\", sep=""),
#                              paste(here(), "\\ProvedIT_profiles\\PROVEDIt_2-5-Person Profiles_3500 5sec_GF29cycles\\5 sec\\RD14-0003(030716ADG_5sec)\\", sep="")
#                              )

profiles_to_convert_dir <- c("C:\\Users\\Philo\\Box Sync\\ProvedIT_profiles\\PROVEDIt_1-Person Profiles_3500 5sec_GF29cycles\\5_sec\\RD14-0003(011316ADG_5sec)\\",
                             "C:\\Users\\Philo\\Box Sync\\ProvedIT_profiles\\PROVEDIt_1-Person Profiles_3500 5sec_GF29cycles\\5_sec\\RD14-0003(011516ADG_5sec)\\",
                             "C:\\Users\\Philo\\Box Sync\\ProvedIT_profiles\\PROVEDIt_1-Person Profiles_3500 5sec_GF29cycles\\5_sec\\RD14-0003(011816ADG_5sec)\\")


number_of_directories <- length(profiles_to_convert_dir)
convert_from_xml_first <- TRUE
#this takes some time
if (convert_from_xml_first){
  for (iDir in 1:number_of_directories){
   convert_xml_to_csv(profiles_to_convert_dir[iDir])
  }
}

#for each directory go through and for each csv produce a smoothed version and save it

for (iDir in 1:number_of_directories){
  
  SampleNames <- list.files(path = profiles_to_convert_dir[iDir], pattern="\\.csv")
  NoOfSamples <- length(SampleNames)
  
  for (iSample in 1:NoOfSamples){
    #gets the sample
    sampleName <- SampleNames[iSample]
    print(paste("smoothing sample ", sampleName, sep=""))
    
    #gets the raw profile path
    raw_profile_path <- paste(profiles_to_convert_dir[iDir], sampleName, sep="")
    
    #reads in the raw profile data
    raw_profile <- read.csv(file = raw_profile_path, sep=",", header = TRUE, stringsAsFactors = FALSE)
    #get_mock_EPG_from_scans(raw_profile)
    
    #baselines the profile
    basleined_profile <- baseline_raw_profile(raw_profile)
    
    #plot the profile to screen
    #get_mock_EPG_from_scans(basleined_profile)
    
    #smooths the profile data
    smoothed_profile <- smooth_profile(basleined_profile, 10) #threshold of 6 means 1 million to 1
    
    #plot the profile
    #get_mock_EPG_from_scans(smoothed_profile)
    
    #plots the baselined vs the smoothed
    #compare_raw_v_smoothed_from_scans(basleined_profile, smoothed_profile, paste(profiles_to_convert_dir[iDir], sampleName, sep=""))
    
    #plots a simple baselined vs smoothed (this shows more what the ANN will learn to emulate)
    sample_name_without_suffix <- substr(x = sampleName, start = 1, stop = nchar(sampleName) - 4)
    compare_raw_v_smoothed_from_scans(baseline_raw_profile_simple(raw_profile),
                                      smoothed_profile,
                                      paste(profiles_to_convert_dir[iDir], sample_name_without_suffix, sep="")
    )
    
    save_name <- paste(profiles_to_convert_dir[iDir], sample_name_without_suffix, "_smooth.csv", sep="")
    write.table(x = smoothed_profile, file = save_name, col.names = TRUE, row.names = FALSE, sep=",")
  }
}

