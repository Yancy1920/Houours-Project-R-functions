smooth_profile <- function(raw_profile, probability_threshold){
  
#  raw_profile <-  basleined_profile
  
  source('peak_detection.R')
  
  #the number of dyes
  number_dyes <- dim(raw_profile)[2] - 1
  #makes a copy of the profile to return
  return_profile <- raw_profile
  #goes through each dye
  for (idye in 2:(number_dyes+1)){
    
    print(paste("starting dye ", idye, sep=""))
    
    #identify peaks at certain thresholds and store position and height
    peak_probs_for_dye <- peak_detection(raw_profile[,idye])
    
    #screen out non-peaks below probability threhold
    thinned_peak_probs_for_dye <- peak_probs_for_dye
    thinned_peak_probs_for_dye[thinned_peak_probs_for_dye < probability_threshold] <- 0
    #use a min-max function so that each peak is left with one central scan point
    scan_window <- 10
    # for (iScan in 1:(length(thinned_peak_probs_for_dye)- 1)){
    #   last_scan_in_window <- min(length(thinned_peak_probs_for_dye), (iScan + scan_window))
    #   if (max(thinned_peak_probs_for_dye[(iScan + 1):last_scan_in_window]) >= thinned_peak_probs_for_dye[iScan]){
    #     thinned_peak_probs_for_dye[iScan] <- 0
    #   }
    # }
    
    
    for (iScan in 1:(length(thinned_peak_probs_for_dye) - scan_window)){
      max_prob_in_scan_window <- max(thinned_peak_probs_for_dye[iScan:(iScan + scan_window)])
      indices_of_non_maxes <- which(thinned_peak_probs_for_dye[iScan:(iScan + scan_window)] != max_prob_in_scan_window)
      thinned_peak_probs_for_dye[iScan:(iScan + scan_window)][indices_of_non_maxes] <- 0
    }
    
    
    #screen out values prior ro primer flare
    thinned_peak_probs_for_dye[1:4000] <- 0
    #screen out any peak centres for which profile has below set fluorecsence
    thinned_peak_probs_for_dye[raw_profile[,idye] < 20] <- 0
    
    #now get heights for each peak position
    peak_centres_at_indices <- which(thinned_peak_probs_for_dye > 0)
    heights_of_peak_centres <- raw_profile[peak_centres_at_indices,idye]
    
    print(paste("found ", length(peak_centres_at_indices), " peaks", sep=""))
    #draws the peaks
    plot_y <- rep(0, length( raw_profile[,idye]))
    if (length(peak_centres_at_indices) > 0){
      #now create smoothed profile from these centres
      peak_width <- 4
      plot_x <- 1:length( raw_profile[,idye])
      for (peak in 1:length(peak_centres_at_indices)){
        current_size <- peak_centres_at_indices[peak]
        current_height <- heights_of_peak_centres[peak]
        #multiplier so that rfu = height of graph peaks
        peak_height_add_array <- dnorm(plot_x, mean = current_size, sd = peak_width)
        multiplier <- current_height/max(peak_height_add_array, na.rm = TRUE)
        plot_y <- plot_y + multiplier*peak_height_add_array
      }
    }
    #smooths
 #   k_smoothed <- ksmooth(y = raw_profile[,idye], x = raw_profile[,1], "normal", bandwidth = bandwidth)
#    return_profile[[idye]] <- k_smoothed$y

    return_profile[[idye]] <- plot_y
    
    print(paste("finished dye ", idye, sep=""))
  }
  #returns profile
  return(return_profile)
  
}

#  lower <- 4000
#  upper <- 5500
# 
# plot(basleined_profile[,idye], col="orange", lwd=2, type='l', ylim=c(-10, 1300), xlim=c(lower, upper))
# lines(x = lower:upper, y = peak_probs_for_dye[lower:upper])
# lines(x = lower:upper, y = thinned_peak_probs_for_dye[lower:upper])
# lines(x = lower:upper, y = return_profile[[idye]][lower:upper])
