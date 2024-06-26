profile_generator_pre_training <- function(settings,
                                           complete_model, 
                                           number_epochs, 
                                           save_profile_images, 
                                           save_profile_csvs, 
                                           train_with_initial_sample_weights, 
                                           training_data){
  
  #libraries
  library('tensorflow')
  library('keras')
  library('tidyverse')
  #sources
  source('baseline_raw_profile_simple.R')

  #-------------------------  some global ANN settings ---------------------------
  #assigning local variables with the values from the settings list
  
  number_of_dyes <- settings$number_of_dyes
  startScan <- settings$startScan
  number_scanpoint_in_input_layer <- settings$number_of_scanpoints
  saturation <- settings$saturation
  profile_dirs <- settings$real_and_smooth_dirs
  fake_dirs <- settings$fake_dirs
  #the end scan point
  endScan <- startScan + number_scanpoint_in_input_layer - 1 # need to minus 1 so that startScan:endScan gives number_scanpoint_in_input_layer data points
  
  
  number_of_profiles <- dim(training_data$input_profiles)[1] 
  
  
  
  number_of_profiles_to_use <- dim(training_data$input_profiles)[1] 
  batch_size <- round(number_of_profiles_to_use/10, 0) # Each batch takes 8 profiles for training, and each epoch takes 10 batchs for training
  number_batches <- round(number_of_profiles_to_use/batch_size - 0.5, 0)
  
  ######################################################################################
  
  #formats the training inputs
  input_profiles_smooth <- array(training_data$input_profiles[1:number_of_profiles_to_use,,], dim=c(number_of_profiles_to_use,number_of_dyes,number_scanpoint_in_input_layer,1)) # 83,6,5000,1
  targets_profiles_real <- array(training_data$target_profiles[1:number_of_profiles_to_use,,], dim=c(number_of_profiles_to_use, number_of_dyes, number_scanpoint_in_input_layer,1))
  
  
  #the numbered input array to assist with sequential training 
  numbered_input <- matrix(rep(1:500, 6), ncol=500, byrow = TRUE)/500 
  numbered_input_array <- array(NA, dim=c(number_of_profiles_to_use, 6, 500, 1)) 
  
  for(iBatch in 1:number_of_profiles_to_use){
    numbered_input_array[iBatch, , , 1] <- numbered_input
  }
  

  
  loss_values <- NULL
  val_loss_values <- NULL
  
  
#   if(train_with_initial_sample_weights){
#     #fits the model but with the fixed random values generated upfront
#     history <- complete_model %>% fit(  # fit function is used to trains the model for a fixed number of epochs (iterations on a dataset)
# #      input_profiles_smooth,
#       list(input_profiles_smooth, numbered_input_array), # x: training data
#       targets_profiles_real, # y: target data
#       epochs = number_epochs,
#       batch_size = batch_size,
#       view_metrics = TRUE,
#       validation_split = 0.1, # separate test data from training data
#       sample_weight = input_profiles_smooth*100 + 1 #this just forces the classifier to concentrate on getting the peaks right
#     )
#     
#     loss_values <- c(loss_values, history$metrics$loss)
#     val_loss_values <- c(val_loss_values, history$metrics$val_loss)
#   }
  
  
  # now trains without the emphasis on peaks
  history <- complete_model %>% fit(
#    input_profiles_smooth,
    list(input_profiles_smooth, numbered_input_array),
    targets_profiles_real,
    epochs = number_epochs,
    batch_size = batch_size,
    view_metrics = TRUE,
    verbose = 1, # Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch)
    validation_split = 0.1
  )
  
  loss_values <- c(loss_values, history$metrics$loss)
  val_loss_values <- c(val_loss_values, history$metrics$val_loss)
  
  #plots the loss
  jpeg(file=paste(fake_dirs,"ANN_generator_pre-traing_loss.jpg", sep=""), width = 2000, height=2000, res=200)
    yplot_max <- max(loss_values, val_loss_values)
    yplot_min <- min(loss_values, val_loss_values)
    plot(loss_values, type='l', col="blue", ylim=c(yplot_min, yplot_max), ylab="MeanSquaredError", xlab="epoch", log='y')
    lines(val_loss_values, col="dark green")
    abline(v = number_epochs, col = "grey", lty = 2)
    legend("topright", col=c("blue", "dark green"), lwd=1, legend=c("loss", "val loss"))
  dev.off()
  
  #########################################################################################
  
  #-------------------------------------------------------------------------------------
  #shows the performance of the ANN in making the smoothed data from the training set look realistic
  if(save_profile_images | save_profile_csvs){
    
    #get predictions from training dataset
#    all_preds <- complete_model %>% predict(input_profiles_smooth)
    all_preds <- complete_model %>% predict(list(input_profiles_smooth, numbered_input_array))
    
    for (iProfile in 1:dim(all_preds)[1]){
      #the profile you wish to view
      pred_plot_profile <- iProfile
      
      #set saturation (leave as 1 if you just want to plot raw ANN outut)
      saturation <- settings$saturation
      #gets the data for that profile
      ANN_predicted_profile_data_plot <- all_preds[pred_plot_profile, , , 1]*saturation
      input_profiles_smooth_plot <- input_profiles_smooth[pred_plot_profile, , , 1]*saturation
      targets_profiles_real_plot <- targets_profiles_real[pred_plot_profile, , , 1]*saturation
      
      #get the number of dyes
      number_of_dyes <- dim(ANN_predicted_profile_data_plot)[1]
      
      #-------converts ANN output and training data input to dataframes--------
      #transpose data
      ANN_predicted_profile_data_plot.df <- t(ANN_predicted_profile_data_plot)
      #add scans
      ANN_predicted_profile_data_plot.df <- cbind(1:5000, ANN_predicted_profile_data_plot.df)
      #add dye names
      colnames(ANN_predicted_profile_data_plot.df) <- c("scan", sprintf("dye%s", 1:number_of_dyes))
      #convert to dataframe
      ANN_predicted_profile_data_plot.df <- as.data.frame(ANN_predicted_profile_data_plot.df)
      
      
      #transpose data
      input_profiles_smooth_plot.df <- t(input_profiles_smooth_plot)
      #add scans
      input_profiles_smooth_plot.df <- cbind(1:5000, input_profiles_smooth_plot.df)
      #add dye names
      colnames(input_profiles_smooth_plot.df) <- c("scan", sprintf("dye%s", 1:number_of_dyes))
      #convert to dataframe
      input_profiles_smooth_plot.df <- as.data.frame(input_profiles_smooth_plot.df)
      
      
      profile_name <- basename(training_data$input_paths[iProfile]) # removes all of the path up to and including the last path separator (if any).
      profile_name_without_suffix <- substr(x = profile_name, start = 1, stop = nchar(profile_name) - 4) # remove".csv"
      #calls the comparison plotting method
      source('compare_raw_v_smoothed_from_scans.R')
      save_name <- paste(fake_dirs, profile_name_without_suffix, iProfile, sep="")
      compare_raw_v_smoothed_from_scans(ANN_predicted_profile_data_plot.df, # predicted data, orange
                                        input_profiles_smooth_plot.df, # smoothed profile, black
                                        save_name,
                                        1,
                                        5000,
                                        1,
                                        5000,
                                        save_profile_images)
      
      if (save_profile_csvs){
        #saves predicted profile
        write.table(x = ANN_predicted_profile_data_plot.df,
                    file = paste(fake_dirs, profile_name_without_suffix, "_fake.csv", sep=""),
                    sep=",",
                    col.names = TRUE,
                    row.names = FALSE)
      }
    }
   
  }
  #---------------------------------------------------------------------------------------------------------------------------
   
  #returns the model
  return(complete_model)
}


