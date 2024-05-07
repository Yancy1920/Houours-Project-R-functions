compare_raw_v_smoothed_from_scans <- function(profile_data,
                                              smoothed_profile,
                                              sample_name_with_path,
                                              startScan_profile = 4000,
                                              endScan_profile = 9000,
                                              startScan_smoothed = 4000,
                                              endScan_smoothed = 9000,
                                              save_jpg = TRUE,
                                              real_profile = NULL,
                                              startScan_real = 4000,
                                              endScan_real = 9000){
  
  library('ggplot2')
  library('gridExtra')
  final_column <- (dim(profile_data)[2])
  plot_max_rfu = max(max(as.numeric(unlist(profile_data[startScan_profile:endScan_profile, 2:final_column]))),
                     max(as.numeric(unlist(smoothed_profile[startScan_profile:endScan_profile, 2:final_column]))))
  plot_min_rfu = min(min(as.numeric(unlist(profile_data[startScan_profile:endScan_profile, 2:final_column]))),
                     min(as.numeric(unlist(smoothed_profile[startScan_profile:endScan_profile, 2:final_column]))))
  
  number_of_dyes <- dim(smoothed_profile)[2] - 1
  
  mockEPGPlot <- vector('list', length(number_of_dyes))
  for (dye in 1:number_of_dyes){
    p <- ggplot()
    if (!is.null(real_profile)){
      p <- p + geom_line(data = real_profile[startScan_real:endScan_real,], aes_string(x = "scan", y = paste("dye", dye, sep="")), col = "grey", linewidth = 0.1)
    }
    p <- p + geom_line(data = profile_data[startScan_profile:endScan_profile,], aes_string(x = "scan", y = paste("dye", dye, sep="")), col = "orange", linewidth = 0.1)
    p <- p + geom_line(data = smoothed_profile[startScan_smoothed:endScan_smoothed,], aes_string(x = "scan", y = paste("dye", dye, sep="")), col = "black", linewidth = 0.1)
    p <- p + theme(legend.position = "none", axis.text.x = element_text(size = 5))
    p <- p + ylim(plot_min_rfu, plot_max_rfu)
    p <- p + xlab("")
    p <- p + ylab("RFU")
    p <- p + theme_classic()
    mockEPGPlot[[dye]] <- p
  }
  full_EPG_plot <- do.call("grid.arrange", c(mockEPGPlot, ncol = 1))
  if(save_jpg){
    save_name <- paste(sample_name_with_path, ".jpg", sep="")
    ggsave(save_name, full_EPG_plot, width = 15, height = 8, dpi = 600, units = "in")
  }
}
