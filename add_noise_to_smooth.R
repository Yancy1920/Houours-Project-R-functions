add_noise_to_smooth <- function(smooth){
  # Add simulated noise onto smooth profiles
  x1_114 <- arima.sim(list(order = c(1,1,4), ar=0.8462, ma=c(-1.4763,0.4934,0.0244,-0.0359)), n = 5100, sd=sqrt(0.0059254))
  x1_205 <- arima.sim(list(order = c(2,0,5), ar=c(1.7871,-0.7893), ma=c(-1.3612,0.4153,0.0043,-0.0129,-0.0255)), n = 5100)
  x2_113 <- arima.sim(list(order = c(1,1,3), ar=0.7152, ma=c(-1.3922,0.4326,-0.0323)), n = 5100, sd=sqrt(0.01708))
  # x2_202 <- arima.sim(list(order = c(2,0,2), ar=c(1.783,-0.0227), ma=c(-1.5352,0.5429)), n = 5000)
  x3_112 <- arima.sim(list(order = c(1,1,2), ar=0.8346, ma=c(-1.6054, 0.6114)), n=5100, sd=sqrt(0.002958333))
  x3_202 <- arima.sim(list(order = c(2,0,2), ar=c(1.8280,-0.8282), ma=c(-1.5337,0.5443)), n = 5100)
  x4_212 <- arima.sim(list(order = c(2,1,2), ar=c(0.8649,0.0085), ma=c(-1.6674,0.6696)), n = 5100, sd=sqrt(0.0068105))
  x4_112 <- arima.sim(list(order = c(1,1,2), ar=0.8576, ma=c(-1.616,0.62475)), n = 5100)
  x5_304 <- arima.sim(list(order = c(3,0,4), ar=c(0.3014,0.0454,0.1267), ma=c(0.3361,0.2900,0.1188,0.0806)), n = 5100, sd=sqrt(0.00818375))
  # x5_302 <- arima.sim(list(order = c(3,0,2), ar=c(1.272,-0.390,0.2605), ma=c(-0.5645,0.4393)), n = 5000)
  x6_113 <- arima.sim(list(order = c(1,1,3), ar=0.8042, ma=c(-1.4994,0.5256,-0.0227)), n = 5100, sd=sqrt(0.0102884))
  x6_112 <- arima.sim(list(order = c(1,1,2), ar=0.8002, ma=c(-1.5589,0.5669)), n = 5100)
  
  smooth_profiles <- smooth$smooth_profiles
  smooth_paths <- smooth$smooth_paths
  # add noise

  # dye 1
  for(iPoint in 1:dim(smooth_profiles)[3]){
    smooth_profiles[,1,iPoint] <- smooth_profiles[,1,iPoint] + x1_114[iPoint+100]
  }
  # dye 2
  for(iPoint in 1:dim(smooth_profiles)[3]){
    smooth_profiles[,2,iPoint] <- smooth_profiles[,2,iPoint] + x2_113[iPoint+100]
  }
  # dye 3
  for(iPoint in 1:dim(smooth_profiles)[3]){
    smooth_profiles[,3,iPoint] <- smooth_profiles[,3,iPoint] + x3_112[iPoint+100]
  }
  # dye 4
  for(iPoint in 1:dim(smooth_profiles)[3]){
    smooth_profiles[,4,iPoint] <- smooth_profiles[,4,iPoint] + x4_212[iPoint+100]
  }
  # dye 5
  for(iPoint in 1:dim(smooth_profiles)[3]){
    smooth_profiles[,5,iPoint] <- smooth_profiles[,5,iPoint] + x5_304[iPoint+100]
  }
  # dye 6
  for(iPoint in 1:dim(smooth_profiles)[3]){
    smooth_profiles[,6,iPoint] <- smooth_profiles[,6,iPoint] + x6_113[iPoint+100]
  }
  return(smooth_profiles)
  
  
  
}
