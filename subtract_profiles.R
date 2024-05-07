subtract_profiles <- function(true_profiles, fake_profiles){
  noise <- array(0, dim = dim(true_profiles))
  for(iNoise in 1:dim(true_profiles)[3]){
    noise[,,iNoise] <- true_profiles[,,iNoise] - fake_profiles[,,iNoise]
  }
  return(noise)
}