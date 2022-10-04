s.err <- function(x){
  temp <- sd(x)/sqrt(length(x))
  return(temp)
}

s.err.na <- function(x){
  temp <- sd(x, na.rm = T)/sqrt(length(which(!is.na(x))))
  return(temp)
}

calcVPD <- function(RH, temp){
  vpd <- 0.61365 * exp(17.502 * temp/(240.97 + temp)) * (1 -(RH/100))
  return(vpd)
}