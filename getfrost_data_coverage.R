#!/usr/bin/env Rscript

# Function: check data coverage of daily observations from frost.met.no
#
# lineb@met.no 
# 25 Aug 2020
#
# Example Isfjord Radio: 
# source('getfrost_obs.R'); D_tmp<-getfrost_obs(99790,"2019-01-01","2019-12-31","min(air_temperature P1D)","P1D","PT18H"); getfrost_data_coverage(D_tmp$referenceTime,2019,2019,"P1D",0.96))
#

# ------------------------------------------------------------------------------
#  

monnb <- function(d) {
  lt <- as.POSIXlt(as.Date(d, origin = "1900-01-01"))
  return(lt$year*12 + (lt$mon))
}

mondf <- function(d1, d2) {
  return(monnb(d2) - monnb(d1) + 1)
}

# ------------------------------------------------------------------------------

getfrost_data_coverage <- function(t_vec, start, stop, t_resolution, frac_limit) {
  
  if (t_resolution == "P1D") {
    period_full <- as.numeric(difftime(stop, start, units = c("days")))
  } else if (t_resolution == "PT1H") {
    period_full <- as.numeric(difftime(stop, start, units = c("hours")))
  } else if (t_resolution == "P1M") {
    period_full <- mondf(start,stop)
  } else if (t_resolution == "P1Y") {
    period_full <- as.numeric(substr(stop,1,4)) - as.numeric(substr(start,1,4))
  } else {
    print("t_resolution should be: P1D, PT1H, P1M, or P1Y")
  }
 
  period_min <- round(frac_limit*period_full)
  period2test <- length(t_vec)

  return(period2test >= period_min)
}


