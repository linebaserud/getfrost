#!/usr/bin/env Rscript 

library(jsonlite)
library(tidyr)

# -------------------------------------------------------------------------------------------------

# get available time series from Frost
getfrost_ats <- function(station, start, stop, element, t_offset){

  client_id  <- "ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c"
  url <- paste0("https://", client_id, "@frost.met.no/observations/availableTimeSeries/v0.jsonld?",
               "sources=SN", station)

  output <- try(fromJSON(URLencode(url), flatten = T))
  if (class(output) != 'try-error') {print(paste0("SN", station, ": information on available timeseries retrieved from frost.met.no! (", object.size(output), " bytes)"))}
  else {print(paste0("SN", station, ": no success :-("))}
  df <- unnest(output$data,cols=c())

  eid <- df$elementId
  tof=df$timeOffset
 # print(eid)
 # print(element)
  

  if (length(which(eid == element))>0) { # check if have this element

    idx=intersect(which(eid == element), which(tof == t_offset)) 
    validfrom_before_start <- as.numeric(difftime(start,df$validFrom[idx])) > 0
    validto_after_stop <- as.numeric(difftime(stop,df$validTo[idx])) < 0 || is.na(as.numeric(difftime(stop,df$validTo[idx])))

    if (validfrom_before_start && validto_after_stop){ # check if stations has data for påeriod start-stop
      return(TRUE)
    } else {
      return(FALSE)
    }

  } else {
    return(FALSE)
  }
}

