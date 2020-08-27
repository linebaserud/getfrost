#!/usr/bin/env Rscript 

library(jsonlite)
library(tidyr)

# -------------------------------------------------------------------------------------------------

# get observations from Frost
fget_obs <- function(station, start, stop, element, t_resolution, t_offset){

  client_id  <- "ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c"
  url <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld?",
               "sources=SN", station,
               "&referencetime=", start, "T00:00/", stop, "T23:00",
               "&elements=", element,
               "&timeoffsets=", t_offset,
               "&timeresolutions=", t_resolution)

  output <- try(fromJSON(URLencode(url), flatten = T))
  if (class(output) != 'try-error') {print(paste0("SN", station, ": data retrieved from frost.met.no! (", object.size(output), " bytes)"))}
  else {print(paste0("SN", station, ": no success :-("))}
  df <- unnest(output$data, cols = c(observations))
  df$referenceTime <- as.POSIXct(strptime(x = as.character(df$referenceTime), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")) # transform date format  
  
  return(df)
}



