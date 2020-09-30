#!/usr/bin/env Rscript 

# Function: get observations from frost.met.no
#
# Example: getfrost_obs(18700, "2018-01-01","2018-02-02","max(air_temperature P1D)","P1D","PT18H")
#
# NB: add your client_id to a file named client_id.txt (see client_id_dummy.txt for format)

# -------------------------------------------------------------------------------------------------

library(jsonlite)
library(tidyr)
library(readr)

source("~/projects/frost/R_scripts/getfrost/err_messages.R")

# -------------------------------------------------------------------------------------------------

# get observations from Frost
getfrost_obs <- function(station, start, stop, element, t_resolution, t_offset){
  
  client_id  <- as.character(read_tsv("client_id.txt", col_names = TRUE, cols(client_id = col_character())))
  url <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld?",
               "sources=SN", station,
               "&referencetime=", start, "T00:00/", stop, "T23:59",
               "&elements=", element,
               "&timeoffsets=", t_offset,
               "&timeresolutions=", t_resolution,
               "&includeextra=",1)

  cat("------------------------------------------------------------------\n")
  output <- try(fromJSON(URLencode(url), flatten = TRUE))
  if (class(output) != 'try-error') {
    print(paste0("SN", station, ": observations retrieved from frost.met.no! (", object.size(output), " bytes)"))
  } else {
    err_messages()
  }
  
  obs <- unnest(output$data, cols = c(observations))
  obs$referenceTime <- as.POSIXct(strptime(x = as.character(obs$referenceTime), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")) # transform date format  
  
  return(obs)
}



