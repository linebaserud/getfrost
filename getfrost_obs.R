
# Function: get observations from frost.met.no
#
# Example: 
# getfrost_obs("SN18700", "2018-01-01","2018-02-02","max(air_temperature P1D)","P1D","PT18H")
# getfrost_obs("SN18700", "2018-01-01","2018-02-02","wind_speed","PT1H")
#
# NB: add client_id to a file named client_id.txt 

# -------------------------------------------------------------------------------------------------

library(jsonlite)
library(tidyr)
library(readr)

source("err_messages.R")

# -------------------------------------------------------------------------------------------------

getfrost_obs <- function(station, start, stop, element, t_resolution, t_offset = NA, levels = NA){
  
  client_id  <- as.character(read_tsv("client_id.txt", col_names = TRUE, cols(client_id = col_character())))
  url <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld?",
               "sources=", station,
               "&referencetime=", start, "T00:00/", stop, "T23:59",
               "&elements=", element,
               "&timeresolutions=", t_resolution)
  if(!is.na(t_offset)){url <- paste0(url, "&timeoffsets=", t_offset)}
  if(!is.na(levels)){url <- paste0(url, "&levels=", levels)}

  cat("------------------------------------------------------------------\n")

  output <- try(fromJSON(URLencode(url), flatten = TRUE))

  if (class(output) != 'try-error') {
    print(paste0(station, ": observations retrieved from frost.met.no! (", object.size(output), " bytes)"))

    obs <- unnest(output$data, cols = c(observations))
    obs$referenceTime <- as.POSIXct(strptime(x = as.character(obs$referenceTime), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")) # transform date format  
    return(obs)

  } else {
    err_messages()
  }
  
}



