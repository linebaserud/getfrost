
# Function: get available timeseries from frost.met.no 
#
# ----------------------------------------------------------------------------------
#
# Examples:
# Get all available timeseries from Oslo-Blindern: 
# getfrost_ats("SN18700")
#
# Get all available timeseries for wind speed from Oslo-Blindern: 
# getfrost_ats("SN18700","wind_speed")
#
# Check if data for combination of station, weather element and time resolution: 
# getfrost_ats("SN18700","wind_speed", "PT1H")
#
# Check if data for combination of station, weather element and period: 
# getfrost_ats("SN18700","wind_speed", NA, "1952-01-01", "1952-01-02")
#
# Get all available timeseries for combination of weather element, and time resolution 
# getfrost_ats(NA,"surface_snow_thickness", "PT1H")
#
# ------------------------------------------------------------------------------------
#
# September 2020 / March 2021
# lineb@met.no
#
# NB: add client_id to a file named client_id.txt 


# -------------------------------------------------------------------------------------------------

library(jsonlite)
library(tidyr)
library(readr)

source("err_messages.R")

# -------------------------------------------------------------------------------------------------

getfrost_ats <- function(station = NA, element = NA, t_resolution = NA, start = NA, stop = NA){

  client_id  <- as.character(read_tsv("client_id.txt", col_names = TRUE, cols(client_id = col_character())))

  url <- paste0("https://", client_id, "@frost.met.no/observations/availableTimeSeries/v0.jsonld?")

  if(!is.na(station)){url <- paste0(url, "sources=", station)}
  if(!is.na(element) & is.na(station)){url <- paste0(url, "elements=", element)}
  if(!is.na(element) & !is.na(station)){url <- paste0(url, "&elements=", element)}

  if(!is.na(t_resolution)){url <- paste0(url, "&timeresolutions=", t_resolution)}
  if(!is.na(start) & !is.na(stop)){url <- paste0(url, "&referencetime=", start, "T00:00/", stop, "T23:59")}

  cat("-------------------------------------------------------------------------\n")

  output <- try(fromJSON(URLencode(url), flatten = TRUE))

  if (class(output) != 'try-error') {
    if(is.na(station)){
      print(paste0("Information on available timeseries retrieved from frost.met.no! (", object.size(output), " bytes)"))
    } else {
      print(paste0(station, ": information on available timeseries retrieved from frost.met.no! (", object.size(output), " bytes)"))
    }
    ats <- unnest(output$data, cols = c())
    return(ats)
  } else {
    err_messages()
  }

}

