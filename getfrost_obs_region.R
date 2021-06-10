#
# Function: get regional observations from frost.met.no
#
# March 2021 / June 2021
# lineb@met.no
#
# List of all regions: https://frost.met.no/sources/v0.jsonld?types=RegionDataset&ids=GR*
# List of elements Norway : https://frost.met.no/observations/availableTimeSeries/v0.jsonld?sources=GR0&fields=sourceId,elementId,validFrom,validTo

# Examples:
# Norway:    getfrost_obs_region("GR0","1900-01-01","2020-01-01","mean(air_temperature_anomaly P1Y 1961_1990)")
# Trøndelag: getfrost_obs_region("GR4","2000-01-01","2020-01-01","sum(precipitation_amount P1M)")

# -------------------------------------------------------------------------------------------------

library(jsonlite)
library(tidyr)
library(readr)

source("~/projects/frost/R_scripts/getfrost/err_messages.R")

# -------------------------------------------------------------------------------------------------

# get observations from Frost
getfrost_obs_region <- function(region, start, stop, element){

  client_id  <- as.character(read_tsv("client_id.txt", col_names = TRUE, cols(client_id = col_character())))
  url <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld?",
               "sources=", region,
               "&referencetime=", start, "T00:00/", stop, "T23:59",
               "&elements=", element)

print(url)

  cat("------------------------------------------------------------------\n")
  output <- try(fromJSON(URLencode(url), flatten = TRUE))
  if (class(output) != 'try-error') {
    print(paste0("Observations retrieved from frost.met.no! (", object.size(output), " bytes)"))
  } else {
    err_messages()
  }

  obs <- unnest(output$data, cols = c(observations))
  obs$referenceTime <- as.POSIXct(strptime(x = as.character(obs$referenceTime), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")) # transform date format  

  return(obs)
}



