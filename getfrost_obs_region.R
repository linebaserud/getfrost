#!/usr/bin/env Rscript 

# region GR0 = Norge
# Liste over elementer man kan be om for Norge : https://frost.met.no/observations/availableTimeSeries/v0.jsonld?sources=GR0&fields=sourceId,elementId,validFrom,validTo
#
# Liste over alle regioner: https://frost.met.no/sources/v0.jsonld?types=RegionDataset&ids=GR*)

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



