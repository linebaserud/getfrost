#!/usr/bin/env Rscript 

# Function: get all available timeseries for specific element from frost.met.no
#
# Example: getfrost_ats(18700,"max(air_temperature P1D)")
#
# 1 September 2020 
# lineb@met.no
#

# -------------------------------------------------------------------------------------------------

library(jsonlite)
library(tidyr)
library(readr)

source("err_messages.R")

# -------------------------------------------------------------------------------------------------

getfrost_ats <- function(station, element){

  client_id  <- as.character(read_tsv("client_id.txt", col_names = TRUE, cols(client_id = col_character())))
  url <- paste0("https://", client_id, "@frost.met.no/observations/availableTimeSeries/v0.jsonld?",
               "sources=SN", station)

  cat("----------------------------------------------------------------------------------------------------\n")
  output <- try(fromJSON(URLencode(url), flatten = TRUE))
  if (class(output) != 'try-error') {
    print(paste0("SN", station, ": information on available timeseries retrieved from frost.met.no! (", object.size(output), " bytes)"))
  } else {
    err_messages()
  }

  ats <- unnest(output$data, cols=c())
  ats_element <- ats[which(ats$elementId == element), ]

  return(ats_element)

}

