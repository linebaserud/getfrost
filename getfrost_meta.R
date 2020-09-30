#!/usr/bin/env Rscript 

# Function: retrieve meta data for specific station from frost.met.no
#
# Example: getfrost_meta(18700)
#
# 30 September 2020 
# lineb@met.no
#

# --------------------------------------------------------------

library(jsonlite)
library(tidyr)
library(readr)

source("err_messages.R")

# ------------------------------------------------------------

getfrost_meta <- function(station){

  client_id  <- as.character(read_tsv("client_id.txt", col_names = TRUE, cols(client_id = col_character())))
  url <- paste0("https://", client_id, "@frost.met.no/sources/v0.jsonld?",
                "ids=SN", station)

  output <- try(fromJSON(URLencode(url), flatten = TRUE))
  if (class(output) != 'try-error') {
    print(paste0("SN", station, ": metadata retrieved from frost.met.no! (", object.size(output), " bytes)"))
  } else {
    err_messages()
  }

  meta <- unnest(output$data, cols=c())

  return(meta)
}
