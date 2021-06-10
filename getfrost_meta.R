
# Function: get meta data for stations from frost.met.no

# Examples: 
# Oslo Blindern and Bergen Florida: getfrost_meta("SN18700, SN50540")
# All stations: getfrost_meta("") 
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
                "ids=", station)

  output <- try(fromJSON(URLencode(url), flatten = TRUE))
  if (class(output) != 'try-error') {
    print(paste0(station, ": metadata retrieved from frost.met.no! (", object.size(output), " bytes)"))
    
    meta <- unnest(output$data, cols=c())
    return(meta)
  } else {
    err_messages()
  }
}
