# Examples: 
# Svalbard lufthavn: getfrost_name("Svalbard lu*")
# Alle stasjoner som inneholder "berget": getfrost_name("*berget*") 
# 
# June 2021 
# lineb@met.no
#

# --------------------------------------------------------------

library(jsonlite)
library(tidyr)
library(readr)

source("err_messages.R")

# ------------------------------------------------------------

getfrost_name <- function(sources){

  client_id  <- as.character(read_tsv("client_id.txt", col_names = TRUE, cols(client_id = col_character())))
  url <- paste0("https://", client_id, "@frost.met.no/sources/v0.jsonld?",
                "name=", sources)

  output <- try(fromJSON(URLencode(url), flatten = TRUE))
  if (class(output) != 'try-error') {
    print(paste0(station, ": metadata retrieved from frost.met.no! (", object.size(output), " bytes)"))

    meta <- unnest(output$data, cols=c())
    return(meta)
  } else {
    err_messages()
  }
}

