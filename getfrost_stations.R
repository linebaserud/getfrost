
# Function: get dataframe with all stations in either municipality, county or country
#
# 25 Aug 2020
# lineb@met.no 
#
# Examples:
# df <- getfrost_stations("county", "TROMS OG FINNMARK")
# df <- getfrost_stations("municipality", "TROMSØ")
# df <- getfrost_stations("country", "NORGE")
# 
# NB: add your client_id to a file named client_id.txt

# -----------------------------------------------------------------------------------

library(jsonlite)
library(tidyr)
library(readr)

# -----------------------------------------------------------------------------------

getfrost_stations <- function(area_type, area){
  
  client_id  <- as.character(read_tsv("client_id.txt", col_names = TRUE, cols(client_id = col_character())))
  
  url <- paste0("https://", client_id, "@frost.met.no/sources/v0.jsonld?", area_type, "=", area)
  output <- try(fromJSON(URLencode(url), flatten = TRUE))
  df <- unnest(output$data, cols = c())

  return(df)
}






