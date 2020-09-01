#!/usr/bin/env Rscript

# Function: get dataframe with all stations in either municipality, county or country
#
# lineb@met.no 
# 25 Aug 2020
#
# Examples:
# df <- getfrost_stations("county", "TROMS OG FINNMARK")
# df <- getfrost_stations("municipality", "TROMSØ")
# df <- getfrost_stations("country", "NORGE")

# -----------------------------------------------------------------------------------

getfrost_stations <- function(area_type, area){

  client_id  <- "ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c"
  url <- paste0("https://", client_id, "@frost.met.no/sources/v0.jsonld?",
               area_type,"=", area)
  output <- try(fromJSON(URLencode(url),flatten=T))
  df<-unnest(output$data,cols=c())

  return(df)
}






