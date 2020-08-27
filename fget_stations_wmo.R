#!/usr/bin/env Rscript

# Function: get dataframe with all wmo stations in either municipality, county or country

# Examples:
# df <- fget_stations("county", "TROMS OG FINNMARK")
# df <- fget_stations("municipality", "TROMSØ")
# df <- fget_stations("country", "NORGE")

# -----------------------------------------------------------------------------------

fget_stations_wmo <- function(area_type, area){

  client_id  <- "ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c"
  url <- paste0("https://", client_id, "@frost.met.no/sources/v0.jsonld?",
               area_type,"=", area,
               "&wmoid=*")
  output <- try(fromJSON(URLencode(url),flatten=T))
  df<-unnest(output$data,cols=c())

  return(df)
}






