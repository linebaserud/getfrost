
# Function: get observations either over or under a threshold for one station
#
# June 2021
# lineb@met.no
#
# Example: 
# getfrost_ou_threshold("over", 25, "SN18700", "2016-01-01","2018-12-31", "max(air_temperature P1D)", "P1D", "PT18H")
# getfrost_ou_threshold("under", 10, "SN18700", "2005-01-01","2020-12-31", "mean(air_temperature P1M)", "P1M", "PT0H")
# getfrost_ou_threshold("over", 20.8, "SN52535", "2020-01-01","2020-12-31", "max(wind_speed PT1H)", "PT1H", "PT0H")
#
# NB: add client_id to a file named client_id.txt 
# note: add if no rows, message no data above/below
# -------------------------------------------------------------------------------------------------

source("getfrost_obs.R")

# -------------------------------------------------------------------------------------------------

getfrost_ou_threshold <- function(over_under, threshold, station, start, stop, element, t_resolution, t_offset = NA){
  
  df <- getfrost_obs(station, start, stop, element, t_resolution, t_offset)

  if (over_under == "over"){
    dates  <- df$referenceTime[which(df$value >= threshold)]
    values <- df$value[which(df$value >= threshold)]
  } else if (over_under == "under"){
    dates  <- df$referenceTime[which(df$value <= threshold)]
    values <- df$value[which(df$value <= threshold)]
  }

  len <- length(dates)
  df_threshold <- tibble(sourceId = rep(df$sourceId[1], len), 
                         elementId = rep(df$elementId[1], len), 
                         referenceTime = dates, 
                         value = values, 
                         unit = rep(df$unit[1], len), 
                         timeOffset = rep(df$timeOffset[1], len), 
                         timeResolution = rep(df$timeResolution[1], len))

  return(df_threshold)
}
