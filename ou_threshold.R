
# Function: get observations either over or under a threshold for one station
#
# June 2021
# lineb@met.no
#
# Example: 
# ou_threshold("above", 25, "SN18700", "2016-01-01","2018-12-31", "max(air_temperature P1D)", "P1D", "PT18H")
# ou_threshold("below", 10, "SN18700", "2005-01-01","2020-12-31", "mean(air_temperature P1M)", "P1M", "PT0H")
# ou_threshold("above", 20.8, "SN52535", "2020-01-01","2020-12-31", "max(wind_speed PT1H)", "PT1H", "PT0H")
#
# NB: add client_id to a file named client_id.txt 
# note: add if no rows, message no data above/below
# -------------------------------------------------------------------------------------------------

source("getfrost_obs.R")

# -------------------------------------------------------------------------------------------------

ou_threshold <- function(above_below, threshold, station, start, stop, element, t_resolution, t_offset){
  
  df <- getfrost_obs(station, start, stop, element, t_resolution, t_offset)

  if (above_below == "above"){
    dates  <- df$referenceTime[which(df$value >= threshold)]
    values <- df$value[which(df$value >= threshold)]
  } else if (above_below == "below"){
    dates  <- df$referenceTime[which(df$value >= threshold)]
    values <- df$value[which(df$value >= threshold)]
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
