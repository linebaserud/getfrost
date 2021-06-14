#
# Function: get combination of two elements under/over thresholds
#
# June 2021 
# lineb@met.no
#
# Example: 
# getfrost_ou2_threshold("SN18700", "2016-01-01","2018-12-31", "over", 28, "max(air_temperature P1D)", "P1D", "PT18H", "over", 14, "max(wind_speed_of_gust P1D)", "P1D")
#
# ---------------------------------------------------------------

source("getfrost_ou_threshold.R")

# ---------------------------------------------------------------

getfrost_ou2_threshold <- function(station, start, stop, over_under1, threshold1,  element1, t_resolution1, t_offset1 = NA, over_under2, threshold2,  element2, t_resolution2, t_offset2 = NA){

  df1 <- getfrost_ou_threshold(over_under1, threshold1, station, start, stop, element1, t_resolution1, t_offset1)
  df2 <- getfrost_ou_threshold(over_under2, threshold2, station, start, stop, element2, t_resolution2, t_offset2)

  if (df1$timeResolution[1] == df2$timeResolution[1]){
    index1 <- which(df_check$referenceTime %in% df_ref$referenceTime) 
    index2 <- which(df_ref$referenceTime   %in% df_check$referenceTime[index1])
    df_tot <- tibble(sourceId        = df1$sourceId[1],
                     referenceTime   = df1$referenceTime[index1],
                     elementId1      = df1$elementId[1],
                     value1          = df1$value[index1],
                     unit1           = df1$unit[1],
                     timeOffset1     = df1$timeOffset[1],
                     timeResolution1 = df1$timeResolution[1],
                     elementId2      = df2$elementId[1],
                     value2          = df2$value[index2],
                     unit2           = df2$unit[1],
                     timeOffset2     = df2$timeOffset[1],
                     timeResolution2 = df2$timeResolution[1])
  } else {
    print("Error: the function does not (yet) support cases where t_resolution1 != t_resolution2 !")
  }
  return(df_tot)
}



