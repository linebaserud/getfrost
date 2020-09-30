#
# Function for writing daily .txt files with observations, quality code, and meta data from a list of stations
#
# Example: getfrost_daily_txt(list('18700', '50540', '90450'), "2020-07-01", "2020-07-31", "mean(air_temperature P1D)", "P1D", "PT0H", "TAM")
#
# RBCN stations titanlab: station_list = list('4780', '16560', '16610', '18700', '24890', '36200', '44560', '47300', '50540', 
#                                             '62480', '69100', '71550', '80740', '82290', '90450', '90490', '97251', '98550', '99840')
#
# 30 September 2020
# lineb@met.no
#

source('~/projects/frost/R_scripts/getfrost/getfrost_obs.R')
source('~/projects/frost/R_scripts/getfrost/getfrost_meta.R')

# -----------------------------------------------------------------------------------

getfrost_daily_txt <- function(station_list, start, stop, element, t_resolution, t_offset, write_var){

  # Download data for entire time period (start-stop) for each station in station_list 
  obs <- tibble() 
  for (i in 1:length(station_list)) {
    obs_tmp <- getfrost_obs(station_list[i], start, stop, element, t_resolution, t_offset)
    meta_tmp <- getfrost_meta(station_list[i])     

    z <- meta_tmp$masl
    latlon_tmp <- meta_tmp$geometry.coordinates[[1]]
    lon <- latlon_tmp[1]  
    lat <- latlon_tmp[2]  
    wmoid <- meta_tmp$wmoId 
    stid <- as.numeric(substring(meta_tmp$id, 3)) 

    value <- obs_tmp$value
    qcode <- obs_tmp$qualityCode
    t_ref <- obs_tmp$referenceTime

    obs <- rbind(obs, tibble(lon, lat, z, stid, wmoid, qcode, value, t_ref))  

    rm(obs_tmp, meta_tmp, z, latlon_tmp, lat, lon, wmoid, stid, value, qcode, t_ref)
  }

  # Separate data into one data frame per day to write daily .txt files (one line per station)
  dates <- seq.Date(from = as.Date(start), to = as.Date(stop), by = 'days')
  for (j in 1:length(dates)) {

    z <- obs$z[which(as.Date(obs$t_ref) == as.Date(dates[j]))]
    lon <- obs$lon[which(as.Date(obs$t_ref) == as.Date(dates[j]))]
    lat <- obs$lat[which(as.Date(obs$t_ref) == as.Date(dates[j]))]
    wmoid <- obs$wmoid[which(as.Date(obs$t_ref) == as.Date(dates[j]))]
    stid <- obs$stid[which(as.Date(obs$t_ref) == as.Date(dates[j]))]
      
    value <- obs$value[which(as.Date(obs$t_ref) == as.Date(dates[j]))]
    qcode <- obs$qcode[which(as.Date(obs$t_ref) == as.Date(dates[j]))]
    t_ref <- as.Date(obs$t_ref[which(as.Date(obs$t_ref) == as.Date(dates[j]))])

    daily_obs <- tibble(lon, lat, z, stid, wmoid, qcode, value) # , t_ref)
    write.table(daily_obs, file = paste0("obs_", write_var, "_", substr(dates[j], 1, 4), substr(dates[j], 6, 7), substr(dates[j], 9, 10), ".txt"), sep = ";", row.names = FALSE)
#    write.table(daily_obs, file = paste0("./", substr(dates[j], 1, 4), "/", substr(dates[j], 6, 7), "/obs_", write_var, 
#                                         "_", substr(dates[j], 1, 4), substr(dates[j], 6, 7), substr(dates[j], 9, 10), ".txt"), sep = ";", row.names = FALSE) # save to folder ./yyyy/mm/
  }
  cat('--------------------------------------------------------------------\n')
  print(paste0("Writing daily files (obs_", write_var, "_yyyymmdd.txt)"))
}


