#!/usr/bin/env Rscript

# Function: Visualize / print statistics of sesonal precipitation distributions within county
#
# lineb@met.no
# August 2020
# 
# Example: regional_pdfs_precipitation("TROMS OG FINNMARK", 2015, 2019, "sum(precipitation_amount P1D)", 3)

# -----------------------------------------------------------------------

library("ggplot2")
library("dplyr")
library("gridExtra")
library("grid")

source("getfrost_stations_wmo.R")
source("getfrost_obs.R")
source("getfrost_data_coverage.R")
source("getfrost_ats.R")

# -----------------------------------------------------------------------

regional_pdfs_precipitation <- function(county, start_year, stop_year, element, nb_sd){

  stations <- getfrost_stations_wmo('county', county)

#  # Filter stations based on altitude range to account for vertical temperature gradient  
#  stations_altitude <- tibble()
#  for (idx in 1:length(stations$id)) {
#    if (stations$masl[idx] >= alt_min && stations$masl[idx] < alt_max) {
#      stations_altitude <- rbind(stations_altitude, stations[idx, 2:11])
#    }
#  }

  # Filter out stations that do not measure precip
  stations_precip <- tibble()
  for (idx in 1:length(stations$id)) {
    print("---------------------------------------------------------------------------------------------")
    if (getfrost_ats(substr(stations$id[idx], 3, 8),paste0(start_year, "-01-01"), paste0(stop_year, "-12-31"), element, t_offset)) {
      print(paste0( "  ", element, " exists between ", start, " and ", stop))
      stations_precip <- rbind(stations_precip, stations[idx, 2:11])
    }
  }
  print("---------------------------------------------------------------------------------------------")

  # Combine observations from all stations with data coverage and distribute by month
  t_offset <- "PT6H"
  t_resolution <- "P1D"
  coverage_limit <- 0.9
  data <- tibble(); stations_coverage <- tibble()
  for (i in 1:length(stations_precip$id)) {
    print("------------------------------------------------------------")
    data_tmp <- getfrost_obs(substr(stations_precip$id[i], 3, 8), paste0(start_year, "-01-01"), paste0(stop_year, "-12-31"), element, t_resolution, t_offset)

    # Add observations only if data coverage (and track these stations)
    if (getfrost_data_coverage(data_tmp$referenceTime, paste0(start_year, "-01-01"), paste0(stop_year, "-12-31"), t_resolution, coverage_limit)) {
      print(paste0("  Data coverage above ", coverage_limit * 100, " %, using data from SN",substr(stations_precip$id[i], 3, 8)))
      data <- rbind(data,cbind(data_tmp, mth = as.numeric(substr(data_tmp$referenceTime, 6, 7))))
      stations_coverage <- rbind(stations_coverage, stations_precip[i, ])
    } else {
      print(paste0("  Data coverage below ", coverage_limit * 100, " %"))
    }
  }
  print("------------------------------------------------------------")
  data_jan <- data[data$mth == "1", ]
  data_feb <- data[data$mth == "2", ]
  data_mar <- data[data$mth == "3", ]
  data_apr <- data[data$mth == "4", ]
  data_may <- data[data$mth == "5", ]
  data_jun <- data[data$mth == "6", ]
  data_jul <- data[data$mth == "7", ]
  data_aug <- data[data$mth == "8", ]
  data_sep <- data[data$mth == "9", ]
  data_oct <- data[data$mth == "10", ]
  data_nov <- data[data$mth == "11", ]
  data_dec <- data[data$mth == "12", ]

  rm(data, data_tmp, stations, stations_precip) # clean up 

  # Visualize distributions ---------------------------------------------
  
  xmin <- 0; xmax <- 50
  ymax <- 0.8
  ytxt <- 0.15

  plot_mth <- function(mth, data_mth, col, xmin, xmax, ymax, ytxt, xlab, ylab){
    p <- ggplot(data_mth, aes(x = value)) +
         geom_density(color = col) +
         geom_vline(aes(xintercept = mean(value)), color = col, linetype = "dashed", size = 0.5) +
         geom_vline(aes(xintercept = mean(value) + sd(value)), color = col, alpha = 0.5, linetype = "dashed", size = 0.5) +
        # geom_vline(aes(xintercept = mean(value) - sd(value)), color = col, alpha = 0.5, linetype = "dashed", size = 0.5) +
         scale_x_continuous(name = xlab, limits = c(xmin, xmax)) +
         scale_y_continuous(name = ylab, limits = c(0, ymax)) +
         annotate("text", x = xmax-10, y = ytxt, label = mth,hjust = 0, size = 4, fontface = "bold") +
         theme(axis.text.x = element_text(size = 14),
               axis.text.y = element_text(size = 14),
               axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
               panel.background = element_rect(fill = "transparent"), plot.background = element_rect(fill = "transparent", color = NA),
               panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    return(p)
  }

  p_jan <- plot_mth("January", data_jan, "blue", xmin, xmax, ymax, ytxt, "", "")
  p_feb <- plot_mth("February", data_feb, "blue", xmin, xmax, ymax, ytxt, "", "")
  p_mar <- plot_mth("March", data_mar, "green", xmin, xmax, ymax, ytxt, "", "Density")
  p_apr <- plot_mth("April", data_apr, "green", xmin, xmax, ymax, ytxt, "", "")
  p_may <- plot_mth("May", data_may, "green", xmin, xmax, ymax, ytxt, "", "")
  p_jun <- plot_mth("June", data_jun, "red", xmin, xmax, ymax, ytxt, "", "Density")
  p_jul <- plot_mth("July", data_jul, "red", xmin, xmax, ymax, ytxt, "", "")
  p_aug <- plot_mth("August", data_aug, "red", xmin, xmax, ymax, ytxt, "", "")
  p_sep <- plot_mth("September", data_sep, "brown", xmin, xmax, ymax, ytxt, "Temperature (°C)", "Density")
  p_oct <- plot_mth("October", data_oct, "brown", xmin, xmax, ymax, ytxt, "Temperature (°C)", "")
  p_nov <- plot_mth("November", data_nov, "brown", xmin, xmax, ymax, ytxt, "Temperature (°C)", "")
  p_dec <- plot_mth("December", data_dec, "blue", xmin, xmax, ymax, ytxt, "", "Density")

  grid.arrange(arrangeGrob(p_dec, top = '')
             , arrangeGrob(p_jan, top = textGrob(paste0(county, " ", start_year, "-", stop_year, " (", length(stations_coverage$id), " stations)"), gp = gpar(fontsize = 14)))
             , arrangeGrob(p_feb, top = '')
             , arrangeGrob(p_mar, top = '')
             , arrangeGrob(p_apr, top = '')
             , arrangeGrob(p_may, top = '')
             , arrangeGrob(p_jun, top = '')
             , arrangeGrob(p_jul, top = '')
             , arrangeGrob(p_aug, top = '')
             , arrangeGrob(p_sep, top = '')
             , arrangeGrob(p_oct, top = '')
             , arrangeGrob(p_nov, top = ''), nrow = 4)



  # Write statistics to .csv file --------------------------------------- 

  stat <- tibble()
  stat_mth <- function(mth, data_mth){
    m <- mean(data_mth$value); s <- sd(data_mth$value)
    max_val <- max(data_mth$value, na.rm = TRUE)
    min_val <- min(data_mth$value, na.rm=TRUE)
    stat <- rbind(stat, tibble(month = mth,
                               mean = round(m, 2),
                               sd = round(s, 2),
                               minus_nbSDxSD = round(m - (nb_sd * s), 2),
                               plus_nbSDxSD = round(m + (nb_sd * s), 2),
                               max = max_val,
                               min = min_val,
                               unit = data_mth$unit[1]))
    return(stat)
  }

  stat <- stat_mth("January", data_jan)
  stat <- stat_mth("February", data_feb)
  stat <- stat_mth("Mars", data_mar)
  stat <- stat_mth("April", data_apr)
  stat <- stat_mth("May", data_may)
  stat <- stat_mth("June", data_jun)
  stat <- stat_mth("July", data_jul)
  stat <- stat_mth("August", data_aug)
  stat <- stat_mth("September", data_sep)
  stat <- stat_mth("October", data_oct)
  stat <- stat_mth("November", data_nov)
  stat <- stat_mth("December", data_dec)

  meta_info <- tibble(element, county, t_offset, start_year, stop_year,  nb_sd)
  filename <- paste0("./output/stat_", element, "_", county, "_", start_year, "_", stop_year, "__", nb_sd, "sd.csv")

  write.table(meta_info, file = filename, sep = ";", row.names = FALSE)                                               # meta_information
  cat("\n", file = filename, append = TRUE)
  write.table(stat, append = TRUE, file = filename, sep = ";", col.names = TRUE, row.names = FALSE)                   # statistics
  cat("\n", file = filename, append = TRUE)
  write.table(stations_coverage, append = TRUE, file = filename, row.names = FALSE, col.names = TRUE, sep = ";")      # stations


}






