# Load necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, readxl, dplyr, lubridate, openair, car)

# Checklist: modify the following parameters, move the pa_lv_hourly.csv file to the package directory
# move the LEO datasets to the package directory and rename them to STEPS_yyyy-mm.xlsx, GOODMAN_yyyy-mm.xls,
# MTN_Davis_yyyy-mm.xls. 

# To run this script correctly, you MUST hit source -> Source as Background Job

# Get the path of the currently running script 
wd <- getwd()
base_location <- paste0(wd, "/")
print(base_location)

# Parameters to modify to control output
# base_location <- "Path_to_your_directory)"
sensor_id <- "188163" #Sensor index for the PurpleAir sensor (what to search for in the dataset)
month <- "2024-12" #Month of the data to be collected
weather_station <- "STEPS" #Closest leo weather station to sensor (STEPS, MTN_Davis, GOODMAN)



# Suppress all warnings globally
options(warn = -1)

# Create the results directory if it does not exist
if(!file.exists(paste0(base_location, "results"))) {
  dir.create(paste0(base_location, "results"), showWarnings = FALSE)
}

create_data_set <- function(base_location, sensor_data, month) {
  
  inversion <- function(leo_goodman_dataset, leo_mountain_dataset) {
    # Initialize an empty vector to store the inversion results
    inversion_results <- vector("list", length(leo_steps_dataset$leo_temp))
    
    # Iterate through each value in the datasets
    for (i in seq_along(leo_steps_dataset$leo_temp)) {
      # Convert values to numeric
      leo_temp_steps <- as.numeric(leo_steps_dataset$leo_temp[i])
      leo_temp_mountain <- as.numeric(leo_mountain_dataset$leo_temp[i])
      leo_P_steps <- as.numeric(leo_steps_dataset$leo_P[i])
      leo_P_mountain <- as.numeric(leo_mountain_dataset$leo_P[i])
      
      # Check if the corresponding values are numeric and not NA
      if (!is.na(leo_temp_steps) && !is.na(leo_temp_mountain) &&
          !is.na(leo_P_steps) && !is.na(leo_P_mountain)) {
        
        # Perform the inversion calculation
        inversion_value <- (leo_temp_mountain - leo_temp_steps) / 
          (leo_P_steps - leo_P_mountain)
        
        # Replace NA values with '.'
        if (is.na(inversion_value)) {
          inversion_results[[i]] <- '.'
        } else {
          inversion_results[[i]] <- inversion_value
        }
      } else {
        # Add '.' if any value is not numeric or is NA
        inversion_results[[i]] <- '.'
      }
    }
    
    # Convert the list to a vector
    return(unlist(inversion_results))
  }
  
  output_file <- paste0(base_location, "results/", paste0("LEO_Master_", weather_station, "-", month, ".csv"))
  leo_dataset <- read.csv(paste0(base_location, paste0("leo_", weather_station, "_dataset.csv")), header = TRUE, sep = ",")
  leo_mountain_dataset <- read.csv(paste0(base_location, "leo_mountain_dataset.csv"), header = TRUE, sep = ",")
  leo_steps_dataset <- read.csv(paste0(base_location, "leo_steps_dataset.csv"), header = TRUE, sep = ",")
  purpleair_dataset <- read.csv(paste0(base_location, "pa_lv_hourly.csv"), header = TRUE, sep = ",")
  
  
  # Extract relavant columns from the leo dataset
  leo_temp <- leo_dataset$leo_temp
  leo_RH <- leo_dataset$leo_RH
  leo_WS <- leo_dataset$leo_WS
  leo_WD <- leo_dataset$leo_WD
  leo_P <- leo_dataset$leo_P
  leo_prec <- leo_dataset$leo_prec
  leo_inversion <- inversion(leo_steps_dataset, leo_mountain_dataset)
  
  #Scan the purpleair_dataset for the corresponding sensor_id and mark the start and end rows
  sensor_row <- purpleair_dataset$sensor_index 
  row_numbers <- which(sensor_row == sensor_id) 
  
  #Extract the data corresponding to the specified PM2.5 sensor
  sensor_data <- purpleair_dataset[row_numbers[1]:row_numbers[length(row_numbers)],]
  
  #Extract the month's data
  date_row <- sensor_data$datetime
  row_numbers <- which(grepl(month, date_row)) 
  sensor_data <- sensor_data[row_numbers[1]:row_numbers[length(row_numbers)],]
  
  # Replace '.' placeholders with NA if present
  sensor_data[sensor_data == "."] <- NA
  
  # Keep relevant columns (include A/B channels)
  sensor_data <- data.frame(
    datetime = sensor_data$datetime,
    pm25 = as.numeric(sensor_data$pm25),
    pm25_a = as.numeric(sensor_data$pm25_a),
    pm25_b = as.numeric(sensor_data$pm25_b)
  )
  
  # --- LOGGING SETUP ---
  log_entries <- data.frame(
    timestamp = Sys.time(),
    month = month,
    total_rows_start = nrow(sensor_data),
    removed_missing = NA,
    removed_diff = NA,
    removed_low = NA,
    total_rows_final = NA
  )
  
  # --- FILTERING RULES ---
  
  # 1️⃣ Remove rows with missing PM2.5 or channel data
  before <- nrow(sensor_data)
  sensor_data <- sensor_data %>%
    filter(!is.na(pm25), !is.na(pm25_a), !is.na(pm25_b))
  log_entries$removed_missing <- before - nrow(sensor_data)
  
  # 2️⃣ Remove rows where A and B differ by >30%
  before <- nrow(sensor_data)
  sensor_data <- sensor_data %>%
    filter(abs(pm25_a - pm25_b) / (max(pm25_a + pm25_b) <= 0.3)
  log_entries$removed_diff <- before - nrow(sensor_data)
  
  # 3️⃣ Remove rows where PM2.5 < 2
  before <- nrow(sensor_data)
  sensor_data <- sensor_data %>%
    filter(pm25 >= 2)
  log_entries$removed_low <- before - nrow(sensor_data)
  
  # --- FINALIZE ---
  # Recalculate pm25 as average of A and B
  sensor_data <- sensor_data %>%
    mutate(pm25 = (pm25_a + pm25_b) / 2) %>%
    select(datetime, pm25)
  
  log_entries$total_rows_final <- nrow(sensor_data)
  
  # --- WRITE LOG ---
  if (!dir.exists("results")) dir.create("results")
  log_file <- "results/filter_log.csv"
  
  if (file.exists(log_file)) {
    existing_log <- read.csv(log_file)
    combined_log <- rbind(existing_log, log_entries)
    write.csv(combined_log, log_file, row.names = FALSE)
  } else {
    write.csv(log_entries, log_file, row.names = FALSE)
  }
  
  #The following code will find all mising hours of sensor data in the month
  
  # Round the timestamps in sensor_data to the nearest hour
  sensor_data$datetime <- floor_date(as.POSIXct(sensor_data$datetime, format = "%Y-%m-%d %H:%M:%S"), unit = "hour")
  
  # Define the start and end dates for the specified month
  start_date <- as.POSIXct(paste0(month, "-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S")
  end_date <- as.POSIXct(paste0(month, "-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S") + months(1) - hours(1)
  all_hours <- seq.POSIXt(from = start_date, to = end_date, by = "hour")
  date_hour_df <- data.frame(datetime = all_hours)
  
  # Merge date_hour_df with sensor_data to identify missing hours filling in blank spaces with '.'
  merged_data <- merge(date_hour_df, sensor_data, by = "datetime", all.x = TRUE)
  merged_data$pm25[is.na(merged_data$pm25)] <- '.'
  
  # Add row index as a column to both datasets
  merged_data$index <- 1:nrow(merged_data)

  

  
  #Combine the data from pm2.5 and leo dataset
  min_length <- min(length(merged_data$pm25),
                    length(leo_temp),
                    length(leo_RH),
                    length(leo_WS),
                    length(leo_WD),
                    length(leo_P),
                    length(leo_prec),
                    length(leo_inversion))
  
  master_data <- data.frame(datetime = merged_data$datetime[1:min_length],
                            PM2.5 = merged_data$pm25[1:min_length],
                            leo_temp = leo_temp[1:min_length],
                            leo_RH = leo_RH[1:min_length],
                            leo_WS_MPH = leo_WS[1:min_length],
                            leo_WD = leo_WD[1:min_length],
                            leo_P = leo_P[1:min_length],
                            leo_prec = leo_prec[1:min_length],
                            leo_inversion = leo_inversion[1:min_length])    
  
  #Write to CSV
  write.csv(master_data, output_file, row.names = TRUE)
}

average_and_extract_MTP <- function(input_file, output_file) {
  # Custom function to calculate the average, skipping '.'
  average <- function(values) {
    numeric_values <- as.numeric(values[values != '.'])
    if (length(numeric_values) == 0) {
      return('.')
    } else {
      avg <- mean(numeric_values, na.rm = TRUE)
      if (is.nan(avg)) {
        return('.')
      } else {
        return(round(avg, 2))
      }
    }
  }
  # Read the Excel file
  df <- read_excel(input_file, sheet = "15-minute")
  
  # Extract the data for relative humidity and air temperature
  doy <- as.numeric(unlist(df[8:nrow(df), 1]))
  End_edt <- as.numeric(unlist(df[8:nrow(df), 2]))
  t_air_data <- as.numeric(unlist(df[8:nrow(df), 4]))
  rh_air_data <- as.numeric(unlist(df[8:nrow(df), 23]))
  ws_data <- as.numeric(unlist(df[8:nrow(df), 16]))
  wd_data <- unlist(df[8:nrow(df), 18])
  p_data <- as.numeric(unlist(df[8:nrow(df), 15]))
  prec_data <- as.numeric(unlist(df[8:nrow(df), 20]))
  
  # Remove any null data
  doy <- na.omit(doy)
  End_edt <- na.omit(End_edt)
  
  
  # Set null data to '.' 
  t_air_data[is.na(t_air_data)] <- '.'
  rh_air_data[is.na(rh_air_data)] <- '.'
  ws_data[is.na(ws_data)] <- '.'
  wd_data[is.na(wd_data)] <- '.'
  p_data[is.na(p_data)] <- '.'
  prec_data[is.na(prec_data)] <- '.'
  
  # Convert wind direction to number based on chart
  wd_data[wd_data == 'E'] <- 90.0
  wd_data[wd_data == 'ENE'] <- 67.5
  wd_data[wd_data == 'N'] <- 360.0
  wd_data[wd_data == 'NE'] <- 45.0
  wd_data[wd_data == 'NNE'] <- 22.5
  wd_data[wd_data == 'NNW'] <- 337.5
  wd_data[wd_data == 'NW'] <- 315.0 
  wd_data[wd_data == 'S'] <- 180.0 
  wd_data[wd_data == 'SE'] <- 135.0
  wd_data[wd_data == 'SSE'] <- 157.5
  wd_data[wd_data == 'SSW'] <- 202.5 
  wd_data[wd_data == 'SW'] <- 225.0 
  wd_data[wd_data == 'W'] <- 270.0 
  wd_data[wd_data == 'WNW'] <- 292.5 
  wd_data[wd_data == 'WSW'] <- 247.5 
  
  
  # Convert DOY to date format
  reference_date <- as.Date("2024-08-01") - 45505
  doy <- as.Date(doy, origin = reference_date)
  
  # Convert End_edt to hour and minute time
  End_edt_hours <- End_edt * 24
  End_edt_hour <- floor(End_edt_hours)
  End_edt_minute <- round((End_edt_hours - End_edt_hour) * 60)
  
  # Handle the case where minutes are 60
  End_edt_hour <- End_edt_hour + floor(End_edt_minute / 60)
  End_edt_minute <- End_edt_minute %% 60
  
  End_TIME  <- sprintf("%02d:%02d", End_edt_hour, End_edt_minute)
  
  # If relative humidity is greater than 100, or less than or equal to 2 set it to null
  rh_air_data[as.numeric(rh_air_data) > 100 | as.numeric(rh_air_data) <= 2] <- NA
  
  
  # The following averages calculate the mean of up to 4 data point (average
  # is calculated as long as there is at least 1, invalid data is skipped.)
  
  # Calculate the data using avaliable data
  t_air_hourly <- sapply(seq(1, length(t_air_data), by = 4), function(i) {
    average(t_air_data[i:min(i+3, length(t_air_data))])
  })
  rh_air_hourly <- sapply(seq(1, length(rh_air_data), by = 4), function(i) {
    average(rh_air_data[i:min(i+3, length(rh_air_data))])
  })
  ws_data <- sapply(seq(1, length(ws_data), by = 4), function(i) {
    average(ws_data[i:min(i+3, length(ws_data))])
  })
  wd_data <- sapply(seq(1, length(wd_data), by = 4), function(i) {
    average(wd_data[i:min(i+3, length(wd_data))])
  })
  p_data <- sapply(seq(1, length(p_data), by = 4), function(i) {
    average(p_data[i:min(i+3, length(p_data))])
  })  
  prec_data <- sapply(seq(1, length(prec_data), by = 4), function(i) {
    average(prec_data[i:min(i+3, length(prec_data))])
  })
  
  #Convert wind speed from m/s to miles per hour
  ws_data <- (ws_data * 3600) / 1609.34
  ws_data <- round(ws_data, 2)
  
  #Convert pressure from inches to millibars
  p_data <- (p_data * 33.8639)
  p_data <- round(p_data, 2)
  
  # Convert temperature to Celsius, excluding '.'
  t_air_hourly <- sapply(t_air_hourly, function(x) {
    if (x != '.') {
      return(round((as.numeric(x) - 32) * (5/9), 2))
    } else {
      return('.')
    }
  })
  
  # Extract every 4th element
  doy_every_4th <- doy[seq(1, length(doy), by = 4)]
  End_TIME  <- End_TIME[seq(1, length(End_TIME), by = 4)]
  
  # Ensure all vectors have the same length
  min_length <- min(length(doy_every_4th), length(End_TIME), length(t_air_hourly), length(rh_air_hourly))
  
  # Combine the results into a data frame with all columns
  summary_data <- data.frame(DOY = doy_every_4th[1:min_length], 
                             TIME = End_TIME[1:min_length],
                             leo_temp = t_air_hourly[1:min_length],
                             leo_RH = rh_air_hourly[1:min_length],
                             leo_WS = ws_data[1:min_length],
                             leo_WD = wd_data[1:min_length],
                             leo_P  = p_data[1:min_length],
                             leo_prec  = prec_data[1:min_length])
  
  # Write to CSV
  write.csv(summary_data, output_file, row.names = FALSE)
}

average_and_extract_STEPS <- function(input_file, output_file) {
  # Custom function to calculate the average, skipping '.'
  average_wd <- function(values) {
    numeric_values <- as.numeric(values[values != '.'])
    if (length(numeric_values) == 0) {
      return('.')
    } else {
      avg <- mean(numeric_values, na.rm = TRUE)
      if (is.nan(avg)) {
        return('.')
      } else {
        return(avg)
      }
    }
  }
  # Read the Excel file
  df <- read_excel(input_file, sheet = "Raw data")
  
  # Extract the data for relative humidity and air temperature
  Date_Time <- unlist(df[1:nrow(df), 2])
  t_air_data <- as.numeric(unlist(df[1:nrow(df), 3]))
  rh_air_data <- as.numeric(unlist(df[1:nrow(df), 6]))
  ws_data <- as.numeric(unlist(df[1:nrow(df), 10]))
  wd_data <- unlist(df[1:nrow(df), 14])
  p_data <- as.numeric(unlist(df[1:nrow(df), 76]))
  prec_data <- as.numeric(unlist(df[1:nrow(df), 15]))
  
  # Remove any null data
  Date_Time <- na.omit(Date_Time)
  t_air_data <- na.omit(t_air_data)
  rh_air_data <- na.omit(rh_air_data)
  ws_data <- na.omit(ws_data)
  p_data <- na.omit(p_data)
  prec_data <- na.omit(prec_data)
  
  
  # Set wind direction to NA if it is null
  wd_data[is.na(wd_data)] <- '.'
  
  # Convert wind direction to number based on chart
  wd_data[wd_data == 'E'] <- 90.0
  wd_data[wd_data == 'ENE'] <- 67.5
  wd_data[wd_data == 'N'] <- 360.0
  wd_data[wd_data == 'NE'] <- 45.0
  wd_data[wd_data == 'NNE'] <- 22.5
  wd_data[wd_data == 'NNW'] <- 337.5
  wd_data[wd_data == 'NW'] <- 315.0 
  wd_data[wd_data == 'S'] <- 180.0 
  wd_data[wd_data == 'SE'] <- 135.0
  wd_data[wd_data == 'SSE'] <- 157.5
  wd_data[wd_data == 'SSW'] <- 202.5 
  wd_data[wd_data == 'SW'] <- 225.0 
  wd_data[wd_data == 'W'] <- 270.0 
  wd_data[wd_data == 'WNW'] <- 292.5 
  wd_data[wd_data == 'WSW'] <- 247.5 
  
  
  
  # The following averages calculate the mean of up to 4 data point (average
  # is calculated as long as there is at least 1, invalid data is skipped.)
  
  # Calculate the average of every 4 data points for t_air_data
  t_air_hourly <- sapply(seq(1, length(t_air_data), by = 4), function(i) {
    mean(t_air_data[i:min(i+3, length(t_air_data))], na.rm = TRUE)
  })
  
  # Calculate the average of every 4 data points for rh_air_data
  rh_air_hourly <- sapply(seq(1, length(rh_air_data), by = 4), function(i) {
    mean(rh_air_data[i:min(i+3, length(rh_air_data))], na.rm = TRUE)
  })
  
  # Calculate the average of every 4 data points for ws_data
  ws_hourly <- sapply(seq(1, length(ws_data), by = 4), function(i) {
    mean(ws_data[i:min(i+3, length(ws_data))], na.rm = TRUE)
  }) 
  
  # Calculate the average of every 4 data points for wd_data if it is not '.'
  wd_hourly <- sapply(seq(1, length(wd_data), by = 4), function(i) {
    average_wd(wd_data[i:min(i+3, length(wd_data))])
  })
  
  # Calculate the average of every 4 data points for p_data if it is not '.'
  p_hourly <- sapply(seq(1, length(p_data), by = 4), function(i) {
    mean(p_data[i:min(i+3, length(p_data))], na.rm = TRUE)
  })
  
  prec_hourly <- sapply(seq(1, length(prec_data), by = 4), function(i) {
    mean(prec_data[i:min(i+3, length(prec_data))], na.rm = TRUE)
  })
  
  
  # Convert t_air_hourly to Celsius
  t_air_hourly <- (t_air_hourly - 32) * 5/9
  
  # convert p from inches to millibars
  p_hourly <- p_hourly * 33.8639
  
  # Round the data to 4 decimal places
  t_air_hourly <- round(t_air_hourly, 2)
  rh_air_hourly <- round(rh_air_hourly, 2)
  ws_hourly <- round(ws_hourly, 2)
  p_hourly <- round(p_hourly, 2)
  
  # Extract every 4th element
  Date_Time_every_4th <- Date_Time[seq(1, length(Date_Time), by = 4)]
  
  # Ensure all vectors have the same length
  min_length <- min(length(Date_Time), length(t_air_hourly), length(rh_air_hourly))
  
  # Combine the results into a data frame with all columns 
  summary_data <- data.frame(Date_Time = Date_Time_every_4th[1:min_length], 
                             leo_temp = t_air_hourly[1:min_length],
                             leo_RH = rh_air_hourly[1:min_length],
                             leo_WS = ws_hourly[1:min_length],
                             leo_WD = wd_hourly[1:min_length],
                             leo_P  = p_hourly[1:min_length],
                             leo_prec  = prec_hourly[1:min_length])
  # Write to CSV
  write.csv(summary_data, output_file, row.names = FALSE)
}


average_and_extract_GOODMAN <- function(input_file, output_file) {
  # Read the Excel file
  df <- read_excel(input_file, sheet = "15-minute data")
  
  # Extract the data for relative humidity and air temperature
  doy <- as.numeric(unlist(df[7:nrow(df), 3])) # Day of year data
  End_edt <- as.numeric(unlist(df[7:nrow(df), 4])) # End time data
  t_air_data <- as.numeric(unlist(df[7:nrow(df), 5])) # Air temperature data
  rh_air_data <- as.numeric(unlist(df[7:nrow(df), 6])) # Relative humidity data
  ws_data <- as.numeric(unlist(df[7:nrow(df), 10])) # Wind speed data
  wd_data <- as.numeric(unlist(df[7:nrow(df), 12])) # Wind direction data
  p_data <- as.numeric(unlist(df[7:nrow(df), 7])) # Pressure data
  prec_data <- as.numeric(unlist(df[7:nrow(df), 8]))
  
  # Remove any null data
  doy <- na.omit(doy)
  End_edt <- na.omit(End_edt)
  t_air_data <- na.omit(t_air_data)
  rh_air_data <- na.omit(rh_air_data)
  ws_data <- na.omit(ws_data)
  wd_data <- na.omit(wd_data)
  p_data <- na.omit(p_data)
  prec_data <- na.omit(prec_data)
  
  # Remove duplicate End_edt within the same doy
  data <- data.frame(doy, End_edt)
  data <- data %>%
    group_by(doy) %>%
    distinct(End_edt, .keep_all = TRUE) %>%
    ungroup()
  doy <- data$doy
  End_edt <- data$End_edt
  
  # The following averages calculate the mean of up to 4 data point (average
  # is calculated as long as there is at least 1, invalid data is skipped.)
  
  # Calculate the average of every 4 data points for t_air_data
  t_air_hourly <- sapply(seq(1, length(t_air_data), by = 4), function(i) {
    mean(t_air_data[i:min(i+3, length(t_air_data))], na.rm = TRUE)
  })
  
  # Calculate the average of every 4 data points for rh_air_data
  rh_air_hourly <- sapply(seq(1, length(rh_air_data), by = 4), function(i) {
    mean(rh_air_data[i:min(i+3, length(rh_air_data))], na.rm = TRUE)
  })
  
  # Calculate the average of every 4 data points for ws_data
  ws_data <- sapply(seq(1, length(ws_data), by = 4), function(i) {
    mean(ws_data[i:min(i+3, length(ws_data))], na.rm = TRUE)
  })
  
  # Calculate the average of every 4 data points for wd_data
  wd_data <- sapply(seq(1, length(wd_data), by = 4), function(i) {
    mean(wd_data[i:min(i+3, length(wd_data))], na.rm = TRUE)
  })
  
  # Calculate the average of every 4 data points for p_data
  p_data <- sapply(seq(1, length(p_data), by = 4), function(i) {
    mean(p_data[i:min(i+3, length(p_data))], na.rm = TRUE)
  })
  
  prec_data <- sapply(seq(1, length(prec_data), by = 4), function(i) {
    mean(prec_data[i:min(i+3, length(prec_data))], na.rm = TRUE)
  }) 
  
  #Convert wind speed from m/s to miles per hour
  ws_data <- (ws_data * 3600) / 1609.34
  
  # Round the data to 4 decimal places
  t_air_hourly <- round(t_air_hourly, 2)
  rh_air_hourly <- round(rh_air_hourly, 2)
  ws_data <- round(ws_data, 2)
  wd_data <- round(wd_data, 0)
  p_data <- round(p_data, 2)
  prec_data <- round(prec_data, 2)
  
  # Extract every 4th element
  End_edt <- c(as.numeric("00"), End_edt)
  doy_every_4th <- doy[seq(1, length(doy), by = 4)]
  End_edt_every_4th <- End_edt[seq(1, length(End_edt), by = 4)]
  
  # if wind direction is 0, set it to '.'
  wd_data[wd_data == 0] <- '.'
  prec_data[prec_data == 0] <- '.'
  
  # Ensure all vectors have the same length
  min_length <- min(length(doy_every_4th), length(End_edt_every_4th), length(t_air_hourly), 
                    length(rh_air_hourly), length(ws_data), length(wd_data), length(p_data), length(prec_data))
  
  # Combine the results into a data frame with all columns
  summary_data <- data.frame(DOY = doy_every_4th[1:min_length], 
                             End_EDT = End_edt_every_4th[1:min_length],
                             leo_temp = t_air_hourly[1:min_length],
                             leo_RH = rh_air_hourly[1:min_length],
                             leo_WS = ws_data[1:min_length],
                             leo_WD = wd_data[1:min_length],
                             leo_P = p_data[1:min_length],
                             leo_prec = prec_data[1:min_length])
  
  # Write to CSV
  write.csv(summary_data, output_file, row.names = FALSE)
}

windrose <- function(base_location, weather_station) {
  mydata <-  read.csv(paste0(base_location, paste0("leo_", weather_station, "_dataset.csv")), header = TRUE, sep = ",")
  #View(mydata)
  
  #windrose function
  ??windRose
  
  # Save the plot to a PNG file
  png(paste0(paste0(base_location, "results/"), paste0("windrose-", weather_station, "_", month, ".png")), width = 650, height = 650)
  
  
  #dafault windrose
  #windRose(mydata)
  
  #modified windrose
  windRose(mydata, ws = "leo_WS", wd = "leo_WD",
           breaks=c(0,2,5,8,11,17), #Change this to change the windspeed categories
           auto.text = FALSE,
           paddle = FALSE,
           annotate = TRUE,
           grid.line = 5,
           key = list(labels = c(">0 - 2", #Labels for the windspeed categories
                                 ">2 - 5",
                                 ">5 - 8",
                                 ">8 - 11",
                                 ">11 - 17",
                                 ">17")),
           key.footer = "WSP (mph)",
           key.position = "bottom",
           par.settings=list(axis.line=list(col="black")),
           col = c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f", "#d7153a", "#651FFF"),
           labels = c("N", "E", "S", "W")
  )
  # Use these settings if you want to use abs.count instead of percentage abundance
  # grid.line = 50
  # statistic = "abs.count" 
}



t.tester <- function() {
  #Read the data from master spreadsheet
  results_dir <- paste0(base_location, "results/")
  mydata <- read.csv(paste0(base_location, "results/", paste0("LEO_Master_", weather_station, "-", month, ".csv")))
  mydata$`PM2.5` <- as.numeric(as.character(mydata$`PM2.5`))
  
  # Filter the data based on PM2.5 values above 15
  above <- mydata %>%
    filter(`PM2.5` > 15)
  
  # Filter the data based on PM2.5 values below 15
  below <- mydata %>%
    filter(`PM2.5` < 15)
  
  # Combine the data into a single data frame with a group indicator
  above$group <- "above"
  below$group <- "below"
  combined_data <- rbind(above, below)
  
  # Force relevant columns to be numeric
  combined_data <- combined_data %>%
    mutate(
      leo_temp = as.numeric(as.character(leo_temp)),
      PM2.5 = as.numeric(as.character(`PM2.5`)),
      leo_RH = as.numeric(as.character(leo_RH)),
      leo_WS_MPH = as.numeric(as.character(leo_WS_MPH)),
      leo_WD = as.numeric(as.character(leo_WD)),
      leo_P = as.numeric(as.character(leo_P)),
      leo_prec = as.numeric(as.character(leo_prec)),
      leo_inversion = as.numeric(as.character(leo_inversion)))
  
  # Force relevant columns to be numeric
  above <- above %>%
    mutate(
      leo_temp = as.numeric(as.character(leo_temp)),
      PM2.5 = as.numeric(as.character(`PM2.5`)),
      leo_RH = as.numeric(as.character(leo_RH)),
      leo_WS_MPH = as.numeric(as.character(leo_WS_MPH)),
      leo_WD = as.numeric(as.character(leo_WD)),
      leo_P = as.numeric(as.character(leo_P)),
      leo_prec = as.numeric(as.character(leo_prec)),
      leo_inversion = as.numeric(as.character(leo_inversion)))
  
  # Force relevant columns to be numeric
  below <- below %>%
    mutate(
      leo_temp = as.numeric(as.character(leo_temp)),
      PM2.5 = as.numeric(as.character(`PM2.5`)),
      leo_RH = as.numeric(as.character(leo_RH)),
      leo_WS_MPH = as.numeric(as.character(leo_WS_MPH)),
      leo_WD = as.numeric(as.character(leo_WD)),
      leo_P = as.numeric(as.character(leo_P)),
      leo_prec = as.numeric(as.character(leo_prec)),
      leo_inversion = as.numeric(as.character(leo_inversion)))
  
  # Perform Levene's test to check for equality of variances
  #levene_test_result <- leveneTest(leo_temp ~ group, data = combined_data) 
  leo_temp_levene_test <- leveneTest(leo_temp ~ group, data = combined_data)
  PM2.5_levene_test <- leveneTest(PM2.5 ~ group, data = combined_data)
  leo_RH_levene_test <- leveneTest(leo_RH ~ group, data = combined_data)
  leo_WS_MPH_levene_test <- leveneTest(leo_WS_MPH ~ group, data = combined_data)
  leo_WD_levene_test <- leveneTest(leo_WD ~ group, data = combined_data)
  leo_P_levene_test <- leveneTest(leo_P ~ group, data = combined_data)
  leo_inversion_levene_test <- leveneTest(leo_inversion ~ group, data = combined_data)
  
  # Decide whether to assume equal variances based on Levene's test result
  leo_temp_equal_variances <- leo_temp_levene_test$`Pr(>F)`[1] > 0.05
  PM2.5_equal_variances <- PM2.5_levene_test$`Pr(>F)`[1] > 0.05
  leo_RH_equal_variances <- leo_RH_levene_test$`Pr(>F)`[1] > 0.05
  leo_WS_MPH_equal_variances <- leo_WS_MPH_levene_test$`Pr(>F)`[1] > 0.05
  leo_WD_equal_variances <- leo_WD_levene_test$`Pr(>F)`[1] > 0.05
  leo_P_equal_variances <- leo_P_levene_test$`Pr(>F)`[1] > 0.05
  leo_inversion_equal_variances <- leo_inversion_levene_test$`Pr(>F)`[1] > 0.05

  
  print("Levene's test results:")
  print(leo_temp_equal_variances)
  print(PM2.5_equal_variances)
  print(leo_RH_equal_variances)
  print(leo_WS_MPH_equal_variances)
  print(leo_WD_equal_variances)
  print(leo_P_equal_variances)
  print(leo_inversion_equal_variances)

  
  # Perform a two-sample t-test to compare the means of each variable for the two groups
  leo_temp_result <- t.test(above$leo_temp, below$leo_temp, var.equal = leo_temp_equal_variances)
  PM2.5_result <- t.test(above$PM2.5, below$PM2.5, var.equal = PM2.5_equal_variances)
  leo_RH_result <- t.test(above$leo_RH, below$leo_RH, var.equal = leo_RH_equal_variances)
  leo_WS_MPH_result <- t.test(above$leo_WS_MPH, below$leo_WS_MPH, var.equal = leo_WS_MPH_equal_variances)
  leo_WD_result <- t.test(above$leo_WD, below$leo_WD, var.equal = leo_WD_equal_variances)
  leo_P_result <- t.test(above$leo_P, below$leo_P, var.equal = leo_P_equal_variances)
  leo_inversion_result <- t.test(above$leo_inversion, below$leo_inversion, var.equal = leo_inversion_equal_variances)

  results <- data.frame(
    Variable = c("leo_temp", "PM2.5", "leo_RH", "leo_WS_MPH", "leo_WD", "leo_P", "leo_inversion"),
    P_value = c(leo_temp_result$p.value, PM2.5_result$p.value, leo_RH_result$p.value, 
                leo_WS_MPH_result$p.value, leo_WD_result$p.value, leo_P_result$p.value, leo_inversion_result$p.value),
    equal_variances_assumed = c(leo_temp_equal_variances, PM2.5_equal_variances, leo_RH_equal_variances,
                                leo_WS_MPH_equal_variances, leo_WD_equal_variances, leo_P_equal_variances, leo_inversion_equal_variances))
  # ---- Add percentile-based flags ----
  wind_thresh <- quantile(combined_data$leo_WS_MPH, 0.25, na.rm = TRUE)
  rh_thresh   <- quantile(combined_data$leo_RH, 0.75, na.rm = TRUE)
  
  combined_data <- combined_data %>%
    mutate(
      low_ws  = ifelse(leo_WS_MPH <= wind_thresh, 1, 0),
      high_rh = ifelse(leo_RH >= rh_thresh, 1, 0)
    )
  
  write.csv(results, paste0(results_dir, paste0("t_test_results_", sensor_id, "_", month, ".csv")), row.names = TRUE)
  write.csv(combined_data, paste0(results_dir, paste0("grouped_data_", sensor_id, "_", month, ".csv")), row.names = TRUE)
}


#Call to script to create mountaintop dataset
input_file <- paste0(base_location, paste0("MTN_Davis_", month, ".xls"))
output_file <- paste0(base_location, "leo_mountain_dataset.csv")
average_and_extract_MTP(input_file, output_file)

# Call to script to create steps dataset
input_file <- paste0(base_location, paste0("STEPS_", month, ".xlsx"))
output_file <- paste0(base_location, "leo_steps_dataset.csv")
average_and_extract_STEPS(input_file, output_file)

# Call to script to create goodman dataset
input_file <- paste0(base_location, paste0("GOODMAN_", month, ".xls"))
output_file <- paste0(base_location, "leo_goodman_dataset.csv")
average_and_extract_GOODMAN(input_file, output_file)



# Combine the data from all the datasets
create_data_set(base_location, sensor_id, month)

windrose(base_location, "goodman")
windrose(base_location, "steps")
windrose(base_location, "mountain")



t.tester()
generate_aq_condition_plots <- function(base_location, sensor_id, month) {
  pacman::p_load(ggplot2, fmsb, dplyr, tidyr)
  
  results_dir <- paste0(base_location, "results/")
  grouped_file <- paste0(results_dir, paste0("grouped_data_", sensor_id, "_", month, ".csv"))
  
  if (!file.exists(grouped_file)) {
    stop("Grouped data file not found. Please run t.tester() first.")
  }
  
  df <- read.csv(grouped_file)
  
  # Ensure numeric conversions
  df <- df %>%
    mutate(
      PM2.5 = as.numeric(PM2.5),
      leo_prec = as.numeric(leo_prec),
      leo_WS_MPH = as.numeric(leo_WS_MPH),
      leo_RH = as.numeric(leo_RH),
      leo_inversion = as.numeric(leo_inversion),
      low_ws = as.numeric(low_ws),
      high_rh = as.numeric(high_rh)
    )
  
  # Define “good” and “bad” air quality
  df_good <- df %>% filter(PM2.5 < 15)
  df_bad  <- df %>% filter(PM2.5 >= 15)
  
  # Define meteorological indicators
  calc_conditions <- function(data) {
    data %>%
      summarize(
        precipitation_presence = mean(leo_prec > 0, na.rm = TRUE) * 100,
        low_WS = mean(low_ws == 1, na.rm = TRUE) * 100,
        high_RH = mean(high_rh == 1, na.rm = TRUE) * 100,
        inversion_presence = mean(leo_inversion > 0, na.rm = TRUE) * 100
      )
  }
  
  good_conditions <- calc_conditions(df_good)
  bad_conditions  <- calc_conditions(df_bad)
  
  # Combine into one tidy frame for plotting
  combined <- bind_rows(
    good_conditions %>% mutate(category = "Good Air Quality"),
    bad_conditions %>% mutate(category = "Bad Air Quality")
  ) %>%
    pivot_longer(-category, names_to = "Condition", values_to = "Percentage")
  
  # --- BAR PLOTS ---
  bar_plot <- ggplot(combined, aes(x = Condition, y = Percentage, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#4CAF50", "#F44336")) +
    labs(title = paste("Meteorological Conditions During Air Quality Events -", month),
         x = "Condition", y = "Percentage (%)") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  
  ggsave(paste0(results_dir, "bar_air_quality_conditions_", month, ".png"),
         plot = bar_plot, width = 8, height = 5, dpi = 300)
  
  # --- RADAR PLOTS ---
  max_val <- 100
  min_val <- 0
  radar_data_good <- rbind(rep(max_val, 4), rep(min_val, 4), as.numeric(good_conditions[1, ]))
  radar_data_bad  <- rbind(rep(max_val, 4), rep(min_val, 4), as.numeric(bad_conditions[1, ]))
  colnames(radar_data_good) <- c("Precipitation", "Low WS", "High RH", "Inversion")
  colnames(radar_data_bad)  <- c("Precipitation", "Low WS", "High RH", "Inversion")
  
  png(paste0(results_dir, "radar_good_air_quality_", month, ".png"), width = 700, height = 700)
  radarchart(radar_data_good,
             axistype = 1,
             pcol = "#4CAF50",
             pfcol = scales::alpha("#4CAF50", 0.3),
             plwd = 3,
             title = paste("Good Air Quality Conditions (", month, ")", sep = ""))
  dev.off()
  
  png(paste0(results_dir, "radar_bad_air_quality_", month, ".png"), width = 700, height = 700)
  radarchart(radar_data_bad,
             axistype = 1,
             pcol = "#F44336",
             pfcol = scales::alpha("#F44336", 0.3),
             plwd = 3,
             title = paste("Bad Air Quality Conditions (", month, ")", sep = ""))
  dev.off()
  
  message("✅ Radar and bar plots saved in results/ folder.")
}
file.remove(paste0(base_location, "leo_steps_dataset.csv"))
file.remove(paste0(base_location, "leo_mountain_dataset.csv"))
file.remove(paste0(base_location, "leo_goodman_dataset.csv"))
