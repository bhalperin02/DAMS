# This function creates DF to hold measure of center metrics for all three times
# and calculates all measure of center metrics from daily mean flows. if() {} 
# statements means the function only runs if flow data is available
# Updated 2.13.25 BH

# Inputs: daily_flow  = df for all daily flow
#         time1_daily_flow  = df for daily flow for time1
#         time2_daily_flow  = df for daily flow for time2
#         time3_daily_flow  = df for daily flow for time3
# 
# Outputs: Outputs mean and median for each yeah in each time period.
# If no data frame prints in OutputComplete for a time period, that means
# there was not data collection during that time period.

MeasureOfCenterMetrics <- function (inp1 = daily_flow,
                                      inp2 = time1_daily_flow,
                                      inp3 = time2_daily_flow,
                                      inp4 = time3_daily_flow) {
  
  ## Add year as independent column to input data frames 
  #full time
  if(nrow(inp1) > 0) {
    daily_flow$year <- year(daily_flow$Date)
  }
  
  #time 1
  if(nrow(inp2) > 0) {
    time1_daily_flow$year <- year(time1_daily_flow$Date)
  }  

  #time 2
  if(nrow(inp3) > 0) {
    time2_daily_flow$year <- year(time2_daily_flow$Date)
  }  
  
  #time 3
  if(nrow(inp4) > 0) {
    time3_daily_flow$year <- year(time3_daily_flow$Date)
  }  
  
  ##Removal of inc years
  #full time
  if(nrow(inp1) > 0) {
    daily_flow <- subset(daily_flow, inc_yr1 < year & year < inc_yr4)
  }
  
  #time 1
  if(nrow(inp2) > 0) {
    time1_daily_flow <- subset(time1_daily_flow, inc_yr1 < year & year < inc_yr2)
  }
  
  #time 2
  if(nrow(inp3) > 0) {
    time2_daily_flow <- subset(time2_daily_flow, inc_yr2 < year & year < inc_yr3)
  }
  
  #time 3
  if(nrow(inp4) > 0) {
    time3_daily_flow <- subset(time3_daily_flow, inc_yr3 < year & year < inc_yr4)
  }
  
  ## Measure of Center Metrics DF Creation 
  #full time
  if(nrow(inp1) > 0) {
    measure_of_center_metrics_full <- data.frame(matrix(nrow =
                                                         length(
                                                           unique(
                                                             daily_flow$year))))
    measure_of_center_metrics_full <- measure_of_center_metrics_full[ , NULL]
  }

  #time 1
  if(nrow(inp2) > 0) {
    measure_of_center_metrics_time1 <- data.frame(matrix(nrow =
                                                          length(
                                                            unique(
                                                              time1_daily_flow$
                                                                year))))
    measure_of_center_metrics_time1 <- measure_of_center_metrics_time1[ , NULL]
  }

  #time 2
  if(nrow(inp3) > 0) {
    measure_of_center_metrics_time2 <- data.frame(matrix(nrow =
                                                          length(
                                                            unique(
                                                              time2_daily_flow$
                                                                year))))
    measure_of_center_metrics_time2 <- measure_of_center_metrics_time2[ , NULL]
  }

  #time3
  if(nrow(inp4) > 0) {
    measure_of_center_metrics_time3 <- data.frame(matrix(nrow =
                                                          length(
                                                            unique(
                                                              time3_daily_flow$
                                                                year))))
    measure_of_center_metrics_time3 <- measure_of_center_metrics_time3[ , NULL]
  }

  ## Full Time Measure of Center Metrics
  if(nrow(inp1) > 0) {
    #find annual median for each year
    full_median <- aggregate(X_00060_00003 ~ year, 
                             data = daily_flow, FUN = median)
    
    #find annual mean for each year
    full_mean <- aggregate(X_00060_00003 ~ year, 
                           data = daily_flow, FUN = mean)
    
    #add mean and median to metric df
    measure_of_center_metrics_full <- merge(full_median, full_mean, 
                                            by = "year", all.x = TRUE)
    
    #rename median and mean columns for clarity
    colnames(measure_of_center_metrics_full) <- c('year', 'median', 'mean')
  }

  ## Time 1 Measure of Center Metrics
  if(nrow(inp2) > 0) {
    #find annual median for each year
    time1_median <- aggregate(X_00060_00003 ~ year, 
                             data = time1_daily_flow, FUN = median)
    
    #find annual mean for each year
    time1_mean <- aggregate(X_00060_00003 ~ year, 
                           data = time1_daily_flow, FUN = mean)
    
    #add mean and median to metric df
    measure_of_center_metrics_time1 <- merge(time1_median, time1_mean, 
                                            by = "year", all.x = TRUE)
    
    #rename median and mean columns for clarity
    colnames(measure_of_center_metrics_time1) <- c('year', 'median', 'mean')
  }
  
  ## Time 2 Measure of Center Metrics
  if(nrow(inp3) > 0) {
    #find annual median for each year
    time2_median <- aggregate(X_00060_00003 ~ year, 
                              data = time2_daily_flow, FUN = median)
    
    #find annual mean for each year
    time2_mean <- aggregate(X_00060_00003 ~ year, 
                            data = time2_daily_flow, FUN = mean)
    
    #add mean and median to metric df
    measure_of_center_metrics_time2 <- merge(time2_median, time2_mean, 
                                             by = "year", all.x = TRUE)
    
    #rename median and mean columns for clarity
    colnames(measure_of_center_metrics_time2) <- c('year', 'median', 'mean')
  }
  
  ## Time 3 Measure of Center Metrics
  if(nrow(inp4) > 0) {
    #find annual median for each year
    time3_median <- aggregate(X_00060_00003 ~ year, 
                              data = time3_daily_flow, FUN = median)
    
    #find annual mean for each year
    time3_mean <- aggregate(X_00060_00003 ~ year, 
                            data = time3_daily_flow, FUN = mean)
    
    #add mean and median to metric df
    measure_of_center_metrics_time3 <- merge(time3_median, time3_mean, 
                                             by = "year", all.x = TRUE)
    
    #rename median and mean columns for clarity
    colnames(measure_of_center_metrics_time3) <- c('year', 'median', 'mean')
  }
  
  ## return
  output <- list()
  
  if (exists("measure_of_center_metrics_full")) {
    output$measure_of_center_metrics_full = measure_of_center_metrics_full
  } else {
    output$measure_of_center_metrics_full = data.frame()
  } 
  
  if (exists("measure_of_center_metrics_time1")) {
    output$measure_of_center_metrics_time1 = measure_of_center_metrics_time1
  } else {
    output$measure_of_center_metrics_time1 = data.frame()
  } 
  
  if (exists("measure_of_center_metrics_time2")) {
    output$measure_of_center_metrics_time2 = measure_of_center_metrics_time2
  } else {
    output$measure_of_center_metrics_time2 = data.frame()
  } 
  
  if (exists("measure_of_center_metrics_time3")) {
    output$measure_of_center_metrics_time3 = measure_of_center_metrics_time3
  } else {
    output$measure_of_center_metrics_time3 = data.frame()
  } 

  return(output)
  
}




