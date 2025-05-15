# This function characterizes the top 10% daily flows per year (36 per year) for 
# the full time and all three individual times. if() {} 
# statements means the function only runs if flow data is available
# Updated 2.13.25 BH with code also from JJ
# 
# Inputs: daily_flow  = df for all daily flow
#         time1_daily_flow  = df for daily flow for time1
#         time2_daily_flow  = df for daily flow for time2
#         time3_daily_flow  = df for daily flow for time3
# 
# Outputs: Outputs top 10% daily flows per year for full time and each time 
# period. If no data frame prints in OutputComplete for a time period, that 
# means there was not data collection during that time period.

HighFlowMetrics <- function(inp1 = daily_flow,
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
  
  ## Create 10% high flow cutoff threshold 
  #full time
  if(nrow(inp1) > 0) {
    full_threshold <- data.frame(
      year = unique(daily_flow$year), threshold = tapply(
        daily_flow$X_00060_00003, daily_flow$year, 
        function(x) quantile ( x, 0.9, na.rm = TRUE)))
  }
  
  #time 1
  if(nrow(inp2) > 0) {
    time1_threshold <- data.frame(
      year = unique(time1_daily_flow$year), threshold = tapply(
        time1_daily_flow$X_00060_00003, time1_daily_flow$year,
        function(x) quantile (x, 0.9, na.rm = TRUE)))
  }  
  
  #time 2
  if(nrow(inp3) > 0) {
    time2_threshold <- data.frame(
      year = unique(time2_daily_flow$year), threshold = tapply(
        time2_daily_flow$X_00060_00003, time2_daily_flow$year,
        function(x) quantile (x, 0.9, na.rm = TRUE)))
     
  }
    
  #time 3
  if(nrow(inp4) > 0) {
    time3_threshold <- data.frame(
      year = unique(time3_daily_flow$year), threshold = tapply(
        time3_daily_flow$X_00060_00003, time3_daily_flow$year, 
        function(x) quantile (x, 0.9, na.rm = TRUE)))
  }
  
    
  ## Join to add annual threshold to each daily data df
  #full time
  if(nrow(inp1) > 0) {
      daily_flow <- left_join(daily_flow, full_threshold, by = 'year')
    }   
    
  #time 1
  if(nrow(inp2) > 0) {
    time1_daily_flow <- left_join(time1_daily_flow, time1_threshold,
                                  by = 'year')
  }
    
  #time 2
  if(nrow(inp3) > 0) {
    time2_daily_flow <- left_join(time2_daily_flow, time2_threshold, 
                                  by = 'year')
  }
  
  #time 3
  if(nrow(inp4) > 0) {
    time3_daily_flow <- left_join(time3_daily_flow, time3_threshold,
                                  by = 'year')
  }
  
  
  ## Use threshold in daily flow data to isolate top 10% annual flows
  #full time
  if(nrow(inp1) > 0) {
    high_flow_metrics_full <- subset(daily_flow, X_00060_00003 >=
                                       daily_flow$threshold)
  }
  
  #time 1
  if(nrow(inp2) > 0) {
    high_flow_metrics_time1 <- subset(time1_daily_flow, X_00060_00003 >=
                                        time1_daily_flow$threshold)
  }
  
  #time 2
  if(nrow(inp3) > 0) {
    high_flow_metrics_time2 <- subset(time2_daily_flow, X_00060_00003 >=
                                        time2_daily_flow$threshold)
  }
  
  #time 3
  if(nrow(inp4) > 0) {
    high_flow_metrics_time3 <- subset(time3_daily_flow, X_00060_00003 >=
                                        time3_daily_flow$threshold)
  }
  
  
  ## Select necessary columns from each df
  #full time
  if(nrow(inp1) > 0) {
    high_flow_metrics_full <- high_flow_metrics_full[c(
      'X_00060_00003','year', 'threshold')]
  }
  
  #time 1
  if(nrow(inp2) > 0) {
    high_flow_metrics_time1 <- high_flow_metrics_time1[c(
      'X_00060_00003','year', 'threshold')]
  }
  
  #time 2
  if(nrow(inp3) > 0) {
    high_flow_metrics_time2 <- high_flow_metrics_time2[c(
      'X_00060_00003','year', 'threshold')]
  }
  
  #time 3
  if(nrow(inp4) > 0) {
    high_flow_metrics_time3 <- high_flow_metrics_time3[c(
      'X_00060_00003','year', 'threshold')]
  }
  
  ## Rename columns for each df
  #full time
  if(nrow(inp1) > 0) {
    colnames(high_flow_metrics_full) <- c('flow', 'year', 'threshold')
  }
  
  #time 1
  if(nrow(inp2) > 0) {
    colnames(high_flow_metrics_time1) <- c('flow', 'year', 'threshold')
  }
  
  #time 2
  if(nrow(inp3) > 0) {
    colnames(high_flow_metrics_time2) <- c('flow', 'year', 'threshold')
  }
  
  #time 3
  if(nrow(inp4) > 0) {
    colnames(high_flow_metrics_time3) <- c('flow', 'year', 'threshold')
  }
  
  ## return
  output <- list()
  
  if (exists("high_flow_metrics_full")) {
    output$high_flow_metrics_full = high_flow_metrics_full
  } else {
    output$high_flow_metrics_full = data.frame()
  } 
  
  if (exists("high_flow_metrics_time1")) {
    output$high_flow_metrics_time1 = high_flow_metrics_time1
  } else {
    output$high_flow_metrics_time1 = data.frame()
  }
  
  if (exists("high_flow_metrics_time2")) {
    output$high_flow_metrics_time2 = high_flow_metrics_time2
  } else {
    output$high_flow_metrics_time2 = data.frame()
  }
  
  if (exists("high_flow_metrics_time3")) {
    output$high_flow_metrics_time3 = high_flow_metrics_time3
  } else {
    output$high_flow_metrics_time3 = data.frame()
  }
  
  return(output)
  
}
  
  
