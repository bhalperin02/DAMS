# This function calculates the annual peak flow recurrence interval for the 
# full time period as well as individual time periods
# Updated 2.13.25 BH with code also from JJ
# 
# Inputs: peak_flow  = df for all peak flow
#         time1_peak_flow  = df for peak flow for time1
#         time2_peak_flow  = df for peak flow for time2
#         time3_peak_flow  = df for peak flow for time3
# 
# Outputs: Outputs annual peak flow recurrence interval for full time and each  
# time period. If no data frame prints in OutputComplete for a time period, that 
# means there was not data collection during that time period.

PeakFlowMetrics <- function(inp1 = peak_flow,
                            inp2 = time1_peak_flow,
                            inp3 = time2_peak_flow,
                            inp4 = time3_peak_flow) {
  
  ## Arrange peak flow data from largest to smallest
  #full time
  if(nrow(inp1) > 0) {
    peak_RI_full <- peak_flow[order(peak_flow$peak_va, decreasing  = TRUE),]
  }
  
  #time 1
  if(nrow(inp2) > 0) {
    peak_RI_time1 <- time1_peak_flow[order(time1_peak_flow$peak_va,
                                           decreasing  = TRUE),]
  }
  
  #time 2
  if(nrow(inp3) > 0) {
    peak_RI_time2 <- time2_peak_flow[order(time2_peak_flow$peak_va, 
                                           decreasing  = TRUE),]
  }
  
  #time 3
  if(nrow(inp4) > 0) {
    peak_RI_time3 <- time3_peak_flow[order(time3_peak_flow$peak_va,
                                           decreasing  = TRUE),]
  }
  
  
  ## Rank each function
  #full time
  if(nrow(inp1) > 0) {
    peak_RI_full$rank <- c(1:length(peak_RI_full$peak_va))
  }

  #time 1
  if(nrow(inp2) > 0) {
    peak_RI_time1$rank <- c(1:length(peak_RI_time1$peak_va))
  }  
  
  #time 2
  if(nrow(inp3) > 0) {
    peak_RI_time2$rank <- c(1:length(peak_RI_time2$peak_va))
  }  
  
  #time 3
  if(nrow(inp4) > 0) {
    peak_RI_time3$rank <- c(1:length(peak_RI_time3$peak_va))
  }  
  
  
  ## Recurrence Interval (n+1)/rank
  #full time
  if(nrow(inp1) > 0) {
    peak_RI_full$RI <- (nrow(peak_RI_full)+1)/(peak_RI_full$rank)
  }
  
  #time 1
  if(nrow(inp2) > 0) {
    peak_RI_time1$RI <- (nrow(peak_RI_time1)+1)/(peak_RI_time1$rank)
  }
  
  #time 2
  if(nrow(inp3) > 0) {
    peak_RI_time2$RI <- (nrow(peak_RI_time2)+1)/(peak_RI_time2$rank)
  }
  
  #time 3
  if(nrow(inp4) > 0) {
    peak_RI_time3$RI <- (nrow(peak_RI_time3)+1)/(peak_RI_time3$rank)
  }
  
  
  ##Annual Probability of Exceedance 
  #full time
  if(nrow(inp1) > 0) {
    peak_RI_full$APE <- 1/(peak_RI_full$RI)
  }
  
  #time 1
  if(nrow(inp2) > 0) {
    peak_RI_time1$APE <- 1/(peak_RI_time1$RI)
  }
  
  #time 2
  if(nrow(inp3) > 0) {
    peak_RI_time2$APE <- 1/(peak_RI_time2$RI)
  }
  
  #time 3
  if(nrow(inp4) > 0) {
    peak_RI_time3$APE <- 1/(peak_RI_time3$RI)
  }
  
  ## Select necessary columns from each df
  #full time
  if(nrow(inp1) > 0) {
    peak_RI_full <- peak_RI_full[c('peak_dt', 'peak_va',
                                   'rank', 'RI', 'APE')]
  }
  
  #time 1
  if(nrow(inp2) > 0) {
    peak_RI_time1 <- peak_RI_time1[c('peak_dt', 'peak_va',
                                   'rank', 'RI', 'APE')]
  }
  
  #time 2
  if(nrow(inp3) > 0) {
    peak_RI_time2 <- peak_RI_time2[c('peak_dt', 'peak_va',
                                   'rank', 'RI', 'APE')]
  }
  
  #time 3
  if(nrow(inp4) > 0) {
    peak_RI_time3 <- peak_RI_time3[c('peak_dt', 'peak_va',
                                   'rank', 'RI', 'APE')]
  }
  
  ## Return
  output <- list()
  
  if (exists("peak_RI_full")) {
    output$peak_RI_full = peak_RI_full
  } else {
    output$peak_RI_full = data.frame()
  } 
  
  if (exists("peak_RI_time1")) {
    output$peak_RI_time1 = peak_RI_time1
  } else {
    output$peak_RI_time1 = data.frame()
  } 
  
  if (exists("peak_RI_time2")) {
    output$peak_RI_time2 = peak_RI_time2
  } else {
    output$peak_RI_time2 = data.frame()
  } 
  
  if (exists("peak_RI_time3")) {
    output$peak_RI_time3 = peak_RI_time3
  } else {
    output$peak_RI_time3 = data.frame()
  } 
  
  return(output)
  
}


