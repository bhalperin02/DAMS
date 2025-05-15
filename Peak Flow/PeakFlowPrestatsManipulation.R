# This function turns the date into year for peak flow metrics output for the 
# full time period as well as individual time periods
# Updated 2.21.25 BH 
# 
# Inputs: inp1  = df for peak flow metrics time 1
#         inp2  = df for peak flow metrics time 2
#         inp3  = df for peak flow metrics time 3
# 
# Outputs: Outputs peak flow data frames that contain years instead of numbers,
# allowing for stats to be done as numbers can be turned numeric

PeakFlowPrestatsManipulation <- function(inp1 = 
                                          PeakFlowMetricsOutput$peak_RI_time1,
                                        inp2 = 
                                          PeakFlowMetricsOutput$peak_RI_time2, 
                                        inp3 = 
                                          PeakFlowMetricsOutput$peak_RI_time3) {
  ## Add year from peak_dt
  #time 1
  if(nrow(inp1) > 0) {
    inp1$year <- 
      as.numeric(format(inp1$peak_dt,'%Y'))
  }
  
  #time 2
  if(nrow(inp2) > 0) {
    inp2$year <- 
      as.numeric(format(inp2$peak_dt,'%Y'))
  }
  
  #time 3
  if(nrow(inp3) > 0) {
    inp3$year <- 
      as.numeric(format(inp3$peak_dt,'%Y'))
  } 
  
  ## Select necessary columns from each df
  #time 1
  if(nrow(inp1) > 0) {
    inp1 <- inp1[c('year', 'peak_va', 'rank', 'RI', 'APE')]
  }
  
  #time 2
  if(nrow(inp2) > 0) {
    inp2 <- inp2[c('year', 'peak_va', 'rank', 'RI', 'APE')]
  }
  
  #time 3
  if(nrow(inp3) > 0) {
    inp3 <- inp3[c('year', 'peak_va', 'rank', 'RI', 'APE')]
  }
  
  ## Return
  output <- list()
  
  if (exists("inp1")) {
    output$peak_flow_prestats_manipulation_time1 = inp1
  } else {
    output$peak_flow_prestats_manipulation_time1 = data.frame()
  } 
  
  if (exists("inp2")) {
    output$peak_flow_prestats_manipulation_time2 = inp2
  } else {
    output$peak_flow_prestats_manipulation_time2 = data.frame()
  } 
  
  if (exists("inp3")) {
    output$peak_flow_prestats_manipulation_time3 = inp3
  } else {
    output$peak_flow_prestats_manipulation_time3 = data.frame()
  } 
  
  return(output)
  
}