# This function is meant to take the previously created zoo objects that are
# of the full time of the gauge and also split into three and run the required
# functions for each of the 5 groups for the IHA
# Updated 2.21.2025 BH

# Inputs: full_zoo_object  = zoo object that contains dates and daily mean 
#                            flow for all dates on record for the stream gauge
#         time1_zoo_object = zoo object that contains dates and daily mean 
#                            flow for all dates in time1 on record for the 
#                            stream gauge 
#         time2_zoo_object = zoo object that contains dates and daily mean 
#                            flow for all dates in time2 on record for the 
#                            stream gauge  
#         time3_zoo_object = zoo object that contains dates and daily mean 
#                            flow for all dates in time2 on record for the 
#                            stream gauge 
#         
# Outputs: Creates matrices or df for each time period for each group of IHA 
#          indices that each contain individual indicies

IHAGroupPrep <- function (inp1 = IHAZooObjectOutput$full_zoo_object,
                          inp2 = IHAZooObjectOutput$time1_zoo_object,
                          inp3 = IHAZooObjectOutput$time2_zoo_object,
                          inp4 = IHAZooObjectOutput$time3_zoo_object) {
  
  ##Remove all na values
  inp1 <- na.omit(inp1)
  inp2 <- na.omit(inp2)
  inp3 <- na.omit(inp3)
  inp4 <- na.omit(inp4)
  
  ## IHA functions that extract IHA indicies from zoo objects
  # Full time
  if (length(inp1) > 0){
    # monthly median of daily means of daily means
    group1_median_full = group1(inp1, 'calendar', FUN = median) 
    # monthly mean of daily means
    group1_mean_full = group1(inp1, 'calendar', FUN = mean) 
    # magnitude and duration of extreme high and low water conditions
    group2_full = group2(inp1, 'calendar') 
    # Julian day of extreme water conditions
    group3_full = group3(inp1, 'calendar', mimic.tnc = FALSE)
    # frequency and duration of high and low pulses; thresholds set at 25% and 
    # 75% of that years flow
    group4_full = group4(inp1, 'calendar', thresholds = NULL) 
    # rate and frequency of water condition changes
    group5_full = group5(inp1, 'calendar') 
  }
  
  # Time 1 
  if (length(inp2) > 0) {
    group1_median_time1 = group1(inp2, 'calendar', FUN = median)
    group1_mean_time1 = group1(inp2, 'calendar', FUN = mean)
    group2_time1 = group2(inp2, 'calendar') 
    group3_time1 = group3(inp2, 'calendar', mimic.tnc = FALSE)
    group4_time1 = group4(inp2, 'calendar', thresholds = NULL) #thresholds set at 25% and 75% default
    group5_time1 = group5(inp2, 'calendar')
  }
  
  # Time 2 
  if (length(inp3) > 0){
    group1_median_time2 = group1(inp3, 'calendar', FUN = median) 
    group1_mean_time2 = group1(inp3, 'calendar', FUN = mean) 
    group2_time2 = group2(inp3, 'calendar') 
    group3_time2 = group3(inp3, 'calendar', mimic.tnc = FALSE)
    group4_time2 = group4(inp3, 'calendar', thresholds = NULL) #thresholds set at 25% and 75% default
    group5_time2 = group5(inp3, 'calendar') 
  }
  
  # Time 3 
  if (length(inp4) > 0){
    group1_median_time3 = group1(inp4, 'calendar', FUN = median) 
    group1_mean_time3 = group1(inp4, 'calendar', FUN = mean)
    group2_time3 = group2(inp4, 'calendar')
    group3_time3 = group3(inp4, 'calendar', mimic.tnc = FALSE) 
    group4_time3 = group4(inp4, 'calendar', thresholds = NULL) #thresholds set at 25% and 75% default
    group5_time3 = group5(inp4, 'calendar')
  }
  
  ## Return matrices or dfs
  output <- list()
  
  #full time
  if (exists("group1_median_full")) {
    output$group1_median_full = group1_median_full
  } else {
    output$group1_median_full = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group1_mean_full")) {
    output$group1_mean_full = group1_mean_full
  } else {
    output$group1_mean_full = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group2_full")) {
    output$group2_full = group2_full
  } else {
    output$group2_full = data.frame()
  }
  
  if (exists("group3_full")) {
    output$group3_full = group3_full
  } else {
    output$group3_full = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group4_full")) {
    output$group4_full = group4_full
  } else {
    output$group4_full = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group5_full")) {
    output$group5_full = group5_full
  } else {
    output$group5_full = matrix(nrow = 0, ncol = 0)
  }
  
  #time 1
  if (exists("group1_median_time1")) {
    output$group1_median_time1 = group1_median_time1
  } else {
    output$group1_median_time1 = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group1_mean_time1")) {
    output$group1_mean_time1 = group1_mean_time1
  } else {
    output$group1_mean_time1 = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group2_time1")) {
    output$group2_time1 = group2_time1
  } else {
    output$group2_time1 = data.frame()
  }
  
  if (exists("group3_time1")) {
    output$group3_time1 = group3_time1
  } else {
    output$group3_time1 = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group4_time1")) {
    output$group4_time1 = group4_time1
  } else {
    output$group4_time1 = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group5_time1")) {
    output$group5_time1 = group5_time1
  } else {
    output$group5_time1 = matrix(nrow = 0, ncol = 0)
  }  
  
  #time 2
  if (exists("group1_median_time2")) {
    output$group1_median_time2 = group1_median_time2
  } else {
    output$group1_median_time2 = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group1_mean_time2")) {
    output$group1_mean_time2 = group1_mean_time2
  } else {
    output$group1_mean_time2 = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group2_time2")) {
    output$group2_time2 = group2_time2
  } else {
    output$group2_time2 = data.frame()
  }
  
  if (exists("group3_time2")) {
    output$group3_time2 = group3_time2
  } else {
    output$group3_time2 = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group4_time2")) {
    output$group4_time2 = group4_time2
  } else {
    output$group4_time2 = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group5_time2")) {
    output$group5_time2 = group5_time2
  } else {
    output$group5_time2 = matrix(nrow = 0, ncol = 0)
  }  
  
  #time 3
  if (exists("group1_median_time3")) {
    output$group1_median_time3 = group1_median_time3
  } else {
    output$group1_median_time3 = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group1_mean_time3")) {
    output$group1_mean_time3 = group1_mean_time3
  } else {
    output$group1_mean_time3 = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group2_time3")) {
    output$group2_time3 = group2_time3
  } else {
    output$group2_time3 = data.frame()
  }
  
  if (exists("group3_time3")) {
    output$group3_time3 = group3_time3
  } else {
    output$group3_time3 = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group4_time3")) {
    output$group4_time3 = group4_time3
  } else {
    output$group4_time3 = matrix(nrow = 0, ncol = 0)
  }
  
  if (exists("group5_time3")) {
    output$group5_time3 = group5_time3
  } else {
    output$group5_time3 = matrix(nrow = 0, ncol = 0)
  }  
  
  
  return(output)

}