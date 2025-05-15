# This function is meant to take the previously created zoo objects that are
# of the full time of the gauge and also split into three and run the required
# functions for each of the 5 groups for the IHA
# NOTE: THIS FUNCTION IS NO LONGER USED AND REPLACED BY IHAGroupPrep.R
# Updated 9.13.2024 BH

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
# Outputs: Creates matrices for each time period for each group of IHA indices
#          that each have individual indicies

IHAGroups <- function (full_zoo_object,
                       time1_zoo_object,
                       time2_zoo_object,
                       time3_zoo_object) {
  ## IHA functions
  #full time
  group1_median_full = group1(full_zoo_object, 'calendar', FUN = median) #median of monthly water conditions
  group1_mean_full = group1(full_zoo_object, 'calendar', FUN = mean) #mean of monthly water conditions

  group2_full = group2(full_zoo_object, 'calendar') # magnitude and duration of extreme water conditions
  
  group3_full = group3(full_zoo_object, 'calendar', mimic.tnc = FALSE) #Julian date of extreme water conditions 
  
  group4_full = group4(full_zoo_object, 'calendar', thresholds = NULL) #thresholds set at 25% and 75% default; frequency and duration of high and low pulses
  
  group5_full = group5(full_zoo_object, 'calendar') #rate and frequency of water conditions changes
  
  #time1 
  group1_median_time1 = group1(time1_zoo_object, 'calendar', FUN = median)
  group1_mean_time1 = group1(time1_zoo_object, 'calendar', FUN = mean)
  
  group2_time1 = group2(time1_zoo_object, 'calendar') 
  
  group3_time1 = group3(time1_zoo_object, 'calendar', mimic.tnc = FALSE)
  
  group4_time1 = group4(time1_zoo_object, 'calendar', thresholds = NULL) #thresholds set at 25% and 75% default
  
  group5_time1 = group5(time1_zoo_object, 'calendar')
  
  #time2 
  group1_median_time2 = group1(time2_zoo_object, 'calendar', FUN = median) 
  group1_mean_time2 = group1(time2_zoo_object, 'calendar', FUN = mean) 
  
  group2_time2 = group2(time2_zoo_object, 'calendar') 
  
  group3_time2 = group3(time2_zoo_object, 'calendar', mimic.tnc = FALSE)
  
  group4_time2 = group4(time2_zoo_object, 'calendar', thresholds = NULL) #thresholds set at 25% and 75% default
  
  group5_time2 = group5(time2_zoo_object, 'calendar') 
  
  #time3 
  group1_median_time3 = group1(time3_zoo_object, 'calendar', FUN = median) 
  group1_mean_time3 = group1(time3_zoo_object, 'calendar', FUN = mean)
  
  group2_time3 = group2(time3_zoo_object, 'calendar')
  
  group3_time3 = group3(time3_zoo_object, 'calendar', mimic.tnc = FALSE) 
  
  group4_time3 = group4(time3_zoo_object, 'calendar', thresholds = NULL) #thresholds set at 25% and 75% default
  
  group5_time3 = group5(time3_zoo_object, 'calendar')
  
  ## Return matrices
  return (group1_median_full)
  return (group1_mean_full)
  return (group2_full)
  return (group3_full)
  return (group4_full)
  return (group5_full)
  return (group1_median_time1)
  return (group1_mean_time1)
  return (group2_time1)
  return (group3_time1)
  return (group4_time1)
  return (group5_time1)
  return (group1_median_time2)
  return (group1_mean_time2)
  return (group2_time2)
  return (group3_time2)
  return (group4_time2)
  return (group5_time2)
  return (group1_median_time3)
  return (group1_mean_time3)
  return (group2_time3)
  return (group3_time3)
  return (group4_time3)
  return (group5_time3)
}
