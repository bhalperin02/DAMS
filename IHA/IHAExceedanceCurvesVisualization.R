# This function takes zoo object inputs of streamflow daily mean and outputs
# exceedance curves for full time period and times 1, 2, and 3
# Updated 2.24.25 BH

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
# Outputs: full_exceedance_curve_plot  = exceedance curve plot using daily mean 
#                                        for full time
#          time1_exceedance_curve_plot = exceedance curve plot usiing daily mean
#                                        for time1
#          time2_exceedance_curve_plot = exceedance curve plot usiing daily mean
#                                        for time2
#          time3_exceedance_curve_plot = exceedance curve plot usiing daily mean
#                                        for time3

IHAExceedanceCurvesVisualization <- function(
    inp1 = IHAZooObjectOutput$full_zoo_object,
    inp2 = IHAZooObjectOutput$time1_zoo_object,
    inp3 = IHAZooObjectOutput$time2_zoo_object,
    inp4 = IHAZooObjectOutput$time3_zoo_object) {
  
  # Exceedance curves creation
  if (length(inp1) > 0) {
    full_time_exceedance_curve = flow.duration(inp1, ylab = 'Full Time Flow',
                                               xlab = 'Exceedance Probability')
  }
  
  if (length(inp2) > 0) {
  time1_exceedance_curve = flow.duration(inp2, ylab = 'Time 1 Flow',
                                         xlab = 'Exceedance Probability')
  }
  
  if (length(inp3) > 0) {
  time2_exceedance_curve = flow.duration(inp3, ylab = 'Time 2 Flow', 
                                         xlab = 'Exceedance Probability')
  }
  
  if (length(inp4) > 0) {
  time3_exceedance_curve = flow.duration(inp4, ylab = 'Time 3 Flow', 
                                         xlab = 'Exceedance Probability')
  }
  
  # Return
  output <- list()
  
  if (exists('full_time_exceedance_curve')) {
    output$full_time_exceedance_curve = full_time_exceedance_curve
  } 
  
  if (exists('time1_exceedance_curve')) {
    output$time1_exceedance_curve = time1_exceedance_curve
  } 
  
  if (exists('time2_exceedance_curve')) {
    output$time2_exceedance_curve = time2_exceedance_curve
  } 
  
  if (exists('time3_exceedance_curve')) {
    output$time3_exceedance_curve = time3_exceedance_curve
  } 

  return(output)
  
}