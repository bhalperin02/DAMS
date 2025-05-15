# The function is meant to turn the dataframe with flow and date and turn it
# into zoo objects for full time, time1, time2, and time3
# Updated 1.31.25 BH

# Inputs: daily_flow  = df for all daily flow
#         time1_daily_flow  = df for daily flow for time1
#         time2_daily_flow  = df for daily flow for time2
#         time3_daily_flow  = df for daily flow for time3
#
# Outputs: 

IHAZooObject <- function(inp1 = daily_flow,
                         inp2 = time1_daily_flow,
                         inp3 = time2_daily_flow,
                         inp4 = time3_daily_flow) {
  # Full time
  if (nrow(inp1) > 0){
    # defines a variable that is only flow
    full_flow = inp1$X_00060_00003
    # defines a variable that is only date
    full_date = as.Date(inp1$Date)
    # creates a zoo object only made of flow and date
    full_zoo_object = zoo(full_flow, full_date)
  }

  # Time 1
  if (nrow(inp2) > 0) {
    time1_flow = inp2$X_00060_00003
    time1_date = as.Date(inp2$Date)
    time1_zoo_object = zoo(time1_flow, time1_date)
  }
  
  
  # Time 2
  if (nrow(inp3) > 0) {
    time2_flow = inp3$X_00060_00003
    time2_date = as.Date(inp3$Date)
    time2_zoo_object = zoo(time2_flow, time2_date)
  }
  
  # Time 3
  if (nrow(inp4) > 0) {
    time3_flow = inp4$X_00060_00003
    time3_date = as.Date(inp4$Date)
    time3_zoo_object = zoo(time3_flow, time3_date)
  }
  
  # Return
  output <- list()
  
  if (exists("full_zoo_object")) {
    output$full_zoo_object = full_zoo_object
  } else {
    output$full_zoo_object = zoo()
  } 
  
  if (exists("time1_zoo_object")) {
    output$time1_zoo_object = time1_zoo_object
  } else {
    output$time1_zoo_object = zoo()
  }
  
  if (exists("time2_zoo_object")) {
    output$time2_zoo_object = time2_zoo_object
  } else {
    output$time2_zoo_object = zoo()
  }
  
  if (exists("time3_zoo_object")) {
    output$time3_zoo_object = time3_zoo_object
  } else {
    output$time3_zoo_object = zoo()
  }
  
  return(output)
  
}
