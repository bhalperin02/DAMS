# This function translates the julian day of the year to angle in radians
# Updates 9.30.24 BH

# Inputs: group3_time1 = matrix containing group 3 time 1 output
#         group3_time2 = matrix containing group 3 time 2 output
#         group3_time3 = matrix containing group 3 time 3 output
# 
# Outputs: group3_time1 = matrix containing group 3 time 1 output with radians
#          group3_time1 = matrix containing group 3 time 1 output with radians
#          group3_time1 = matrix containing group 3 time 1 output with radians

IHAGroup3AddRadians <- function(
    inp1 = IHAGroup3DFOutput$full_df,
    inp2 = IHAGroup3DFOutput$time1_df,
    inp3 = IHAGroup3DFOutput$time2_df,
    inp4 = IHAGroup3DFOutput$time3_df) {
  
  # Translate julian day to radian 
  inp1$min_angle_rad <- (360 * (inp1$Min / 365)) * (pi / 180)
  inp1$max_angle_rad <- (360 * (inp1$Max / 365)) * (pi / 180)
  
  inp2$min_angle_rad <- (360 * (inp2$Min / 365)) * (pi / 180)
  inp2$max_angle_rad <- (360 * (inp2$Max / 365)) * (pi / 180)
  
  inp3$min_angle_rad <- (360 * (inp3$Min / 365)) * (pi / 180)
  inp3$max_angle_rad <- (360 * (inp3$Max / 365)) * (pi / 180)
  
  inp4$min_angle_rad <- (360 * (inp4$Min / 365)) * (pi / 180)
  inp4$max_angle_rad <- (360 * (inp4$Max / 365)) * (pi / 180)
  
  # Return
  return(list(full_df = inp1,
              time1_df = inp2,
              time2_df = inp3,
              time3_df = inp4))
  
}

