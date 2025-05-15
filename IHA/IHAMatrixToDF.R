# This function is changes the outputs from a matrix to a df and adds a year 
# column
# Updated 10.27.24 BH

# Inputs: full_matrix  = matrix containing data from full time
#         time1_matrix = matrix continaing data from time 1
#         time2_matrix = matrix containing data from time 2
#         time3_matrix = matrix containing data from time 3

# Outputs: full_df  = df containing data from full time
#          time1_df = df containing data form time 1
#          time2_df = df containing data form time 2
#          time3_df = df containing data form time 3

 
IHAMatrixToDF <- function(full_matrix, time1_matrix, 
                          time2_matrix, time3_matrix) {
  
  # converts matrix to df 
  full_df <- as.data.frame(full_matrix)
  time1_df <- as.data.frame(time1_matrix)
  time2_df <- as.data.frame(time2_matrix)
  time3_df <- as.data.frame(time3_matrix)
  
  # adds year column
  full_df$year <- rownames(full_df)
  time1_df$year <- rownames(time1_df)
  time2_df$year <- rownames(time2_df)
  time3_df$year <- rownames(time3_df)
  
  # Revert rownames to 1 through n
  rownames(full_df) = NULL
  rownames(time1_df) = NULL
  rownames(time2_df) = NULL
  rownames(time3_df) = NULL
  
  # Return
  return(list(full_df = full_df,
              time1_df = time1_df,
              time2_df = time2_df,
              time3_df = time3_df))
  
}
                                  