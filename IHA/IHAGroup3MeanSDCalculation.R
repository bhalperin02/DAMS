# This computes the vector mean day and SD for IHA Group 3
# Updates 3.7.25 BH

# Inputs:  DF that holds stats output for group3 and of the time dfs
# 
# Outputs: Function that computes vector mean day and SD for group 3

IHAGroup3MeanSDCalculation <- function(statsDF, inp1, inp2, inp3, inp4) {

  ## Full Time
if(nrow(inp1) > 0) {
  #remove min and max column, leaving year and min and max angels
  inp1_mean <- inp1[, -c(1,2,3)]
  
  #convert all columns in inp1 to numeric
  inp1_mean[] <- lapply(inp1_mean, as.numeric)
  
  #calculates mean day
  sum_sin <- apply(inp1_mean, 2, function(x) sum(sin(x), na.rm = TRUE))
  sum_cos <- apply(inp1_mean, 2, function(x) sum(cos(x), na.rm = TRUE))
  
  ratio <- sum_sin / sum_cos
  atan_value <- atan(ratio)
  
  sum_sin <- as.list(sum_sin)
  sum_cos <- as.list(sum_cos)
  ratio <- as.list(ratio)
  atan_value <- as.list(atan_value)
  
  for (i in seq_along(sum_cos)) {
    if (sum_cos[[i]] < 0) {
      atan_value[[i]] <- atan_value[[i]] + pi
    }
  }
  
  for (i in seq_along(atan_value)) {
    if (atan_value[[i]] < 0) {
      atan_value[[i]] <- atan_value[[i]] + (2 * pi)
    }
  }
    
  mean_day <- lapply(atan_value, function(x) (x * 365 * 180) / (360 * pi))
  
  statsDF$full_mean_day <- mean_day
  
  statsDF$full_mean_day <- as.numeric(statsDF$full_mean_day)
  
  # calculate standard deviation
  mean_day_matrix <- matrix(rep(mean_day, each = nrow(inp1)),
                            nrow = nrow(inp1), byrow = TRUE)
  
  inp1_sd <- inp1[, -c(3,4,5)]
  
  difference_a <- abs(365 - (inp1_sd - mean_day))
  difference_b <- abs(inp1_sd - mean_day)
  
  difference <- pmin(difference_a, difference_b)

  julian_day_sd <- apply(difference, 2, function(x) {
    sqrt(sum(x^2) / (length(x) - 1))
  })
  
  statsDF$full_julian_day_sd <- julian_day_sd
}
  
  ## Time 1
if(nrow(inp2) > 0) {
    
  #remove min and max column, leaving year and min and max angels
  inp2_mean <- inp2[, -c(1,2,3)]
  
  #convert all columns in inp1 to numeric
  inp2_mean[] <- lapply(inp2_mean, as.numeric)
  
  #calculates mean day
  sum_sin <- apply(inp2_mean, 2, function(x) sum(sin(x), na.rm = TRUE))
  sum_cos <- apply(inp2_mean, 2, function(x) sum(cos(x), na.rm = TRUE))
  
  ratio <- sum_sin / sum_cos
  atan_value <- atan(ratio)
  
  sum_sin <- as.list(sum_sin)
  sum_cos <- as.list(sum_cos)
  ratio <- as.list(ratio)
  atan_value <- as.list(atan_value)
  
  for (i in seq_along(sum_cos)) {
    if (sum_cos[[i]] < 0) {
      atan_value[[i]] <- atan_value[[i]] + pi
    }
  }
  
  for (i in seq_along(atan_value)) {
    if (atan_value[[i]] < 0) {
      atan_value[[i]] <- atan_value[[i]] + (2 * pi)
    }
  }
  
  mean_day <- lapply(atan_value, function(x) (x * 365 * 180) / (360 * pi))
  
  statsDF$t1_mean_day <- mean_day
  
  statsDF$t1_mean_day <- as.numeric(statsDF$t1_mean_day)
  
  # calculate standard deviation
  mean_day_matrix <- matrix(rep(mean_day, each = nrow(inp2)),
                            nrow = nrow(inp2), byrow = TRUE)
  
  inp2_sd <- inp2[, -c(3,4,5)]
  
  difference_a <- abs(365 - (inp2_sd - mean_day))
  difference_b <- abs(inp2_sd - mean_day)
  
  difference <- pmin(difference_a, difference_b)
  
  julian_day_sd <- apply(difference, 2, function(x) {
    sqrt(sum(x^2) / (length(x) - 1))
  })
  
  statsDF$t1_julian_day_sd <- julian_day_sd

}
  
  ## Time 2
if(nrow(inp3) > 0) {
    
  #remove min and max column, leaving year and min and max angels
  inp3_mean <- inp3[, -c(1,2,3)]
  
  #convert all columns in inp1 to numeric
  inp3_mean[] <- lapply(inp3_mean, as.numeric)
  
  #calculates mean day
  sum_sin <- apply(inp2_mean, 2, function(x) sum(sin(x), na.rm = TRUE))
  sum_cos <- apply(inp2_mean, 2, function(x) sum(cos(x), na.rm = TRUE))
  
  ratio <- sum_sin / sum_cos
  atan_value <- atan(ratio)
  
  sum_sin <- as.list(sum_sin)
  sum_cos <- as.list(sum_cos)
  ratio <- as.list(ratio)
  atan_value <- as.list(atan_value)
  
  for (i in seq_along(sum_cos)) {
    if (sum_cos[[i]] < 0) {
      atan_value[[i]] <- atan_value[[i]] + pi
    }
  }
  
  for (i in seq_along(atan_value)) {
    if (atan_value[[i]] < 0) {
      atan_value[[i]] <- atan_value[[i]] + (2 * pi)
    }
  }
  
  mean_day <- lapply(atan_value, function(x) (x * 365 * 180) / (360 * pi))
  
  statsDF$t2_mean_day <- mean_day
  
  statsDF$t2_mean_day <- as.numeric(statsDF$t2_mean_day)
  
  # calculate standard deviation
  mean_day_matrix <- matrix(rep(mean_day, each = nrow(inp3)),
                            nrow = nrow(inp3), byrow = TRUE)
  
  inp3_sd <- inp3[, -c(3,4,5)]
  
  difference_a <- abs(365 - (inp3_sd - mean_day))
  difference_b <- abs(inp3_sd - mean_day)
  
  difference <- pmin(difference_a, difference_b)
  
  julian_day_sd <- apply(difference, 2, function(x) {
    sqrt(sum(x^2) / (length(x) - 1))
  })
  
  statsDF$t2_julian_day_sd <- julian_day_sd

}
  
  ## Time 3
  if(nrow(inp4) > 0) {
    
  #remove min and max column, leaving year and min and max angels
  inp4_mean <- inp4[, -c(1,2,3)]
  
  #convert all columns in inp1 to numeric
  inp4_mean[] <- lapply(inp4_mean, as.numeric)
  
  #calculates mean day
  sum_sin <- apply(inp4_mean, 2, function(x) sum(sin(x), na.rm = TRUE))
  sum_cos <- apply(inp2_mean, 2, function(x) sum(cos(x), na.rm = TRUE))
  
  ratio <- sum_sin / sum_cos
  atan_value <- atan(ratio)
  
  sum_sin <- as.list(sum_sin)
  sum_cos <- as.list(sum_cos)
  ratio <- as.list(ratio)
  atan_value <- as.list(atan_value)
  
  for (i in seq_along(sum_cos)) {
    if (sum_cos[[i]] < 0) {
      atan_value[[i]] <- atan_value[[i]] + pi
    }
  }
  
  for (i in seq_along(atan_value)) {
    if (atan_value[[i]] < 0) {
      atan_value[[i]] <- atan_value[[i]] + (2 * pi)
    }
  }
  
  mean_day <- lapply(atan_value, function(x) (x * 365 * 180) / (360 * pi))
  
  statsDF$t3_mean_day <- mean_day
  
  statsDF$t3_mean_day <- as.numeric(statsDF$t3_mean_day)
  
  # calculate standard deviation
  mean_day_matrix <- matrix(rep(mean_day, each = nrow(inp4)),
                            nrow = nrow(inp4), byrow = TRUE)
  
  inp3_sd <- inp3[, -c(3,4,5)]
  
  difference_a <- abs(365 - (inp3_sd - mean_day))
  difference_b <- abs(inp3_sd - mean_day)
  
  difference <- pmin(difference_a, difference_b)
  
  julian_day_sd <- apply(difference, 2, function(x) {
    sqrt(sum(x^2) / (length(x) - 1))
  })
  
  statsDF$t3_julian_day_sd <- julian_day_sd


}
  
  
  ## Return
  return(statsDF)
}
