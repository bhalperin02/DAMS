# This function visualizes Group 1 mean and median over the full time period and
# for times 1, 2, and 3. No manipulation has to occur.
# Updated 2.21.25 BH

# Inputs: group1_mean_full    = matrix containing group 1 mean full output 
#         group1_median_full  = matrix containing group 1 median full output
#         group1_mean_time1   = matrix containing group 1 mean time 1 output
#         group1_median_time1 = matrix containing group 1 median time 1 output
#         group1_mean_time2   = matrix containing group 1 mean time 2 output
#         group1_median_time2 = matrix containing group 1 median time 2 output
#         group1_mean_time3   = matrix containing group 1 mean time 3 output
#         group1_median_time3 = matrix containing group 1 median time 3 output
# 
# Outputs: group1_mean_full_plot    = plot with group 1 mean full output 
#          group1_median_full_plot  = plot with group 1 median full output
#          group1_mean_time1_plot   = plot with group 1 mean time 1 output
#          group1_median_time1_plot = plot with group 1 median time 1 output
#          group1_mean_time2_plot   = plot with group 1 mean time 2 output
#          group1_median_time2_plot = plot with group 1 median time 2 output
#          group1_mean_time3_plot   = plot with group 1 mean time 3 output
#          group1_median_time3_plot = plot with group 1 median time 3 output

IHAGroup1ManipulationVisualization <- function(
    inp1 = IHAGroupPrepOutput$group1_mean_full,
    inp2 = IHAGroupPrepOutput$group1_median_full,
    inp3 = IHAGroupPrepOutput$group1_mean_time1,
    inp4 = IHAGroupPrepOutput$group1_median_time1,
    inp5 = IHAGroupPrepOutput$group1_mean_time2,
    inp6 = IHAGroupPrepOutput$group1_median_time2,
    inp7 = IHAGroupPrepOutput$group1_mean_time3,
    inp8 = IHAGroupPrepOutput$group1_median_time3) {
  
  # full time
  if (nrow(inp1) > 0) {
    group1_mean_full_plot <- plot(inp1, 
                                  border = NA,
                                  breaks  = 30,
                                  col = heat.colors(30),
                                  xlab = 'MONTH',
                                  ylab = 'YEAR',
                                  main = 'GROUP 1 MEAN FULL TIME')
  }
  
  if (nrow(inp2) > 0) {
    group1_median_full_plot <- plot(inp2, 
                                    border = NA,
                                    breaks  = 30,
                                    col = heat.colors(30),
                                    xlab = 'MONTH',
                                    ylab = 'YEAR',
                                    main = 'GROUP 1 MEDIAN FULL TIME') 
  }
  
  # time 1
  if (nrow(inp3) > 0) {
    group1_mean_time1_plot <- plot(inp3, 
                                   border = NA,
                                   breaks  = 30,
                                   col = heat.colors(30),
                                   xlab = 'MONTH',
                                   ylab = 'YEAR',
                                   main = 'GROUP 1 MEAN TIME 1')
  }
  
  if (nrow(inp4) > 0) {
    group1_median_time1_plot <- plot(inp4, 
                                     border = NA,
                                     breaks  = 30,
                                     col = heat.colors(30),
                                     xlab = 'MONTH',
                                     ylab = 'YEAR',
                                     main = 'GROUP 1 MEDIAN TIME 1')
  }
  
  # time 2
  if (nrow(inp5) > 0) {
    group1_mean_time2_plot <- plot(inp4, 
                                   border = NA,
                                   breaks  = 30,
                                   col = heat.colors(30),
                                   xlab = 'MONTH',
                                   ylab = 'YEAR',
                                   main = 'GROUP 1 MEAN TIME 2')
  }
  
  if (nrow(inp6) > 0) {
    group1_median_time2_plot <- plot(inp6, 
                                     border = NA,
                                     breaks  = 30,
                                     col = heat.colors(30),
                                     xlab = 'MONTH',
                                     ylab = 'YEAR',
                                     main = 'GROUP 1 MEDIAN TIME 2')
  }
    
  # time 3
  if (nrow(inp7) > 0) {
    group1_mean_time3_plot <- plot(inp7, 
                                   border = NA,
                                   breaks  = 30,
                                   col = heat.colors(30),
                                   xlab = 'MONTH',
                                   ylab = 'YEAR',
                                   main = 'GROUP 1 MEAN TIME 3')
  }
    
  if (nrow(inp8) > 0) {
    group1_median_time3_plot <- plot(inp8, 
                                     border = NA,
                                     breaks  = 30,
                                     col = heat.colors(30),
                                     xlab = 'MONTH',
                                     ylab = 'YEAR',
                                     main = 'GROUP 1 MEDIAN TIME 3')
  }
  
  ## return
  output <- list()
  #full time
  if (exists("group1_mean_full_plot")) {
    output$group1_mean_full_plot = group1_mean_full_plot
  } 
  
  if (exists("group1_median_full_plot")) {
    output$group1_median_full_plot = group1_median_full_plot
  }
  
  #time 1
  if (exists("group1_mean_time1_plot")) {
    output$group1_mean_time1_plot = group1_mean_time1_plot
  } 
  
  if (exists("group1_median_time1_plot")) {
    output$group1_median_time1_plot = group1_median_time1_plot
  } 
  
  #time 2
  if (exists("group1_mean_time2_plot")) {
    output$group1_mean_time2_plot = group1_mean_time2_plot
  } 
  
  if (exists("group1_median_time2_plot")) {
    output$group1_median_time2_plot = group1_median_time2_plot
  } 
  
  #time 3
  if (exists("group1_mean_time3_plot")) {
    output$group1_mean_time3_plot = group1_mean_time3_plot
  } 
  
  if (exists("group1_median_time3_plot")) {
    output$group1_median_time3_plot = group1_median_time3_plot
  } 
  
  return(output)
    
}
    
  



