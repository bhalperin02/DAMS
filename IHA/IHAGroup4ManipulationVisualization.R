# This function manipulates and visualizes Group 4 over the full time period and
# for times 1, 2, and 3. 
# Updated 2.25.24 BH

# Inputs: group4_full  = matrix containing group 4 full output
#         group4_time1 = matrix containing group 4 time 1 output
#         group4_time2 = matrix containing group 4 time 2 output
#         group4_time3 = matrix containing group 4 time 3 output
#   
# Outputs: group4_number_full_plot  = plot with group 4 number full output
#          group4_length_full_plot  = plot with group 4 length full output
#          group4_number_time1_plot = plot with group 4 number time 1 output
#          group4_length_time1_plot = plot with group 4 length time 1 output
#          group4_number_time2_plot = plot with group 4 number time 2 output
#          group4_length_time2_plot = plot with group 4 length time 2 output
#          group4_number_time3_plot = plot with group 4 number time 3 output
#          group4_length_time4_plot = plot with group 4 length time 3 output
  
IHAGroup4ManipulationVisualization <- function(
    inp1 = IHAGroupPrepOutput$group4_full,
    inp2 = IHAGroupPrepOutput$group4_time1,
    inp3 = IHAGroupPrepOutput$group4_time2,
    inp4 = IHAGroupPrepOutput$group4_time3) {
  
  ## Full Time
  if (nrow(inp1) > 0) {
  # Manipulation
  group4_full = as.data.frame(inp1)
  group4_full$year = rownames(group4_full)
  rownames(group4_full) = NULL
  
  group4_full = subset(group4_full, inc_yr1 < year & year < inc_yr4)
  
  group4_number_full = group4_full[c('year', 'Low pulse number', 'High pulse number')]
  group4_length_full = group4_full[c('year', 'Low pulse length', 'High pulse length')]
  
  group4_number_full_long = gather(group4_number_full, key ='metric', value = 'number', - year)
  group4_length_full_long = gather(group4_length_full, key ='metric', value = 'length', - year)
  group4_number_full_long$year = as.numeric(group4_number_full_long$year)
  group4_length_full_long$year = as.numeric(group4_length_full_long$year)
  
  # Visualization
  group4_number_full_plot <- ggplot(
    data = group4_number_full_long, aes(x = year, y = number, color = metric, 
                                        group = metric)) +
    labs (title = 'GROUP 4 NUMBER OF PULSES FULL TIME ', x = 'YEAR', 
          y = 'NUMBER OF PULSES') +
    ylim(0, max(group4_number_full_long$number) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Pulse")) +
    theme_minimal()
  
  group4_length_full_plot <- ggplot(
    data = group4_length_full_long, aes(x = year, y = length, color = metric, 
                                        group = metric)) +
    labs (title = 'GROUP 4 LENGTH OF PULSES FULL TIME', x = 'YEAR', 
          y = 'MEAN LENGTH OF PULSES') +
    ylim(0, max(group4_length_full_long$length) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Pulse")) +
    theme_minimal()
  }
  
  ## Time 1
  if (nrow(inp2) > 0) {
  # Manipulation
  group4_time1 = as.data.frame(inp2)
  group4_time1$year = rownames(group4_time1)
  rownames(group4_time1) = NULL
  
  group4_time1 = subset(group4_time1, inc_yr1 < year & year < inc_yr4)
  
  group4_number_time1 = group4_time1[c('year', 'Low pulse number', 'High pulse number')]
  group4_length_time1 = group4_time1[c('year', 'Low pulse length', 'High pulse length')]
  
  group4_number_time1_long = gather(group4_number_time1, key ='metric', value = 'number', - year)
  group4_length_time1_long = gather(group4_length_time1, key ='metric', value = 'length', - year)
  group4_number_time1_long$year = as.numeric(group4_number_time1_long$year)
  group4_length_time1_long$year = as.numeric(group4_length_time1_long$year)
  
  # Visualization
  group4_number_time1_plot <- ggplot(
    data = group4_number_time1_long, aes(x = year, y = number, color = metric, 
                                         group = metric)) +
    labs (title = 'GROUP 4 NUMBER OF PULSES TIME 1', x = 'YEAR', 
          y = 'NUMBER OF PULSES') +
    ylim(0, max(group4_number_full_long$number) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Pulse")) +
    theme_minimal()
  
  group4_length_time1_plot <- ggplot(
    data = group4_length_time1_long, aes(x = year, y = length, color = metric, 
                                         group = metric)) +
    labs (title = 'GROUP 4 LENGTH OF PULSES TIME 1', x = 'YEAR', 
          y = 'MEAN LENGTH OF PULSES') +
    ylim(0, max(group4_length_full_long$length) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Pulse")) +
    theme_minimal()
  }
  
  ## Time 2
  if (nrow(inp3) > 0) {
  # Manipulation
  group4_time2 = as.data.frame(inp3)
  group4_time2$year = rownames(group4_time2)
  rownames(group4_time2) = NULL
  
  group4_time2 = subset(group4_time2, inc_yr1 < year & year < inc_yr4)
  
  group4_number_time2 = group4_time2[c('year', 'Low pulse number', 'High pulse number')]
  group4_length_time2 = group4_time2[c('year', 'Low pulse length', 'High pulse length')]
  
  group4_number_time2_long = gather(group4_number_time2, key ='metric', value = 'number', - year)
  group4_length_time2_long = gather(group4_length_time2, key ='metric', value = 'length', - year)
  group4_number_time2_long$year = as.numeric(group4_number_time2_long$year)
  group4_length_time2_long$year = as.numeric(group4_length_time2_long$year)
  
  # Visualization
  group4_number_time2_plot <- ggplot(
    data = group4_number_time2_long, aes(x = year, y = number, color = metric, 
                                         group = metric)) +
    labs (title = 'GROUP 4 NUMBER OF PULSES TIME 2', x = 'YEAR', 
          y = 'NUMBER OF PULSES') +
    ylim(0, max(group4_number_full_long$number) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Pulse")) +
    theme_minimal()
  
  group4_length_time2_plot <- ggplot(
    data = group4_length_time2_long, aes(x = year, y = length, color = metric, 
                                         group = metric)) +
    labs (title = 'GROUP 4 LENGTH OF PULSES TIME 2', x = 'YEAR', 
          y = 'MEAN LENGTH OF PULSES') +
    ylim(0, max(group4_length_full_long$length) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Pulse")) +
    theme_minimal()
  }
  
  ## Time 3
  if (nrow(inp4) > 0) {
  # Manipulation
  group4_time3 = as.data.frame(inp4)
  group4_time3$year = rownames(group4_time3)
  rownames(group4_time3) = NULL
  
  group4_time3 = subset(group4_time3, inc_yr1 < year & year < inc_yr4)
  
  group4_number_time3 = group4_time3[c('year', 'Low pulse number', 'High pulse number')]
  group4_length_time3 = group4_time3[c('year', 'Low pulse length', 'High pulse length')]
  
  group4_number_time3_long = gather(group4_number_time3, key ='metric', value = 'number', - year)
  group4_length_time3_long = gather(group4_length_time3, key ='metric', value = 'length', - year)
  group4_number_time3_long$year = as.numeric(group4_number_time3_long$year)
  group4_length_time3_long$year = as.numeric(group4_length_time3_long$year)
  
  # Visualization
  group4_number_time3_plot <- ggplot(
    data = group4_number_time3_long, aes(x = year, y = number, color = metric, 
                                         group = metric)) +
    labs (title = 'GROUP 4 NUMBER OF PULSES TIME 3', x = 'YEAR', 
          y = 'NUMBER OF PULSES') +
    ylim(0, max(group4_number_full_long$number) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Pulse")) +
    theme_minimal()
  
  group4_length_time3_plot <- ggplot(
    data = group4_length_time3_long, aes(x = year, y = length, color = metric, 
                                         group = metric)) +
    labs (title = 'GROUP 4 LENGTH OF PULSES TIME 3', x = 'YEAR', 
          y = 'MEAN LENGTH OF PULSES') +
    ylim(0, max(group4_length_full_long$length) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Pulse")) +
    theme_minimal()
  }
  
  ## Return
  output <- list()
  #full time
  if (exists("group4_number_full_plot")) {
    output$group4_number_full_plot = group4_number_full_plot
  } 
  
  if (exists("group4_length_full_plot")) {
    output$group4_length_full_plot = group4_length_full_plot
  }

  #time 1
  if (exists("group4_number_time1_plot")) {
    output$group4_number_time1_plot = group4_number_time1_plot
  } 
  
  if (exists("group4_length_time1_plot")) {
    output$group4_length_time1_plot = group4_length_time1_plot
  }

  #time 2
  if (exists("group4_number_time2_plot")) {
    output$group4_number_time2_plot = group4_number_time2_plot
  } 
  
  if (exists("group4_length_time2_plot")) {
    output$group4_length_time2_plot = group4_length_time2_plot
  }
  
  #time 3
  if (exists("group4_number_time3_plot")) {
    output$group4_number_time3_plot = group4_number_time3_plot
  } 
  
  if (exists("group4_length_time3_plot")) {
    output$group4_length_time3_plot = group4_length_time3_plot
  }
  
  print(output)
  
}
  

