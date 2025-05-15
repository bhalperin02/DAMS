# This function manipulates and visualizes Group 3 over the time2 time period
# and for time periods 1, 2, and 3. Visual outputs are both scatter plots and 
# rose diagrams.
# Updated 2.25.24 BH

# Inputs: group3_full  = matrix containing group 3 full output
#         group3_time1 = matrix containing group 3 time 1 output
#         group3_time2 = matrix containing group 3 time 2 output
#         group3_time3 = matrix containing group 3 time 3 output
#
# Outputs: group3_min_full_week_plot    = rose diagram with group 3 min full 
#                                         frequency
#          group3_max_full_week_plot    = rose diagram with group 3 max full 
#                                         frequency
#          group3_min_time1_week_plot   = rose diagram with group 3 min time 1 
#                                         frequency
#          group3_max_time1_week_plot   = rose diagram with group 3 max time 1 
#                                         frequency
#          group3_min_time2_week_plot   = rose diagram with group 3 min time 2 
#                                         frequency
#          group3_max_time2_week_plot   = rose diagram with group 3 max time 2 
#                                         frequency
#          group3_min_time3_week_plot   = rose diagram with group 3 min time 3 
#                                         frequency
#          group3_max_time3_week_plot   = rose diagram with group 3 max time 3 
#                                         frequency
#          group3_full_julian_day_plot  = plot with group 3 full julian day
#          group3_time1_julian_day_plot = plot with group 3 time 1 julian day
#          group3_time2_julian_day_plot = plot with group 3 time 2 julian day
#          group3_time3_julian_day_plot = plot with group 3 time 3 julian day


IHAGroup3ManipulationVisualization <- function(
    inp1 = IHAGroupPrepOutput$group3_full,
    inp2 = IHAGroupPrepOutput$group3_time1,
    inp3 = IHAGroupPrepOutput$group3_time2,
    inp4 = IHAGroupPrepOutput$group3_time3) {
  
  ### Rose Diagram
  ## Full time
  if (nrow(inp1) > 0) {
  # Manipulation
  group3_full <- as.data.frame(inp1)
  group3_full$year <- rownames(group3_full)
  rownames(group3_full) <- NULL
  
  group3_full <- subset(group3_full, inc_yr1 < year & year < inc_yr4)
  
  group3_min_full <- group3_full[c('year', 'Min')]
  group3_max_full <- group3_full[c('year', 'Max')]
  
  group3_min_full$week <- ceiling(group3_min_full$Min / 7) # ceiling() is to  
                                                           # round to week
  group3_min_full$date <- time_convert(input = group3_min_full$Min, 
                                       output = 'yyyy-mm-dd')
  group3_min_full <- subset(group3_min_full, select = -c(date))
  
  group3_max_full$week <- ceiling(group3_max_full$Max / 7)
  group3_max_full$date <- time_convert(input = group3_max_full$Max, 
                                       output = 'yyyy-mm-dd')
  group3_max_full <- subset(group3_max_full, select = -c(date))
  
  group3_min_full_week_frequency <- as.data.frame(plyr::count(group3_min_full, 
                                                              vars = 'week'))

  group3_max_full_week_frequency <- as.data.frame(plyr::count(group3_max_full, 
                                                              vars = 'week'))
  # Visualization
  group3_min_full_week_plot <- ggplot(data = group3_min_full_week_frequency, 
                                 aes(x = week, y = freq)) +
    geom_bar(stat = 'identity') +
    scale_x_continuous(limits = c(0.5, 52.5), breaks = 1:52) +
    coord_polar() + 
    labs (title = 'GROUP 3 MIN FULL TIME WEEK FREQUENCY', x = 'WEEK', 
          y = 'FREQUENCY')
  
  group3_max_full_week_plot <- ggplot(data = group3_max_full_week_frequency, 
                                 aes(x = week, y = freq)) +
    geom_bar(stat = 'identity') +
    scale_x_continuous(limits = c(0.5, 52.5), breaks = 1:52) +
    coord_polar() + 
    labs (title = 'GROUP 3 MAX FULL TIME WEEK FREQUENCY', x = 'WEEK', 
          y = 'FREQUENCY')
  }
  
  ## Time 1
  if (nrow(inp2) > 0) {
  # Manipulation
  group3_time1 <- as.data.frame(inp2)
  group3_time1$year <- rownames(group3_time1)
  rownames(group3_time1) <- NULL
  
  group3_time1 <- subset(group3_time1, inc_yr1 < year & year < inc_yr2)
  
  group3_min_time1 <- group3_time1[c('year', 'Min')]
  group3_max_time1 <- group3_time1[c('year', 'Max')]
  
  group3_min_time1$week <- ceiling(group3_min_time1$Min / 7) # ceiling() is to  
  # round to week
  group3_min_time1$date <- time_convert(input = group3_min_time1$Min, 
                                        output = 'yyyy-mm-dd')
  group3_min_time1 <- subset(group3_min_time1, select = -c(date))
  
  group3_max_time1$week <- ceiling(group3_max_time1$Max / 7)
  group3_max_time1$date <- time_convert(input = group3_max_time1$Max, 
                                        output = 'yyyy-mm-dd')
  group3_max_time1 <- subset(group3_max_time1, select = -c(date))
  
  group3_min_time1_week_frequency <- as.data.frame(plyr::count(group3_min_time1, 
                                                               vars = 'week'))
  
  group3_max_time1_week_frequency <- as.data.frame(plyr::count(group3_max_time1, 
                                                               vars = 'week'))
  # Visualization
  group3_min_time1_week_plot <- ggplot(data = group3_min_time1_week_frequency, 
                                  aes(x = week, y = freq)) +
    geom_bar(stat = 'identity') +
    scale_x_continuous(limits = c(0.5, 52.5), breaks = 1:52) +
    coord_polar() + 
    labs (title = 'GROUP 3 MIN TIME 1 WEEK FREQUENCY', x = 'WEEK', 
          y = 'FREQUENCY')
  
  group3_max_time1_week_plot <- ggplot(data = group3_max_time1_week_frequency, 
                                  aes(x = week, y = freq)) +
    geom_bar(stat = 'identity') +
    scale_x_continuous(limits = c(0.5, 52.5), breaks = 1:52) +
    coord_polar() + 
    labs (title = 'GROUP 3 MAX TIME 1 WEEK FREQUENCY', x = 'WEEK', 
          y = 'FREQUENCY')
  }
  
  ## Time 2
  if (nrow(inp3) > 0) {
  # Manipulation
  group3_time2 <- as.data.frame(inp3)
  group3_time2$year <- rownames(group3_time2)
  rownames(group3_time2) <- NULL
  
  group3_time2 <- subset(group3_time2, inc_yr2 < year & year < inc_yr3)
  
  group3_min_time2 <- group3_time2[c('year', 'Min')]
  group3_max_time2 <- group3_time2[c('year', 'Max')]
  
  group3_min_time2$week <- ceiling(group3_min_time2$Min / 7) # ceiling() is to  
  # round to week
  group3_min_time2$date <- time_convert(input = group3_min_time2$Min, 
                                        output = 'yyyy-mm-dd')
  group3_min_time2 <- subset(group3_min_time2, select = -c(date))
  
  group3_max_time2$week <- ceiling(group3_max_time2$Max / 7)
  group3_max_time2$date <- time_convert(input = group3_max_time2$Max, 
                                        output = 'yyyy-mm-dd')
  group3_max_time2 <- subset(group3_max_time2, select = -c(date))
  
  group3_min_time2_week_frequency <- as.data.frame(plyr::count(group3_min_time2, 
                                                               vars = 'week'))
  
  group3_max_time2_week_frequency <- as.data.frame(plyr::count(group3_max_time2, 
                                                               vars = 'week'))
  # Visualization
  group3_min_time2_week_plot <- ggplot(data = group3_min_time2_week_frequency, 
                                  aes(x = week, y = freq)) +
    geom_bar(stat = 'identity') +
    scale_x_continuous(limits = c(0.5, 52.5), breaks = 1:52) +
    coord_polar() + 
    labs (title = 'GROUP 3 MIN TIME 2 WEEK FREQUENCY', x = 'WEEK', 
          y = 'FREQUENCY')
  
  group3_max_time2_week_plot <- ggplot(data = group3_max_time2_week_frequency, 
                                  aes(x = week, y = freq)) +
    geom_bar(stat = 'identity') +
    scale_x_continuous(limits = c(0.5, 52.5), breaks = 1:52) +
    coord_polar() + 
    labs (title = 'GROUP 3 MAX TIME 2 WEEK FREQUENCY', x = 'WEEK', 
          y = 'FREQUENCY')
  }
  
  ## Time 3
  if (nrow(inp4) > 0) {
  # Manipulation
  group3_time3 <- as.data.frame(inp4)
  group3_time3$year <- rownames(group3_time3)
  rownames(group3_time3) <- NULL
  
  group3_time3 <- subset(group3_time3, inc_yr3 <  year & year < inc_yr4)
  
  group3_min_time3 <- group3_time3[c('year', 'Min')]
  group3_max_time3 <- group3_time3[c('year', 'Max')]
  
  group3_min_time3$week <- ceiling(group3_min_time3$Min / 7) # ceiling() is to  
  # round to week
  group3_min_time3$date <- time_convert(input = group3_min_time3$Min, 
                                        output = 'yyyy-mm-dd')
  group3_min_time3 <- subset(group3_min_time3, select = -c(date))
  
  group3_max_time3$week <- ceiling(group3_max_time3$Max / 7)
  group3_max_time3$date <- time_convert(input = group3_max_time3$Max, 
                                        output = 'yyyy-mm-dd')
  group3_max_time3 <- subset(group3_max_time3, select = -c(date))
  
  group3_min_time3_week_frequency <- as.data.frame(plyr::count(group3_min_time3, 
                                                               vars = 'week'))
  
  group3_max_time3_week_frequency <- as.data.frame(plyr::count(group3_max_time3, 
                                                               vars = 'week'))
  # Visualization
  group3_min_time3_week_plot <- ggplot(data = group3_min_time3_week_frequency, 
                                  aes(x = week, y = freq)) +
    geom_bar(stat = 'identity') +
    scale_x_continuous(limits = c(0.5, 52.5), breaks = 1:52) +
    coord_polar() + 
    labs (title = 'GROUP 3 MIN TIME 3 WEEK FREQUENCY', x = 'WEEK', 
          y = 'FREQUENCY')
  
  group3_max_time3_week_plot <- ggplot(data = group3_max_time3_week_frequency, 
                                  aes(x = week, y = freq)) +
    geom_bar(stat = 'identity') +
    scale_x_continuous(limits = c(0.5, 52.5), breaks = 1:52) +
    coord_polar() + 
    labs (title = 'GROUP 3 MAX TIME 3 WEEK FREQUENCY', x = 'WEEK', 
          y = 'FREQUENCY')
  }
  
  ### Scatter Plot
  ## Full Time
  if (nrow(inp1) > 0) {
  # Manipulation
  group3_full_long <- gather(group3_full, key ='metric', value = 'julian day',
                             - year)
  group3_full_long$year <- as.numeric(group3_full_long$year)
  
  # Visualization
  group3_full_julian_day_plot <- ggplot(data = group3_full_long, 
                                        aes(x = year, y = `julian day`, 
                                            color = metric, group = metric)) +
    labs (title = 'GROUP 3 FULL TIME ', x = 'YEAR', y = 'JULIAN DAY') +
    ylim(0, 366) +
    geom_point() +
    geom_line() + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Extreme Flow")) +
    theme_minimal()
  }
  
  ## Time 1
  if (nrow(inp2) > 0) {
  # Manipulation
  group3_time1_long <- gather(group3_time1, key ='metric', value = 'julian day',
                              - year)
  group3_time1_long$year <- as.numeric(group3_time1_long$year)
  
  # Visualization
  group3_time1_julian_day_plot <- ggplot(data = group3_time1_long, 
                                         aes(x = year, y = `julian day`, 
                                             color = metric, group = metric)) +
    labs (title = 'GROUP 3 TIME 1', x = 'YEAR', y = 'JULIAN DAY') +
    ylim(0, 366) +
    geom_point() +
    geom_line() + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Extreme Flow")) +
    theme_minimal()
  }
  
  ## Time 2
  if (nrow(inp3) > 0) {
  # Manipulation
  group3_time2_long <- gather(group3_time2, key ='metric', value = 'julian day',
                              - year)
  group3_time2_long$year <- as.numeric(group3_time2_long$year)
  
  # Visualization
  group3_time2_julian_day_plot <- ggplot(data = group3_time2_long, 
                                         aes(x = year, y = `julian day`, 
                                             color = metric, group = metric)) +
    labs (title = 'GROUP 3 TIME 2', x = 'YEAR', y = 'JULIAN DAY') +
    ylim(0, 366) +
    geom_point() +
    geom_line() + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Extreme Flow")) +
    theme_minimal()
  }
  
  ## Time 3
  if (nrow(inp4) > 0) {
  # Manipulation
  group3_time3_long <- gather(group3_time3, key ='metric', value = 'julian day',
                              - year)
  group3_time3_long$year <- as.numeric(group3_time3_long$year)
  
  # Visualization
  group3_time3_julian_day_plot <- ggplot(data = group3_time3_long, 
                                         aes(x = year, y = `julian day`, 
                                             color = metric, group = metric)) +
    labs (title = 'GROUP 3 TIME 3', x = 'YEAR', y = 'JULIAN DAY') +
    ylim(0, 366) +
    geom_point() +
    geom_line() + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Extreme Flow")) +
    theme_minimal()
  }
  
  ### Return
  output <- list()
  #full time
  if (exists("group3_min_full_week_plot")) {
    output$group3_min_full_week_plot = group3_min_full_week_plot
  } 
  
  if (exists("group3_max_full_week_plot")) {
    output$group3_max_full_week_plot = group3_max_full_week_plot
  }
  
  if (exists("group3_full_julian_day_plot")) {
    output$group3_full_julian_day_plot = group3_full_julian_day_plot
  }
  
  #time 1
  if (exists("group3_min_full_week_plot")) {
    output$group3_min_time1_week_plot = group3_min_time1_week_plot
  } 
  
  if (exists("group3_max_time1_week_plot")) {
    output$group3_max_time1_week_plot = group3_max_time1_week_plot
  }
  
  if (exists("group3_time1_julian_day_plot")) {
    output$group3_time1_julian_day_plot = group3_time1_julian_day_plot
  }
  
  #time 2
  if (exists("group3_min_time2_week_plot")) {
    output$group3_min_time2_week_plot = group3_min_time2_week_plot
  } 
  
  if (exists("group3_max_time2_week_plot")) {
    output$group3_max_time2_week_plot = group3_max_time2_week_plot
  }
  
  if (exists("group3_time2_julian_day_plot")) {
    output$group3_time2_julian_day_plot = group3_time2_julian_day_plot
  }
  
  #time 3
  if (exists("group3_min_time3_week_plot")) {
    output$group3_min_time3_week_plot = group3_min_time3_week_plot
  } 
  
  if (exists("group3_max_time3_week_plot")) {
    output$group3_max_time3_week_plot = group3_max_time3_week_plot
  }
  
  if (exists("group3_time3_julian_day_plot")) {
    output$group3_time3_julian_day_plot = group3_time3_julian_day_plot
  }
  
  print(output)
  
}

