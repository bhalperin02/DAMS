# This function manipulates then visualizes group 5 over the full time period
# and for times 1, 2, and 3
# Updates 2.25.25 BH

# Inputs: group5_full  = matrix containing group 5 outputs for full time
#         group5_time1 = matrix containing group 5 outputs for time 1
#         group5_time2 = matrix containing group 5 outputs for time 2
#         group5_time3 = matrix containing group 5 outputs for time 3
# 
# Outputs: group5_rate_full_plot      = plot with group 5 rate for full time
#          group5_reversal_full_plot  = plot with group 5 reversals for full time
#          group5_rate_time1_plot     = plot with group 5 rate for time 1
#          group5_reversal_time1_plot = plot with group 5 reversals for time 1
#          group5_rate_time2_plot     = plot with group 5 rate for time 2
#          group5_reversal_time2_plot = plot with group 5 reversals for time 2
#          group5_rate_time3_plot     = plot with group 5 rate for time 3
#          group5_reversal_time3_plot = plot with group 5 reversals for time 3
  
IHAGroup5ManipulationVisualization <- function(
    inp1 = IHAGroupPrepOutput$group5_full,
    inp2 = IHAGroupPrepOutput$group5_time1,
    inp3 = IHAGroupPrepOutput$group5_time2,
    inp4 = IHAGroupPrepOutput$group5_time3) {
  
  ## Full Time
  if (nrow(inp1) > 0) {
  # Manipulation
  group5_full = as.data.frame(inp1)
  group5_full$year = rownames(group5_full)
  rownames(group5_full) = NULL
  group5_full$`Fall rate` = (-1*group5_full$`Fall rate`)
  
  group5_full = subset(group5_full, inc_yr1 < year & year < inc_yr4)
  
  group5_rate_full = group5_full[c('year', 'Rise rate', 'Fall rate')]
  group5_reversal_full = group5_full[c('year', 'Reversals')]
  
  group5_rate_full_long = gather(group5_rate_full, key ='metric', 
                                 value = 'rate', - year)
  group5_reversal_full_long = gather(group5_reversal_full, key ='metric', 
                                     value = 'reversals', - year)
  group5_rate_full_long$year = as.numeric(group5_rate_full_long$year)
  group5_reversal_full_long$year = as.numeric(group5_reversal_full_long$year)
  
  # Visualization
  group5_rate_full_plot <- ggplot(
    data = group5_rate_full_long, aes(x = year, y = rate, color = metric, 
                                      group = metric)) +
    labs (title = 'GROUP 5 RATE FULL TIME', x = 'YEAR', 
          y = 'RATE OF CHANGE (CFS/DAY)') +
    ylim (0, max(group5_rate_full_long$rate) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Change")) +
    theme_minimal()
    
  group5_reversal_full_plot <- ggplot(
    data = group5_reversal_full_long, aes(x = year, y = reversals, 
                                          color = metric, group = metric)) +
    labs (title = 'GROUP 5 REVERSAL FULL TIME', x = 'YEAR', 
          y = 'REVERSAL PER YEAR') +
    ylim (min(group5_reversal_full_long$reversals) * .9, 
          max(group5_reversal_full_long$reversals) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000"))) + 
    guides(color = guide_legend(title = "Reversals")) +
    theme_minimal() 
  }

  ## Time 1
  if (nrow(inp2) > 0) {
  # Manipulation
  group5_time1 = as.data.frame(inp2)
  group5_time1$year = rownames(group5_time1)
  rownames(group5_time1) = NULL
  group5_time1$`Fall rate` = (-1*group5_time1$`Fall rate`)
  
  group5_time1 = subset(group5_time1, inc_yr1 < year & year < inc_yr4)
  
  group5_rate_time1 = group5_time1[c('year', 'Rise rate', 'Fall rate')]
  group5_reversal_time1 = group5_time1[c('year', 'Reversals')]
  
  group5_rate_time1_long = gather(group5_rate_time1, key ='metric',
                                  value = 'rate', - year)
  group5_reversal_time1_long = gather(group5_reversal_time1, key ='metric', 
                                      value = 'reversals', - year)
  group5_rate_time1_long$year = as.numeric(group5_rate_time1_long$year)
  group5_reversal_time1_long$year = as.numeric(group5_reversal_time1_long$year)
  
  # Visualization
  group5_rate_time1_plot <- ggplot(
    data = group5_rate_time1_long, aes(x = year, y = rate, color = metric, 
                                       group = metric)) +
    labs (title = 'GROUP 5 RATE TIME 1', x = 'YEAR', 
          y = 'RATE OF CHANGE (CFS/DAY)') +
    ylim (0, max(group5_rate_full_long$rate) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Change")) +
    theme_minimal()
  
  group5_reversal_time1_plot <- ggplot(
    data = group5_reversal_time1_long, aes(x = year, y = reversals, 
                                           color = metric, group = metric)) +
    labs (title = 'GROUP 5 REVERSAL TIME 1', x = 'YEAR', 
          y = 'REVERSAL PER YEAR') +
    ylim (min(group5_reversal_full_long$reversals) * .9, 
          max(group5_reversal_full_long$reversals) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000"))) + 
    guides(color = guide_legend(title = "Reversals")) +
    theme_minimal() 
  }
  
  ## Time 2
  if (nrow(inp3) > 0) {
  # Manipulation
  group5_time2 = as.data.frame(inp3)
  group5_time2$year = rownames(group5_time2)
  rownames(group5_time2) = NULL
  group5_time2$`Fall rate` = (-1*group5_time2$`Fall rate`)
  
  group5_time2 = subset(group5_time2, inc_yr1 < year & year < inc_yr4)
  
  group5_rate_time2 = group5_time2[c('year', 'Rise rate', 'Fall rate')]
  group5_reversal_time2 = group5_time2[c('year', 'Reversals')]
  
  group5_rate_time2_long = gather(group5_rate_time2, key ='metric',
                                  value = 'rate', - year)
  group5_reversal_time2_long = gather(group5_reversal_time2, key ='metric',
                                      value = 'reversals', - year)
  group5_rate_time2_long$year = as.numeric(group5_rate_time2_long$year)
  group5_reversal_time2_long$year = as.numeric(group5_reversal_time2_long$year)
  
  # Visualization
  group5_rate_time2_plot <- ggplot(
    data = group5_rate_time2_long, aes(x = year, y = rate, color = metric, 
                                       group = metric)) +
    labs (title = 'GROUP 5 RATE TIME 2', x = 'YEAR', 
          y = 'RATE OF CHANGE (CFS/DAY)') +
    ylim (0, max(group5_rate_full_long$rate) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Change")) +
    theme_minimal()
  
  group5_reversal_time2_plot <- ggplot(
    data = group5_reversal_time2_long, aes(x = year, y = reversals, 
                                           color = metric, group = metric)) +
    labs (title = 'GROUP 5 REVERSAL TIME 2', x = 'YEAR', 
          y = 'REVERSAL PER YEAR') +
    ylim (min(group5_reversal_full_long$reversals) * .9, 
          max(group5_reversal_full_long$reversals) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000"))) + 
    guides(color = guide_legend(title = "Reversals")) +
    theme_minimal() 
  }
  
  ## Time 3
  if (nrow(inp4) > 0) {
  # Manipulation
  group5_time3 = as.data.frame(inp4)
  group5_time3$year = rownames(group5_time3)
  rownames(group5_time3) = NULL
  group5_time3$`Fall rate` = (-1*group5_time3$`Fall rate`)
  
  group5_time3 = subset(group5_time3, inc_yr1 < year & year < inc_yr4)
  
  group5_rate_time3 = group5_time3[c('year', 'Rise rate', 'Fall rate')]
  group5_reversal_time3 = group5_time3[c('year', 'Reversals')]
  
  group5_rate_time3_long = gather(group5_rate_time3, key ='metric',
                                  value = 'rate', - year)
  group5_reversal_time3_long = gather(group5_reversal_time3, key ='metric',
                                      value = 'reversals', - year)
  group5_rate_time3_long$year = as.numeric(group5_rate_time3_long$year)
  group5_reversal_time3_long$year = as.numeric(group5_reversal_time3_long$year)
  
  # Visualization
  group5_rate_time3_plot <- ggplot(
    data = group5_rate_time3_long, aes(x = year, y = rate, color = metric, 
                                       group = metric)) +
    labs (title = 'GROUP 5 RATE TIME 3', x = 'YEAR', 
          y = 'RATE OF CHANGE (CFS/DAY)') +
    ylim (0, max(group5_rate_full_long$rate) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
    guides(color = guide_legend(title = "Type of Change")) +
    theme_minimal()
  
  group5_reversal_time3_plot <- ggplot(
    data = group5_reversal_time3_long, aes(x = year, y = reversals, 
                                           color = metric, group = metric)) +
    labs (title = 'GROUP 5 REVERSAL TIME 3', x = 'YEAR', 
          y = 'REVERSAL PER YEAR') +
    ylim (min(group5_reversal_full_long$reversals) * .9, 
          max(group5_reversal_full_long$reversals) * 1.1) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    scale_color_manual(values = (c("#000000"))) + 
    guides(color = guide_legend(title = "Reversals")) +
    theme_minimal() 
  }
  
  ## Return
  output <- list()
  #full time
  if (exists("group5_rate_full_plot")) {
    output$group5_rate_full_plot = group5_rate_full_plot
  } 
  
  if (exists("group5_reversal_full_plot")) {
    output$group5_reversal_full_plot = group5_reversal_full_plot
  }
  
  #time 1
  if (exists("group5_rate_time1_plot")) {
    output$group5_rate_time1_plot = group5_rate_time1_plot
  } 
  
  if (exists("group5_reversal_time1_plot")) {
    output$group5_reversal_time1_plot = group5_reversal_time1_plot
  }
  
  #time 2
  if (exists("group5_rate_time2_plot")) {
    output$group5_rate_time2_plot = group5_rate_time2_plot
  } 
  
  if (exists("group5_reversal_time2_plot")) {
    output$group5_reversal_time2_plot = group5_reversal_time2_plot
  }
  
  #time 3
  if (exists("group5_rate_time3_plot")) {
    output$group5_rate_time3_plot = group5_rate_time3_plot
  } 
  
  if (exists("group5_reversal_time3_plot")) {
    output$group5_reversal_time3_plot = group5_reversal_time3_plot
  }
  
  print(output)
     
}
