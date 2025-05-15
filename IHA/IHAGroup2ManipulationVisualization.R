# This function manipulates then visualizes group 2 over the full time period
# and for times 1, 2, and 3
# Updates 2.24.25 BH

# Inputs: group2_full  = df containing group 2 outputs for full time
#         group2_time1 = df containing group 2 outputs for time 1
#         group2_time2 = df containing group 2 outputs for time 2
#         group2_time2 = df containing group 2 outputs for time 3
# 
# Outputs: group2_min_full_plot  = plot with group 2 min values for full time
#          group2_max_full_plot  = plot with group 2 max values for full time
#          group2_BI_full_plot   = plot with group 2 base index for full time
#          group2_zero_full_plot = plot with group 2 zero flow for full time
#          group2_min_time1_plot  = plot with group 2 min values for time 1
#          group2_max_time1_plot  = plot with group 2 max values for time 1
#          group2_BI_time1_plot   = plot with group 2 base index for time 1
#          group2_zero_time1_plot = plot with group 2 zero flow for time 1
#          group2_min_time2_plot  = plot with group 2 min values for time 2
#          group2_max_time2_plot  = plot with group 2 max values for time 2
#          group2_BI_time2_plot   = plot with group 2 base index for time 2
#          group2_zero_time2_plot = plot with group 2 zero flow for time 2
#          group2_min_time3_plot  = plot with group 2 min values for time 3
#          group2_max_time3_plot  = plot with group 2 max values for time 3
#          group2_BI_time3_plot   = plot with group 2 base index for time 3
#          group2_zero_time3_plot = plot with group 2 zero flow for time 3

IHAGroup2ManipulationVisualization <- function(
    inp1 = IHAGroupPrepOutput$group2_full,
    inp2 = IHAGroupPrepOutput$group2_time1,
    inp3 = IHAGroupPrepOutput$group2_time2,
    inp4 = IHAGroupPrepOutput$group2_time3) {
  
  
  ## Full time
  if (nrow(inp1) > 0) {
  # Manipulation
  inp1 <- subset(inp1, inc_yr1 < year & year < inc_yr4)
  
  group2_min_full <- inp1[c('year', '1 Day Min', '3 Day Min', 
                                   '7 Day Min', '30 Day Min', '90 Day Min')]
  group2_max_full <- inp1[c('year', '1 Day Max', '3 Day Max', 
                                   '7 Day Max', '30 Day Max', '90 Day Max')]
  group2_other_full <- inp1[c('year', 'Zero flow days', 'Base index')]
  
  group2_min_full_long <- gather(group2_min_full, key = 'metric',
                                 value = 'flow', -year)
  group2_max_full_long <- gather(group2_max_full, key = 'metric', 
                                 value = 'flow', -year)
  group2_other_full_long <- gather(group2_other_full, key = 'metric',
                                   value = 'flow', -year)
  
  # Visualization
  group2_min_full_plot <- ggplot(
    data = group2_min_full_long, aes(x = year, y = flow, color = metric,
                                     group = metric)) + 
    labs (title = 'GROUP 2 MINIMUM FULL TIME', x = 'YEAR', y = 'FLOW (CFS)') +
    ylim (0, (max(group2_min_full_long$flow) * 1.1)) +
    geom_point() + 
    geom_line() +
    scale_color_manual(values = (c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                   "#CC79A7"))) +
    guides(color = guide_legend(title = "Minimum Flow Period")) +
    theme_minimal()
  
  group2_max_full_plot <- ggplot(
    data = group2_max_full_long, aes(x = year, y = flow, color = metric,
                                     group = metric)) + 
    labs (title = 'GROUP 2 MAXIMUM FULL TIME', x = 'YEAR', y = 'FLOW (CFS)') +
    ylim (0, max(group2_max_full_long$flow) * 1.1) +
    geom_point() + 
    geom_line() +
    scale_color_manual(values = (c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                   "#CC79A7"))) + 
    guides(color = guide_legend(title = "Maximum Flow Period")) +
    theme_minimal()
  
  group2_BI_full_plot <- ggplot(
    data = group2_other_full, aes(x = year, y = `Base index`)) + 
    labs (title = 'GROUP 2 BASE INDEX FULL TIME', x = 'YEAR', 
          y = 'BASE INDEX (7 DAY MIN FLOW/ANNUAL MEAN FLOW)') + 
    ylim (0, 1) +
    geom_point() + 
    geom_line() +
    theme_minimal()
  
  group2_zero_full_plot <- ggplot(
    data = group2_other_full, aes(x = year, y = `Zero flow days`)) + 
    labs (title = 'GROUP 2 ZERO FLOW DAYS FULL TIME', x = 'YEAR', 
          y = 'ZERO FLOW DAYS') + 
    ylim (0, max(group2_other_full$`Zero flow days` + 2)) +
    geom_point() + 
    geom_line() +
    theme_minimal()
  }
  
  ## Time 1
  if (nrow(inp2) > 0) {
  # Manipulation
  inp2 <- subset(inp2, inc_yr1 < year & year < inc_yr2)
  
  group2_min_time1 <- inp2[c('year', '1 Day Min', '3 Day Min', 
                                     '7 Day Min', '30 Day Min', '90 Day Min')]
  group2_max_time1 <- inp2[c('year', '1 Day Max', '3 Day Max', 
                                     '7 Day Max', '30 Day Max', '90 Day Max')]
  group2_other_time1 <- inp2[c('year', 'Zero flow days', 'Base index')]
  
  group2_min_time1_long <- gather(group2_min_time1, key = 'metric',
                                  value = 'flow', -year)
  group2_max_time1_long <- gather(group2_max_time1, key = 'metric', 
                                  value = 'flow', -year)
  group2_other_time1_long <- gather(group2_other_time1, key = 'metric',
                                    value = 'flow', -year)
  
  # Visualization
  group2_min_time1_plot <- ggplot(
    data = group2_min_time1_long, aes(x = year, y = flow, color = metric,
                                      group = metric)) + 
    labs (title = 'GROUP 2 MINIMUM TIME 1', x = 'YEAR', y = 'FLOW (CFS)') +
    ylim (0, (max(group2_min_full_long$flow) * 1.1)) +
    geom_point() + 
    geom_line() +
    scale_color_manual(values = (c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                                   "#CC79A7"))) +
    guides(color = guide_legend(title = "Minimum Flow Period")) +
    theme_minimal()
  
  group2_max_time1_plot <- ggplot(
    data = group2_max_time1_long, aes(x = year, y = flow, color = metric,
                                      group = metric)) + 
    labs (title = 'GROUP 2 MAXIMUM TIME 1', x = 'YEAR', y = 'FLOW (CFS)') +
    ylim (0, max(group2_max_full_long$flow) * 1.1) +
    geom_point() + 
    geom_line() +
    scale_color_manual(values = (c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                   "#CC79A7"))) + 
    guides(color = guide_legend(title = "Maximum Flow Period")) +
    theme_minimal()
  
  group2_BI_time1_plot <- ggplot(
    data = group2_other_time1, aes(x = year, y = `Base index`)) + 
    labs (title = 'GROUP 2 BASE INDEX TIME 1', x = 'YEAR', 
          y = 'BASE INDEX (7 DAY MIN FLOW/ANNUAL MEAN FLOW)') + 
    ylim (0, 1) +
    geom_point() + 
    geom_line() +
    theme_minimal()
  
  group2_zero_time1_plot <- ggplot(
    data = group2_other_time1, aes(x = year, y = `Zero flow days`)) + 
    labs (title = 'GROUP 2 ZERO FLOW DAYS TIME 1', x = 'YEAR', 
          y = 'ZERO FLOW DAYS') + 
    ylim (0, max(group2_other_full$`Zero flow days` + 2)) +
    geom_point() + 
    geom_line() +
    theme_minimal()
  }
  
  ## Time 2
  if (nrow(inp3) > 0) {
  # Manipulation
  inp3 <- subset(inp3, inc_yr2 < year & year < inc_yr3)
  
  group2_min_time2 <- inp3[c('year', '1 Day Min', '3 Day Min', 
                                     '7 Day Min', '30 Day Min', '90 Day Min')]
  group2_max_time2 <- inp3[c('year', '1 Day Max', '3 Day Max', 
                                     '7 Day Max', '30 Day Max', '90 Day Max')]
  group2_other_time2 <- inp3[c('year', 'Zero flow days', 'Base index')]
  
  group2_min_time2_long <- gather(group2_min_time2, key = 'metric',
                                  value = 'flow', -year)
  group2_max_time2_long <- gather(group2_max_time2, key = 'metric', 
                                  value = 'flow', -year)
  group2_other_time2_long <- gather(group2_other_time2, key = 'metric',
                                    value = 'flow', -year)
  
  # Visualization
  group2_min_time2_plot <- ggplot(
    data = group2_min_time2_long, aes(x = year, y = flow, color = metric,
                                      group = metric)) + 
    labs (title = 'GROUP 2 MINIMUM TIME 2', x = 'YEAR', y = 'FLOW (CFS)') +
    ylim (0, (max(group2_min_full_long$flow) * 1.1)) +
    geom_point() + 
    geom_line() +
    scale_color_manual(values = (c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                                   "#CC79A7"))) +
    guides(color = guide_legend(title = "Minimum Flow Period")) +
    theme_minimal()
  
  group2_max_time2_plot <- ggplot(
    data = group2_max_time2_long, aes(x = year, y = flow, color = metric,
                                      group = metric)) + 
    labs (title = 'GROUP 2 MAXIMUM TIME 2', x = 'YEAR', y = 'FLOW (CFS)') +
    ylim (0, max(group2_max_full_long$flow) * 1.1) +
    geom_point() + 
    geom_line() +
    scale_color_manual(values = (c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                   "#CC79A7"))) + 
    guides(color = guide_legend(title = "Maximum Flow Period")) +
    theme_minimal()
  
  group2_BI_time2_plot <- ggplot(
    data = group2_other_time2, aes(x = year, y = `Base index`)) + 
    labs (title = 'GROUP 2 BASE INDEX TIME 2', x = 'YEAR', 
          y = 'BASE INDEX (7 DAY MIN FLOW/ANNUAL MEAN FLOW)') + 
    ylim (0, 1) +
    geom_point() + 
    geom_line() +
    theme_minimal()
  
  group2_zero_time2_plot <- ggplot(
    data = group2_other_time2, aes(x = year, y = `Zero flow days`)) + 
    labs (title = 'GROUP 2 ZERO FLOW DAYS TIME 2', x = 'YEAR', 
          y = 'ZERO FLOW DAYS') + 
    ylim (0, max(group2_other_full$`Zero flow days` + 2)) +
    geom_point() + 
    geom_line() +
    theme_minimal()
  
  }
  
  ## Time 3
  if (nrow(inp4) > 0) {
  # Manipulation
  inp4 <- subset(inp4, inc_yr3 < year & year < inc_yr4)
  
  group2_min_time3 <- inp4[c('year', '1 Day Min', '3 Day Min', 
                                     '7 Day Min', '30 Day Min', '90 Day Min')]
  group2_max_time3 <- inp4[c('year', '1 Day Max', '3 Day Max', 
                                     '7 Day Max', '30 Day Max', '90 Day Max')]
  group2_other_time3 <- inp4[c('year', 'Zero flow days', 'Base index')]
  
  group2_min_time3_long <- gather(group2_min_time3, key = 'metric',
                                  value = 'flow', -year)
  group2_max_time3_long <- gather(group2_max_time3, key = 'metric', 
                                  value = 'flow', -year)
  group2_other_time3_long <- gather(group2_other_time3, key = 'metric',
                                    value = 'flow', -year)
  
  # Visualization
  group2_min_time3_plot <- ggplot(
    data = group2_min_time3_long, aes(x = year, y = flow, color = metric,
                                      group = metric)) + 
    labs (title = 'GROUP 2 MINIMUM TIME 3', x = 'YEAR', y = 'FLOW (CFS)') +
    ylim (0, (max(group2_min_full_long$flow) * 1.1)) +
    geom_point() + 
    geom_line() +
    scale_color_manual(values = (c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                                   "#CC79A7"))) +
    guides(color = guide_legend(title = "Minimum Flow Period")) +
    theme_minimal()
  
  group2_max_time3_plot <- ggplot(
    data = group2_max_time3_long, aes(x = year, y = flow, color = metric,
                                      group = metric)) + 
    labs (title = 'GROUP 2 MAXIMUM TIME 3', x = 'YEAR', y = 'FLOW (CFS)') +
    ylim (0, max(group2_max_full_long$flow) * 1.1) +
    geom_point() + 
    geom_line() +
    scale_color_manual(values = (c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                   "#CC79A7"))) + 
    guides(color = guide_legend(title = "Maximum Flow Period")) +
    theme_minimal()
  
  group2_BI_time3_plot <- ggplot(
    data = group2_other_time3, aes(x = year, y = `Base index`)) + 
    labs (title = 'GROUP 2 BASE INDEX TIME 3', x = 'YEAR', 
          y = 'BASE INDEX (7 DAY MIN FLOW/ANNUAL MEAN FLOW)') + 
    ylim (0, 1) +
    geom_point() + 
    geom_line() +
    theme_minimal()
  
  group2_zero_time3_plot <- ggplot(
    data = group2_other_time3, aes(x = year, y = `Zero flow days`)) + 
    labs (title = 'GROUP 2 ZERO FLOW DAYS TIME 3', x = 'YEAR', 
          y = 'ZERO FLOW DAYS') + 
    ylim (0, max(group2_other_full$`Zero flow days` + 2)) +
    geom_point() + 
    geom_line() +
    theme_minimal()
  
  }
  
  ## Return
  output <- list()
  
  #full time
  if (exists("group2_min_full_plot")) {
    output$group2_min_full_plot = group2_min_full_plot
  } 
  
  if (exists("group2_max_full_plot")) {
    output$group2_max_full_plot = group2_max_full_plot
  }
  
  if (exists("group2_BI_full_plot")) {
    output$group2_BI_full_plot = group2_BI_full_plot
  } 
  
  if (exists("group2_zero_full_plot")) {
    output$group2_zero_full_plot = group2_zero_full_plot
  } 
  
  #time 1
  if (exists("group2_min_time1_plot")) {
    output$group2_min_time1_plot = group2_min_time1_plot
  } 
  
  if (exists("group2_max_time1_plot")) {
    output$group2_max_time1_plot = group2_max_time1_plot
  } 
  
  if (exists("group2_BI_time1_plot")) {
    output$group2_BI_time1_plot = group2_BI_time1_plot
  } 
  
  if (exists("group2_zero_time1_plot")) {
    output$group2_zero_time1_plot = group2_zero_time1_plot
  } 
  
  #time 2
  if (exists("group2_min_time2_plot")) {
    output$group2_min_time2_plot = group2_min_time2_plot
  } 
  
  if (exists("group2_max_time2_plot")) {
    output$group2_max_time2_plot = group2_max_time2_plot
  } 
  
  if (exists("group2_BI_time2_plot")) {
    output$group2_BI_time2_plot = group2_BI_time2_plot
  } 
  
  if (exists("group2_zero_time2_plot")) {
    output$group2_zero_time2_plot = group2_zero_time2_plot
  }
  
  #time 3
  if (exists("group2_min_time3_plot")) {
    output$group2_min_time3_plot = group2_min_time3_plot
  } 
  
  if (exists("group2_max_time3_plot")) {
    output$group2_max_time3_plot = group2_max_time3_plot
  } 
  
  if (exists("group2_BI_time3_plot")) {
    output$group2_BI_time3_plot = group2_BI_time3_plot
  } 
  
  if (exists("group2_zero_time3_plot")) {
    output$group2_zero_time3_plot = group2_zero_time3_plot
  }
  
  print(output)
  
}
  

