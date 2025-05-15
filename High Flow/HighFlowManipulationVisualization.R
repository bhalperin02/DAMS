# This function visualizes high flow metrics over the full time period 
# and for times 1, 2, and 3. No manipulation has to occur.
# Updated 2.21.25 BH

# Inputs: high_flow_metrics_full = df containing high flow full output 
#         high_flow_metrics_time1 = df containing high flow time 1 output
#         high_flow_metrics_time2 = df containing high flow time 2 output
#         high_flow_metrics_time3 = df containing high flow time 3 output
# 
# Outputs: high_flow_full_plot      = plot of high flow full output 
#          high_flow_time1_plot     = plot of high flow time 1 output
#          high_flow_time2_plot     = plot of high flow time 2 output
#          high_flow_time3_plot     = plot of high flow time 3 output

HighFlowManipulationVisualization <- function(
    inp1 = HighFlowMetricsOutput$high_flow_metrics_full,
    inp2 = HighFlowMetricsOutput$high_flow_metrics_time1,
    inp3 = HighFlowMetricsOutput$high_flow_metrics_time2,
    inp4 = HighFlowMetricsOutput$high_flow_metrics_time3) {
  
  ## Full Time
  if (nrow(inp1) > 0) {
  high_flow_full_plot <- ggplot(
    data = inp1, aes(x = year, y = flow)) +
    labs (title = 'TOP 10% DAILY FLOW PER YEAR FULL TIME', x = 'YEAR', 
          y = 'FLOW (CFS)') +
    ylim(0, max(inp1$flow)*1.1) +
    geom_point(na.rm = TRUE) +
    geom_smooth(na.rm = TRUE) + 
    theme_minimal()
  }
  
  ##time 1
  if (nrow(inp2) > 0) {
    high_flow_time1_plot <- ggplot(
      data = inp2, aes(x = year, y = flow)) +
      labs (title = 'TOP 10% DAILY FLOW PER YEAR TIME 1', x = 'YEAR', 
            y = 'FLOW (CFS)') +
      ylim(0, max(inp1$flow)*1.1) +
      geom_point(na.rm = TRUE) +
      geom_smooth(na.rm = TRUE) + 
      theme_minimal()
  }

  
  ##time 2
  if (nrow(inp3) > 0) {
    high_flow_time2_plot <- ggplot(
      data = inp3, aes(x = year, y = flow)) +
      labs (title = 'TOP 10% DAILY FLOW PER YEAR TIME 2', x = 'YEAR', 
            y = 'FLOW (CFS)') +
      ylim(0, max(inp1$flow)*1.1) +
      geom_point(na.rm = TRUE) +
      geom_smooth(na.rm = TRUE) + 
      theme_minimal()
  }
  
  ##time 3
  if (nrow(inp4) > 0) {
    high_flow_time3_plot <- ggplot(
      data = inp4, aes(x = year, y = flow)) +
      labs (title = 'TOP 10% DAILY FLOW PER YEAR TIME 3', x = 'YEAR', 
            y = 'FLOW (CFS)') +
      ylim(0, max(inp1$flow)*1.1) +
      geom_point(na.rm = TRUE) +
      geom_smooth(na.rm = TRUE) + 
      theme_minimal()
  }

  
  # return
  output <- list()
  
  if (exists('high_flow_full_plot')) {
    output$high_flow_full_plot = high_flow_full_plot
  } 
  
  if (exists('high_flow_time1_plot')) {
    output$high_flow_time1_plot = high_flow_time1_plot
  } 
  
  if (exists('high_flow_time2_plot')) {
    output$high_flow_time2_plot = high_flow_time2_plot
  } 
  
  if (exists('high_flow_time3_plot')) {
    output$high_flow_time3_plot = high_flow_time3_plot
  } 
 
  print(output)
  
}


  



