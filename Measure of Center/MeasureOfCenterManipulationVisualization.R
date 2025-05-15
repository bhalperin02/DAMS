# This function visualizes measure of center metrics over the full time period 
# and for times 1, 2, and 3. No manipulation has to occur.
# Updated 2.21.25 BH

# Inputs: measure_of_center_metrics_full  = 
#                           df containing measure of center full output 
#         measure_of_center_metrics_time1 =  
#                            df containing measure of center time 1 output
#         measure_of_center_metrics_time2 = 
#                            df containing measure of center time 2 output
#         measure_of_center_metrics_time3 = 
#                            df containing measure of center time 3 output
# 
# Outputs: measure_of_center_full_plot    = plot of measure of center full output 
#          measure_of_center_time1_plot   = plot of measure of center time 1 output
#          measure_of_center_time2_plot   = plot of measure of center time 2 output
#          measure_of_center_time3_plot   = plot of measure of center time 3 output

MeasureOfCenterManipulationVisualization <- function(
    inp1 = MeasureOfCenterMetricsOutput$measure_of_center_metrics_full,
    inp2 = MeasureOfCenterMetricsOutput$measure_of_center_metrics_time1,
    inp3 = MeasureOfCenterMetricsOutput$measure_of_center_metrics_time2,
    inp4 = MeasureOfCenterMetricsOutput$measure_of_center_metrics_time3) {
  
  ## Full Time
  if (nrow(inp1) > 0) {
    #turn df long
    measure_of_center_metrics_full_long = gather(inp1, key ='metric', 
                                                 value = 'number', - year)
    
    #visualization
    measure_of_center_full_plot <- ggplot(
      data = measure_of_center_metrics_full_long, 
      aes(x = year, y = number, color = metric,group = metric)) +
      labs (title = 'MEASURE OF CENTER FULL', x = 'YEAR', 
            y = 'STREAM FLOW (CFS)') +
      ylim(0, max(measure_of_center_metrics_full_long$number) * 1.25) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) + 
      scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
      guides(color = guide_legend(title = "Measure of Center")) +
      theme_minimal()
  }
  
  ##time 1
  if (nrow(inp2) > 0) {
    #turn df long
    measure_of_center_metrics_time1_long = gather(inp2, key ='metric', 
                                                 value = 'number', - year)
    
    #visualization
    measure_of_center_time1_plot <- ggplot(
      data = measure_of_center_metrics_time1_long, 
      aes(x = year, y = number, color = metric,group = metric)) +
      labs (title = 'MEASURE OF CENTER TIME 1', x = 'YEAR', 
            y = 'STREAM FLOW (CFS)') +
      ylim(0, max(measure_of_center_metrics_full_long$number) * 1.25) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) + 
      scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
      guides(color = guide_legend(title = "Measure of Center")) +
      theme_minimal()
  }
 
  
  ##time 2
  if (nrow(inp3) > 0) {
    #turn df long
    measure_of_center_metrics_time2_long = gather(inp3, key ='metric', 
                                                  value = 'number', - year)
    
    #visualization
    measure_of_center_time2_plot <- ggplot(
      data = measure_of_center_metrics_time2_long, 
      aes(x = year, y = number, color = metric,group = metric)) +
      labs (title = 'MEASURE OF CENTER TIME 2', x = 'YEAR', 
            y = 'STREAM FLOW (CFS)') +
      ylim(0, max(measure_of_center_metrics_full_long$number) * 1.25) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) + 
      scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
      guides(color = guide_legend(title = "Measure of Center")) +
      theme_minimal()    
  }
  
  
  ##time 3
  if (nrow(inp4) > 0) {
    #turn df long
    measure_of_center_metrics_time3_long = gather(inp4, key ='metric', 
                                                  value = 'number', - year)
    
    #visualization
    measure_of_center_time3_plot <- ggplot(
      data = measure_of_center_metrics_time1_long, 
      aes(x = year, y = number, color = metric,group = metric)) +
      labs (title = 'MEASURE OF CENTER TIME 3', x = 'YEAR', 
            y = 'STREAM FLOW (CFS)') +
      ylim(0, max(measure_of_center_metrics_full_long$number) * 1.25) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) + 
      scale_color_manual(values = (c("#000000", "#CC79A7"))) + 
      guides(color = guide_legend(title = "Measure of Center")) +
      theme_minimal()  
  }
  

  # return
  output <- list()
  
  if (exists('measure_of_center_full_plot')) {
    output$measure_of_center_full_plot = measure_of_center_full_plot
  } 
  
  if (exists('measure_of_center_time1_plot')) {
    output$measure_of_center_time1_plot = measure_of_center_time1_plot
  } 
  
  if (exists('measure_of_center_time2_plot')) {
    output$measure_of_center_time2_plot = measure_of_center_time2_plot
  } 
  
  if (exists('measure_of_center_time3_plot')) {
    output$measure_of_center_time3_plot = measure_of_center_time3_plot
  } 
  
  
  print(output)
  
}


  



