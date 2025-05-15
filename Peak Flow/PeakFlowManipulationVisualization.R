# This function visualizes peak flow metrics over the full time period 
# and for times 1, 2, and 3. No manipulation has to occur.
# Updated 2.21.25 BH

# Inputs: peak_RI_full  = df containing peak flow full output 
#         peak_RI_time1 = df containing peak flow time 1 output
#         peak_RI_time2 = df containing peak flow time 2 output
#         peak_RI_time3 = df containing peak flow time 3 output
# 
# Outputs: peak_flow_full_plot      = plot of peak flow full output 
#          peak_flow_time1_plot     = plot of peak flow time 1 output
#          peak_flow_time2_plot     = plot of peak flow time 2 output
#          peak_flow_time3_plot     = plot of peak flow time 3 output
#          APE_full_plot            = plot of APE full output 
#          APE_time1_plot           = plot of APE time 1 output
#          APE_time2_plot           = plot of APE time 2 output
#          APE_time3_plot           = plot of APE time 3 output

PeakFlowManipulationVisualization <- function(
    inp1 = PeakFlowMetricsOutput$peak_RI_full,
    inp2 = PeakFlowMetricsOutput$peak_RI_time1,
    inp3 = PeakFlowMetricsOutput$peak_RI_time2,
    inp4 = PeakFlowMetricsOutput$peak_RI_time3) {
  
  ## Full Time
  #annual peak flow visualization
  if (nrow(inp1) > 0) {
  peak_flow_full_plot <- ggplot(
    data = inp1, aes(x = peak_dt, y = peak_va)) +
    labs (title = 'ANNUAL PEAK FLOW FULL TIME ', x = 'DATE', 
          y = 'FLOW (CFS)') +
    ylim(0, max(inp1$peak_va)*1.25) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) + 
    theme_minimal()
  }
  
  #APE visualization
  if (nrow(inp1) > 0) {
    APE_full_plot <- ggplot(
      data = inp1, aes(x = peak_va, y = APE)) +
      labs (title = 'ANNUAL PROBABILITY OF EXCEEDANCE FULL TIME', 
            x = 'FLOW (CFS)', 
            y = 'ANNUAL PROBABILITY OF EXCEEDANCE') +
      ylim(0, 1) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) + 
      theme_minimal()
  }
  
  
  ##time 1
  #annual peak flow visualization
  if (nrow(inp2) > 0) {
    peak_flow_time1_plot <- ggplot(
      data = inp2, aes(x = peak_dt, y = peak_va)) +
      labs (title = 'ANNUAL PEAK FLOW TIME 1', x = 'DATE', 
            y = 'FLOW (CFS)') +
      ylim(0, max(inp1$peak_va)*1.25) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) + 
      theme_minimal()
  }
  
  #APE visualization
  if (nrow(inp2) > 0) {
    APE_time1_plot <- ggplot(
      data = inp2, aes(x = peak_va, y = APE)) +
      labs (title = 'ANNUAL PROBABILITY OF EXCEEDANCE TIME 1', 
            x = 'FLOW (CFS)', 
            y = 'ANNUAL PROBABILITY OF EXCEEDANCE') +
      ylim(0, 1) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) + 
      theme_minimal()
  }

  
  ##time 2
  #annual peak flow visualization
  if (nrow(inp3) > 0) {
    peak_flow_time2_plot <- ggplot(
      data = inp3, aes(x = peak_dt, y = peak_va)) +
      labs (title = 'ANNUAL PEAK FLOW TIME 2', x = 'DATE', 
            y = 'FLOW (CFS)') +
      ylim(0, max(inp1$peak_va)*1.25) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) + 
      theme_minimal()
  }
  
  #APE visualization
  if (nrow(inp3) > 0) {
    APE_time2_plot <- ggplot(
      data = inp3, aes(x = peak_va, y = APE)) +
      labs (title = 'ANNUAL PROBABILITY OF EXCEEDANCE TIME 2', 
            x = 'FLOW (CFS)', 
            y = 'ANNUAL PROBABILITY OF EXCEEDANCE') +
      ylim(0, 1) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) + 
      theme_minimal()
  }
  
  ##time 3
  #annual peak flow visualization
  if (nrow(inp4) > 0) {
    peak_flow_time3_plot <- ggplot(
      data = inp4, aes(x = peak_dt, y = peak_va)) +
      labs (title = 'ANNUAL PEAK FLOW TIME 3', x = 'DATE', 
            y = 'FLOW (CFS)') +
      ylim(0, max(inp1$peak_va)*1.25) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) + 
      theme_minimal()
  }
  
  #APE visualization
  if (nrow(inp4) > 0) {
    APE_time3_plot <- ggplot(
      data = inp4, aes(x = peak_va, y = APE)) +
      labs (title = 'ANNUAL PROBABILITY OF EXCEEDANCE TIME 3', 
            x = 'FLOW (CFS)', 
            y = 'ANNUAL PROBABILITY OF EXCEEDANCE') +
      ylim(0, 1) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) + 
      theme_minimal()
  }      
  
  # return
  output <- list()
  
  if (exists('peak_flow_full_plot')) {
    output$peak_flow_full_plot = peak_flow_full_plot
  } 
  
  if (exists('peak_flow_time1_plot')) {
    output$peak_flow_time1_plot = peak_flow_time1_plot
  } 
  
  if (exists('peak_flow_time2_plot')) {
    output$peak_flow_time2_plot = peak_flow_time2_plot
  } 
  
  if (exists('peak_flow_time3_plot')) {
    output$peak_flow_time3_plot = peak_flow_time3_plot
  } 
  
  if (exists('APE_full_plot')) {
    output$APE_full_plot = APE_full_plot
  } 
  
  if (exists('APE_time1_plot')) {
    output$APE_time1_plot = APE_time1_plot
  } 
  
  if (exists('APE_time2_plot')) {
    output$APE_time2_plot = APE_time2_plot
  } 
  
  if (exists('APE_time3_plot')) {
    output$APE_time3_plot = APE_time3_plot
  } 
  
 
  print(output)
  
}


  



