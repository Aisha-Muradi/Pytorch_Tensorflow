library(ggplot2)
library(dplyr)

draw_year_charts <- function(processed) {
  
  lags <- sort(unique(processed$Lag))
  windows <- sort(unique(processed$Window))
  
  # Create the plot
  MAERatePlot <- ggplot(processed, aes(x = Year, y = MAE_Difference_Rate)) +
    facet_grid(Lag ~ Window) + 
    geom_bar(stat = "identity", color = "black", position = position_dodge()) +
    ggtitle(paste0("Difference in Mean Absolute Error for Pytorch - Tensorflow")) +
    ylab("Difference in Mean Absolute Error (MAE) Rate") +
    theme_bw() +
    theme(legend.position = "top", 
          legend.margin = margin(t = 0, unit = "cm"), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(nrow = 1, title = "Model"))
  
  print(MAERatePlot)
  
  MAECountPlot <- ggplot(processed, aes(x = Year, y = MAE_Difference_Count)) +
    facet_grid(Lag ~ Window) + 
    geom_bar(stat = "identity", color = "black", position = position_dodge()) +
    ggtitle(paste0("Difference in Mean Absolute Error for Pytorch - Tensorflow")) +
    ylab("Difference in Mean Absolute Error (MAE) Count") +
    theme_bw() +
    theme(legend.position = "top", 
          legend.margin = margin(t = 0, unit = "cm"), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(nrow = 1, title = "Model"))
  
  print(MAECountPlot)
}
draw_year_charts(processed)
