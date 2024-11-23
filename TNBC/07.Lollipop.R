# Load necessary libraries
library(dplyr)      
library(tidyr)     
library(ggplot2)    
library(grid)   

# Data Preparation -------------------------------------------------------------------

counted <- dismal1K@meta.data %>%
  group_by(fov_id, celltype) %>%
  summarise(count = n(), .groups = 'drop')  

wide_format <- counted %>%
  pivot_wider(names_from = celltype, values_from = count, values_fill = 0)

patient_summary <- dismal1K@meta.data %>%
  group_by(fov_id) %>%
  summarise(
    mac_spp1_ratio = sum(major_type == "Macrophages" & minor_type == "Mac_SPP1+") / sum(major_type == "Macrophages"),
    mac_cxcl9_ratio = sum(major_type == "Macrophages" & minor_type == "Mac_CXCL9+") / sum(major_type == "Macrophages")
  ) %>%
  na.omit()  

wide_format <- left_join(wide_format, patient_summary, by = "fov_id") %>%
  relocate(c("fov_id", 'mac_cxcl9_ratio', 'mac_spp1_ratio'))  


df <- wide_format

count_columns <- celltype_order 

# Function Definition ----------------------------------------------------------------

# Define a function to perform correlation analysis and generate lollipop plots
perform_correlation_analysis <- function(data, count_cols, target_col, exclude_celltype, 
                                         plot_title, ylim_vals, output_file) {
  
  correlation_results <- data.frame(
    Variable = character(),
    Pearson_Rho = numeric(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (col in count_cols) {
    cor_test <- cor.test(data[[col]], data[[target_col]], method = "pearson")
      correlation_results <- rbind(correlation_results, data.frame(
      Variable = col,
      Pearson_Rho = cor_test$estimate,
      P_Value = cor_test$p.value
    ))
  }
  correlation_results <- correlation_results %>%
    rename(x = Variable, y = Pearson_Rho, p_value = P_Value) %>%
    filter(x != exclude_celltype) %>%
    mutate(x = factor(x))
  
  # Adjust p-values for multiple testing using Benjamini-Hochberg method
  correlation_results <- correlation_results %>%
    mutate(
      adjusted_p_value_bh = p.adjust(p_value, method = "BH"),
      threshold = ifelse(adjusted_p_value_bh < 0.05, 'Sig', 'Nonsig'),
      threshold = factor(threshold, levels = c("Sig", 'Nonsig'))
    )
  
  # Arrange the results by the correlation coefficient
  correlation_results <- correlation_results %>%
    arrange(y) %>%
    mutate(x_reordered = factor(x, levels = unique(x)))
  
  # Format adjusted p-values 
  correlation_results <- correlation_results %>%
    mutate(
      adjusted_p_value_bh2 = ifelse(
        adjusted_p_value_bh < 0.001,
        '< 0.001',
        round(adjusted_p_value_bh, 3)
      )
    )
  
  # Generate a lollipop plot for the correlations
  cor_lollipop <- correlation_results %>% 
    ggplot(aes(x = x_reordered, y = y)) +
    geom_segment(aes(xend = x_reordered, y = 0, yend = y, color = threshold), size = 1) +  
    geom_point(size = 4.5, pch = 21, aes(fill = x_reordered, color = x_reordered)) +     
    scale_fill_manual(values = all_cols) +                                               
    scale_color_manual(values = c(Sig = "gray", Nonsig = 'gray', all_cols)) +            
    ylab("Pearson Correlation Coefficient") +
    xlab("") +
    coord_flip(ylim = ylim_vals) +                                                       
    geom_text(aes(x = x_reordered, y = ylim_vals[2] * 0.9, label = adjusted_p_value_bh2), 
              hjust = -0.1, 
              color = ifelse(correlation_results$threshold == "Sig", "#E27A7F", "black"), 
              size = 3) +                                                               
    geom_hline(yintercept = c(-0.2, 0, 0.2), linetype = "dotted", color = 'darkgray') +  
    scale_y_continuous(breaks = seq(ylim_vals[1], ylim_vals[2], by = 0.2), 
                       expand = expansion(mult = c(0, .1))) +                        
    theme_classic() +
    theme(
      legend.position = "none",                                                        
      axis.text.y = element_text(color = "black"),                                     
      panel.grid = element_blank(),                                                   
      plot.margin = margin(20, 0, 0, 0)                                               
    )
  
  print(cor_lollipop)
  
  pdf(paste0(output_file), width = 5, height = 4.5)
  print(cor_lollipop)
  grid.text(bquote(italic(P)[adj]), x = 0.9, y = 0.95, gp = gpar(fontsize = 10, fontface = "bold"))
  dev.off()
  
  correlation_results$type <- plot_title
  
  return(correlation_results)
}

# Correlation Analysis and Plotting for Mac_CXCL9+ -----------------------------------

correlation_results_cxcl9 <- perform_correlation_analysis(
  data = df,
  count_cols = count_columns,
  target_col = 'mac_cxcl9_ratio',
  exclude_celltype = "Mac_CXCL9+",
  plot_title = 'Mac_CXCL9+',
  ylim_vals = c(-0.1, 0.7),
  output_file = paste0(output_folder, "/Mac_CXCL9_Lollipop.pdf")
)

# Correlation Analysis and Plotting for Mac_SPP1+ -------------------------------------

correlation_results_spp1 <- perform_correlation_analysis(
  data = df,
  count_cols = count_columns,
  target_col = 'mac_spp1_ratio',
  exclude_celltype = "Mac_SPP1+",
  plot_title = 'Mac_SPP1+',
  ylim_vals = c(-0.4, 0.4),
  output_file = paste0(output_folder, "/Mac_SPP1_Lollipop.pdf")
)

