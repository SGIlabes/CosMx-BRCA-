# ---------------------------
# 1. Library Loading
# ---------------------------

# List of required libraries
required_libraries <- c(
  "Seurat", "Matrix", "ggplot2", "ggpubr", "viridis",
  "harmony", "stringr", "readr", "readxl", "openxlsx", "ggrastr",
  "grid", "gridExtra", "cowplot", "dplyr", "CellChat", "tidyverse",
  "forcats", "broom", "boot", "reshape2", "purrr"
)

# Install any missing libraries and load all libraries
installed_libraries <- rownames(installed.packages())
for (lib in required_libraries) {
  if (!lib %in% installed_libraries) {
    install.packages(lib, dependencies = TRUE)
  }
  library(lib, character.only = TRUE)
}

# ---------------------------
# 2. Directory Setup
# ---------------------------

# Define folder paths
raw_folder <- '/path/raw/'
object_folder <- '/path/object'
output_folder <- '/path/output'

# Create output directory if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# ---------------------------
# 3. Data Loading and Preparation
# ---------------------------

# Load Seurat objects
dismal1K <- readRDS(file.path(object_folder, 'Dismal1K_all.Rds'))
dismal1K_cancer <- readRDS(file.path(object_folder, 'Dismal1K_cancer_cells.Rds'))

# Extract unique metadata (DFS, OS, fov_id)
df2 <- dismal1K@meta.data %>%
  select(DFS, OS, fov_id) %>%
  distinct() %>%
  na.omit()
rownames(df2) <- NULL

# ---------------------------
# 4. Expression Analysis: IFNG and IFNA1/13
# ---------------------------

# Calculate average expression for IFNG and IFNA1/13 grouped by fov_id
df_ifng <- AverageExpression(
  dismal1K, 
  features = c('IFNG', 'IFNA1/13'),  
  group.by = "fov_id", 
  layer = "data", 
  assays = 'SCT'
) %>%
  as.data.frame() %>%
  mutate(Gene = rownames(.)) %>%
  pivot_longer(
    cols = -Gene,  
    names_to = "fov_id",  
    values_to = "value"  
  ) %>%
  mutate(
    fov_id = str_replace_all(fov_id, "Nanostring\\.|SCT\\.", ""),
    fov_id = str_replace_all(fov_id, "\\.", "_")
  ) %>%
  pivot_wider(
    id_cols = fov_id,
    names_from = Gene,
    values_from = value
  )

# Summarize cell counts and ratios per fov_id
data_summary <- dismal1K@meta.data %>%
  group_by(fov_id) %>%
  summarize(
    CXCL9counts = sum(minor_type == "Mac_CXCL9+"),
    SPP1counts = sum(minor_type == "Mac_SPP1+"),
    Total_Mac = sum(major_type == "Macrophages"),
 
    CXCL9_ratio = ifelse(Total_Mac > 0, CXCL9counts / Total_Mac, NA),  # Prevent division by zero
    SPP1_ratio = ifelse(Total_Mac > 0, SPP1counts / Total_Mac, NA),    # Prevent division by zero
  ) %>%
  na.omit()

# Merge expression data with cell counts
df_ifng_wide <- left_join(df_ifng, data_summary, by = 'fov_id') %>%
  rename(IFNA1 = 'IFNA1/13') %>%
  mutate(
    IFNG_log = log10(IFNG)
  )


# ---------------------------
# 5. Correlation Plots
# ---------------------------

# Function to create and save scatter plots with correlation
create_scatter_plot <- function(data, x_var, y_var, y_label, file_name, method = "pearson") {
  plot <- ggscatter(
    data, 
    x = x_var, 
    y = y_var,
    add = "reg.line",        
    add.params = list(color = "#FF3200FF", fill = "lightgray"),
    conf.int = TRUE,
    ylab = y_label,
    xlab = paste0(x_var, " expression")
  ) +
    labs(color = "") +
    stat_cor(
      method = method, 
      p.accuracy = ifelse(method == "pearson", 1e-5, 0.001), 
      digits = 2, 
      cor.coef.name = ifelse(method == "pearson", 'R', 'rho'), 
      label.x.npc = 0.5
    )
  
  ggsave(
    filename = file.path(output_folder, file_name),
    plot = plot,
    width = 4,
    height = 4
  )
}

# Create and save various correlation plots
create_scatter_plot(df_ifng_wide, "IFNG", "CXCL9counts", "Mac_CXCL9+ counts", "Dismal1K_Mac_CXCL9_counts_IFNG.pdf", "pearson")
create_scatter_plot(df_ifng_wide, "IFNA1", "CXCL9counts", "CXCL9counts+ counts", "Dismal1K_Mac_CXCL9_counts_IFNA.pdf", "pearson")

# ---------------------------
# 6. DotPlot Visualization
# ---------------------------


# Celltype DotPlot

# Minor Type DotPlot
dot_data_minor <- DotPlot(
  object = dismal1K, 
  features = c("IFNG", "IFNA1/13", "IFNGR1",'IFNGR2'), 
  group.by = 'minor_type',
  cols = "OrRd",
  dot.scale = 8
) + RotatedAxis()

dot_data_minor_df <- dot_data_minor$data %>%
  mutate(id = factor(id, levels = rev(minor_order)))

dot_ifng_minor <- ggplot(dot_data_minor_df, aes(features.plot, id)) +
  geom_point(aes(color = avg.exp.scaled, size = pct.exp)) +
  scale_color_distiller(
    palette = 'RdBu', 
    direction = -1,
    name = 'Average Expression',
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  scale_size(name = 'Percent Expressed') +
  theme_linedraw() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = file.path(output_folder, "Dotplot_celltype_IFNG_IFNGR_minor.pdf"),
  plot = dot_ifng_minor,
  width = 7,
  height = 5.5
)

# ---------------------------
# 7. Correlation Analysis: CCL and CXCL Genes
# ---------------------------

# Get CCL and CXCL gene names
ccl_cxcl <- c(
  grep("CCL", rownames(dismal1K), value = TRUE),
  grep("CXCL", rownames(dismal1K), value = TRUE)
)

# Calculate average expression for CCL and CXCL genes grouped by fov_id
df_ccl_cxcl <- AverageExpression(
  dismal1K, 
  features = ccl_cxcl,  
  group.by = "fov_id", 
  layer = "data", 
  assays = 'SCT'
) %>%
  as.data.frame() %>%
  mutate(Gene = rownames(.)) %>%
  pivot_longer(
    cols = -Gene,  
    names_to = "fov_id",  
    values_to = "value"  
  ) %>%
  mutate(
    fov_id = str_replace_all(fov_id, "Nanostring\\.|SCT\\.", ""),
    fov_id = str_replace_all(fov_id, "\\.", "_")
  ) %>%
  pivot_wider(
    id_cols = fov_id,
    names_from = Gene,
    values_from = value
  )

# Merge with cell counts and metadata
data_wide <- left_join(df_ccl_cxcl, data_summary, by = 'fov_id') %>%
  left_join(df2, by = "fov_id")

# Define correlation functions
spearman_ci <- function(data, var1, var2) {
  cor.test(data[[var1]], data[[var2]], method = "spearman")
}

# Bootstrapping function for confidence intervals
boot_spearman <- function(data, var1) {
  boot_corr <- function(data, i) {
    cor(data[i, var1], data[i, "CD8Tcell"], method = "spearman")
  }
  boot_obj <- boot(data[, c("CD8Tcell", var1)], boot_corr, R = 1000)
  ci <- boot.ci(boot_obj, type = "bca")
  if (!is.null(ci)) {
    return(ci$bca[c(4,5)])
  } else {
    return(c(NA, NA))
  }
}

# Variables to correlate
variables <- ccl_cxcl

# Initialize results dataframe
results <- data.frame(
  Variable = character(), 
  SpearmanRho = numeric(), 
  CI_low = numeric(), 
  CI_high = numeric(), 
  PValue = numeric(),
  stringsAsFactors = FALSE
)

# Compute Spearman correlations with CD8Tcell
for (var in variables) {
  test_result <- spearman_ci(data_wide, "CD8Tcell", var)
  boot_result <- boot_spearman(data_wide, var)
  
  # Append results
  results <- rbind(results, data.frame(
    Variable = var, 
    SpearmanRho = test_result$estimate, 
    CI_low = boot_result[1], 
    CI_high = boot_result[2], 
    PValue = test_result$p.value,
    stringsAsFactors = FALSE
  ))
}

# Order variables by SpearmanRho descending
results <- results %>% arrange(desc(SpearmanRho))
ordered_variables <- results$Variable

# Create correlation plot
correlation_plot <- ggplot(results, aes(y = Variable, x = SpearmanRho, xmin = CI_low, xmax = CI_high)) +
  geom_vline(xintercept = 0, linetype = "dotted") +  # Dotted line at 0
  geom_vline(xintercept = 0.5, color = "red", linetype = "dotted") +  # Red dotted line at 0.5
  geom_errorbar(aes(y = Variable, xmin = CI_low, xmax = CI_high), width = 0.1) +
  geom_point(color = '#0081CF', size = 3) +
  theme_bw() +
  labs(
    title = "",
    x = "Correlation to CD8+ T cells counts",
    y = ""
  ) +
  scale_x_continuous(breaks = c(-0.4, 0, 0.4, 0.5, 0.8)) +
  theme(
    axis.text.y = element_text(size = 7, face = "italic")
  ) +
  scale_y_discrete(limits = rev(ordered_variables))

# Print the correlation results
print(results)

# Print the plot
print(correlation_plot)

# Save the correlation plot
ggsave(
  filename = file.path(output_folder, "Dismal1K_Cor_CD8T_CCL_from_all_re.pdf"),
  plot = correlation_plot,
  width = 4,
  height = 4
)
