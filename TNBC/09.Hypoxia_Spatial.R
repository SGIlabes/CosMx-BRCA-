# ---------------------------
# 1. Library Loading
# ---------------------------

# Load necessary libraries
library(dplyr)          
library(ggplot2)         
library(ggnewscale)      
library(RColorBrewer)   
library(scales)         

# Define custom infix operator for "not in"
`%nin%` <- Negate(`%in%`)

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

# Load dismal1K data
dismal1K <- readRDS(file.path(object_folder, 'Dismal1K_all.Rds'))
meta <- dismal1K@meta.data
rownames(meta) <- NULL

# Load dismal1K cancer cells data
dismal1K_cancer <- readRDS(file.path(object_folder, 'Dismal1K_cancer_cells.Rds'))
meta_cancer <- dismal1K_cancer@meta.data
rownames(meta_cancer) <- NULL

# Select relevant columns (scores starting with 'score_')
score_columns <- grep("^score_", names(meta_cancer), value = TRUE)
meta_cancer <- meta_cancer[, c("row_id", score_columns)]

# Merge meta and meta_cancer on 'row_id'
meta_all <- left_join(meta, meta_cancer, by = "row_id")

# Calculate x and y coordinates (scaled by 0.12)
meta_all <- meta_all %>%
  mutate(
    x = CenterX_local_px * 0.12,
    y = CenterY_local_px * 0.12
  )

# Assign 'Cell type' based on major_type and minor_type
meta_all <- meta_all %>%
  mutate(
    `Cell type` = case_when(
      major_type == "Cancer" ~ as.character(score_MP6_Hypoxia_smoothed),
      major_type == "T cells CD8+" ~ "T cells CD8+",
      minor_type == "Mac_SPP1+" ~ "Mac_SPP1+",
      TRUE ~ "Others"
    )
  )

# Convert 'Cell type' to a factor with specified levels
meta_all$`Cell type` <- factor(meta_all$`Cell type`, levels = c("T cells CD8+", "Mac_SPP1+", "Others"))

# ---------------------------
# 4. Color Palette Setup
# ---------------------------

# Define color palettes using RColorBrewer
my_colors <- brewer.pal(n = 11, name = "Spectral")
my_colors <- rev(my_colors)

# Define custom color gradient for hypoxia scores
colors <- colorRampPalette(c('#709AE1', '#FFFFFF', '#FD7446'))(100)

# Function to add transparency to colors
add_alpha <- function(colors, alpha = 1) {
  apply(sapply(colors, col2rgb)/255, 2, 
        function(x) rgb(x[1], x[2], x[3], alpha = alpha))
}

# Define color palette with transparency for hypoxia
distiller_colors <- add_alpha(rev(brewer.pal(11, "Spectral")), 0.7)

# ---------------------------
# 5. Data Subsetting for Plotting
# ---------------------------

# Filter data for a specific Field of View (FOV)
meta_plot <- meta_all %>%
  filter(fov_id == "Dismal03_1_3_FOV24")

# ---------------------------
# 6. Visualization: Hypoxia Score and Cell Types
# ---------------------------

# Create the plot
p3 <- ggplot() +
  # Plot Cancer cells colored by hypoxia score
  geom_point(
    data = subset(meta_plot, major_type == "Cancer"),
    aes(x = x, y = y, color = `score_MP6_Hypoxia_smoothed`),
    size = 1
  ) +
  # Define color gradient for hypoxia
  scale_color_gradientn(
    colors = distiller_colors,
    limits = c(min(meta_plot$`score_MP6_Hypoxia_smoothed`, na.rm = TRUE),
               max(meta_plot$`score_MP6_Hypoxia_smoothed`, na.rm = TRUE)),
    oob = scales::squish
  ) +
  labs(x = "", y = "", color = "Hypoxia") +
  # Add new color scale for non-Cancer cells
  new_scale_color() +
  # Plot non-Cancer cells colored by Cell type
  geom_point(
    data = subset(meta_plot, major_type != "Cancer"),
    aes(x = x, y = y, color = `Cell type`, size = ifelse(`Cell type` %in% c("T cells CD8+", "Mac_SPP1+"), 3.5, 1)),
    shape = 3
  ) +
  # Define manual color scale for Cell type
  scale_color_manual(values = all_cols) +
  # Remove legend for size
  guides(size = "none") +
  # Define size range
  scale_size_continuous(range = c(1, 3.5)) +
  # Theme settings
  theme_bw() +
  theme(
    legend.position = "none"
  )

# Save the plot as PDF
pdf(file.path(output_folder, "Dismal03_1_3_FOV24_Hypoxia_Cd8Tcell.pdf"), width = 5, height = 3.5)
print(p3)
dev.off()