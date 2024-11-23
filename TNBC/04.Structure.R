# Load necessary libraries
library(dplyr)              
library(tidyr)               
library(Seurat)              
library(SingleCellExperiment) 
library(SpatialExperiment)   
library(SPIAT)              
library(reshape2)         
library(pheatmap)         
library(ggplot2)         
library(ggspavis)            
library(RColorBrewer)        
library(gridExtra)          
library(grid)                

# ---------------------------
# 1. SpatialExperiment (SPE) Preparation
# ---------------------------

# Convert major_type and minor_type to character
dismal1K@meta.data$major_type <- as.character(dismal1K@meta.data$major_type)
dismal1K@meta.data$minor_type <- as.character(dismal1K@meta.data$minor_type)

# Create a new overall_type column
dismal1K@meta.data <- dismal1K@meta.data %>%
  mutate(overall_type = ifelse(major_type %in% c("Cancer", "Normal Epithelial", "Myoepithelial"),
                               "Epithelial", major_type))

dismal1K@meta.data$major_type <- factor(dismal1K@meta.data$major_type, levels = major_order)

# Convert Seurat object to SingleCellExperiment
sce <- Seurat::as.SingleCellExperiment(dismal1K)

# Extract and adjust metadata
df <- dismal1K@meta.data
df$CenterX_local_px <- df$CenterX_local_px * 0.12
df$CenterY_local_px <- df$CenterY_local_px * 0.12

# Create spatial coordinates matrix
spatialCoords <- as.matrix(df[, c("CenterX_local_px", "CenterY_local_px")])
colnames(spatialCoords) <- c("Cell.X.Position", "Cell.Y.Position")

# Create SpatialExperiment object
spe <- SpatialExperiment(
  assays = assays(sce),
  rowData = rowData(sce),
  colData = colData(sce),
  metadata = metadata(sce),
  reducedDims = reducedDims(sce),
  altExps = altExps(sce),
  sample_id = 'dismal1K',
  spatialCoords = spatialCoords
)

# Add additional metadata
spe$in_tissue <- 1
spe$data_id <- 'dismal1K'

# Get unique field of view (fov) IDs
fov_ids <- unique(df$fov_id)

# Define cell types for analysis
cell_interest <- setdiff(unique(dismal1K$overall_type), 'Epithelial') # Cells of interest excluding 'Epithelial'

# ---------------------------
# 2. Structure Analysis and Plotting
# ---------------------------

# Define color palette
pal <- brewer.pal(n = 9, name = "Set1")

# Function to identify structures and generate plots for a given fov_id
process_structure <- function(spe_object, cell_interest, pal, all_cols) {
  # Identify bordering cells
  formatted_border <- identify_bordering_cells2(
    spe_object, 
    reference_cell = 'Epithelial',
    feature_colname = "overall_type"
  )
  
  # Calculate distance to margin
  formatted_distance <- calculate_distance_to_margin(formatted_border)
  
  # Define structure based on distance
  formatted_structure <- define_structure(
    formatted_distance, 
    cell_types_of_interest = cell_interest, 
    feature_colname = "overall_type", 
    n_margin_layers = 1
  )
  
  # Categorize structures
  formatted_structure$Structure2 <- ifelse(
    formatted_structure$Structure %in% c("Outside", "Stromal.CoI"), 
    'Stromal', 
    'Intratumoral'
  )
  
  return(formatted_structure)
}

# Initialize lists to store results
area_sum <- data.frame()
plots_list <- list()

# Loop through each fov_id to perform structure analysis and generate plots
for (id in fov_ids) {
  spe_tmp <- spe[, spe$fov_id == id]
  spe_tmp$sample_id <- id 
  
  # Process structure
  formatted_structure <- process_structure(spe_tmp, cell_interest, pal, all_cols)
  
  # Generate plots
  plot_structure2 <- ggspavis::plotSpots(
    formatted_structure, 
    in_tissue = NULL, 
    annotate = 'Structure2', 
    y_reverse = FALSE, 
    point_size = 3, 
    pal = pal
  ) +
    ggtitle(id) +
    labs(color = "") +
    scale_color_manual(values = c('#ffb359', "#0f993d")) + 
    theme(
      title = element_blank(),
      legend.text = element_text(size = 12),
      legend.position = "bottom",     
      legend.direction = "horizontal"
    )
  
  plot_major_type <- ggspavis::plotSpots(
    formatted_structure, 
    in_tissue = NULL, 
    annotate = 'major_type', 
    y_reverse = FALSE, 
    point_size = 3, 
    pal = all_cols
  ) +
    ggtitle(id) +
    labs(color = "")
  
  # Append plots to the list
  plots_list[[paste0("Structure2_", id)]] <- plot_structure2
  plots_list[[paste0("MajorType_", id)]] <- plot_major_type
  
  # Extract and store area information
  area <- attr(formatted_structure, "area")
  area <- as.data.frame(area)
  area$id <- id
  area_sum <- bind_rows(area_sum, area)
  
  # Assign to a temporary variable for specific plotting
  assign(paste0("formatted_structure_tmp", id), formatted_structure)
}

# ---------------------------
# 3. Save All Structure Plots to PDF
# ---------------------------

# Function to save multiple plots into a PDF
save_plots_to_pdf <- function(plots, output_path, plots_per_page = 6, ncol = 2) {
  pdf(output_path, onefile = TRUE, width = 12, height = 18)
  
  num_plots <- length(plots)
  num_pages <- ceiling(num_plots / plots_per_page)
  
  for (i in 1:num_pages) {
    start_index <- (i - 1) * plots_per_page + 1
    end_index <- min(i * plots_per_page, num_plots)
    plots_to_print <- plots[start_index:end_index]
    do.call(grid.arrange, c(plots_to_print, ncol = ncol))
  }
  
  dev.off()
}

# Save all structure plots
save_plots_to_pdf(
  plots = plots_list, 
  output_path = paste0(output_folder, "/Dismal1K_Structure_output_plots.pdf")
)

# ---------------------------
# 4. Save Representative Structure Plot
# ---------------------------

# Define the ID for the representative plot
representative_id <- "Dismal01_1_3_FOV28" # Replace with your specific ID

# Generate and save the representative plot
rep_plot <- ggspavis::plotSpots(
  get(paste0("formatted_structure_tmp", representative_id)), 
  in_tissue = NULL, 
  annotate = 'Structure2', 
  y_reverse = FALSE, 
  point_size = 1, 
  pal = pal
) +
  ggtitle(representative_id) +
  labs(color = "") +
  scale_color_manual(values = c('#ffb359', "#0f993d")) +
  theme(
    title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "bottom",     
    legend.direction = "horizontal"
  )

# Save the representative plot to PDF
pdf(paste0(output_folder, "/Dismal1K_structure_", representative_id, ".pdf"), width = 4, height = 4.3)
print(rep_plot)
dev.off()

# ---------------------------
# 5. Merge Structure Data into Metadata
# ---------------------------

# Function to extract and combine structure data from formatted_structure objects
extract_structure_data <- function(fov_ids) {
  processed_data <- lapply(fov_ids, function(id) {
    formatted_structure <- get(paste0("formatted_structure_tmp", id))
    data <- as.data.frame(formatted_structure@colData@listData)
    data <- data[, c("id", "Region", "region2", "Distance.To.Border", "Structure", "Structure2")]
    return(data)
  })
  
  combined_data <- bind_rows(processed_data)
  return(combined_data)
}

# Extract and combine structure data
combined_structure_data <- extract_structure_data(fov_ids)

# Merge combined structure data into Seurat metadata
dismal1K@meta.data <- dismal1K@meta.data %>%
  left_join(combined_structure_data, by = "id")

# Ensure rownames are correctly set
rownames(dismal1K@meta.data) <- dismal1K@meta.data$row_id

# ---------------------------
# 6. Myeloid Distribution Analysis
# ---------------------------

# Summarize colocalization ratios for myeloid cells
data_summary <- dismal1K@meta.data %>%
  filter(major_type %in% c('Macrophages', 'Monocytes', 'DCs', 'Neutrophils')) %>%
  group_by(minor_type, fov_id) %>%
  summarise(
    tumor = sum(Structure2 == "Intratumoral"),
    stromal = sum(Structure2 == "Stromal"),
    ratio = ifelse(stromal > 0, tumor / stromal, NA)  # Prevent division by zero
  ) %>%
  na.omit()

# Convert minor_type to a factor with predefined levels
data_summary$minor_type <- factor(data_summary$minor_type, levels = m_level)

# Generate boxplot for intratumoral/stromal ratio
p1 <- data_summary %>% 
  ggplot(aes(x = minor_type, y = ratio, fill = minor_type)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = all_cols) +
  coord_flip(ylim = c(0, 10)) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.title = element_text(face = "bold"),
    legend.position = 'none',
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    fill = "",
    x = "",
    y = "Intratumoral/Stromal ratio"
  )

print(p1)

# Save the boxplot to PDF
pdf(paste0(output_folder, "/Dismal1K_Myeloid_Distribution_Structure.pdf"), width = 3.5, height = 2.5)
print(p1)
dev.off()
