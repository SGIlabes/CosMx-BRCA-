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
library(ggpubr)        
library(grid)          

# 1. Data Preparation ------------------------------------------------------------------

set.seed(1234)

sce <- Seurat::as.SingleCellExperiment(dismal1K)

df <- dismal1K@meta.data
df$CenterX_local_px <- df$CenterX_local_px * 0.12
df$CenterY_local_px <- df$CenterY_local_px * 0.12


spatialCoords <- as.matrix(df[, c("CenterX_local_px", "CenterY_local_px")])
colnames(spatialCoords) <- c("Cell.X.Position", "Cell.Y.Position")

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

spe$in_tissue <- 1
spe$data_id <- 'dismal1K'

fov_id <- unique(df$fov_id)

count_columns <- celltype_order  
target <- setdiff(celltype_order, m_level)  

# 2. Colocalization Analysis -----------------------------------------------------------

cmixing <- data.frame()

# Define radii for mixing score calculations
radii <- c(10, 25, 50, 75, 100)

# Loop through each fov_id to calculate mixing scores at different radii
for (id in fov_id) {
  tryCatch({
    # Subset SpatialExperiment object for the current fov_id
    spe_tmp <- spe[, spe$fov_id == id]
    spe_tmp$sample_id <- id 
    
    # Calculate mixing scores for each radius
    mix_list <- lapply(radii, function(r) {
      mix <- mixing_score_summary(
        spe_object = spe_tmp, 
        reference_celltype = target, 
        target_celltype = m_level, 
        radius = r, 
        feature_colname = "celltype"
      )
      mix$radius <- r
      return(mix)
    })
    
    # Combine mixing scores for all radii
    mix_all <- bind_rows(mix_list)
    mix_all$fov_id <- id
    
    cmixing <- bind_rows(cmixing, mix_all)
    
  }, error = function(e) {
    message("Error occurred for fov_id: ", id)
  })
}

# Merge additional information from metadata
infor <- dismal1K@meta.data %>%
  select(fov_id, DFS, OS, bulk_type) %>%
  distinct()

cmixing <- left_join(cmixing, infor, by = "fov_id")


# 3. Data Refinement ---------------------------------------------------------------------

# Filter mixing scores where the number of reference cells is greater than 0
cmixing2 <- cmixing %>% 
  filter(Number_of_reference_cells > 0)

# Focus on mixing scores at radius 50 and replace NA values with 0
cmixing_r50 <- cmixing2 %>% 
  filter(radius == 50) %>%
  mutate(Normalised_mixing_score = ifelse(is.na(Normalised_mixing_score), 0, Normalised_mixing_score))

# 4. Heatmap Generation -----------------------------------------------------------------

# Reshape data to wide format for heatmap visualization
data_wide <- dcast(cmixing_r50, Reference + Target ~ fov_id, value.var = "Normalised_mixing_score")

# Calculate average colocalization across all fov_ids
data_avg <- data_wide %>%
  rowwise() %>%
  mutate(AverageColocalization = mean(c_across(starts_with("Dismal")), na.rm = TRUE)) %>%
  ungroup() %>%
  select(Reference, Target, AverageColocalization)

# Create a matrix suitable for heatmap
data_matrix <- dcast(data_avg, Target ~ Reference, value.var = "AverageColocalization")
rownames(data_matrix) <- data_matrix$Target
data_matrix <- data_matrix %>%
  select(-Target) %>%
  as.matrix()

data_matrix <- data_matrix[, setdiff(celltype_order, m_level)]

# Generate and save heatmap
heatmap_plot <- pheatmap(
  data_matrix, 
  cluster_rows = TRUE, 
  cluster_cols = TRUE, 
  show_rownames = TRUE, 
  show_colnames = TRUE,
  color = colorRampPalette(c("#1262b3", "white", "#cc3d3d"))(100)
)

pdf(paste0(output_folder, "/Dismal1K_Mac_Target_Normal_Mix.pdf"), width = 5, height = 4)
print(heatmap_plot)
dev.off()

# 5. Boxplot for Cancer Colocalization ---------------------------------------------------

# Function to create and save boxplots for different references
create_boxplot <- function(data, reference, mac_levels, y_label, output_file) {
  plot <- data %>%
    filter(Reference %in% reference, Target %in% mac_levels) %>%
    ggplot(aes(x = Target, y = Normalised_mixing_score, fill = Target)) +
    geom_boxplot(outlier.shape = NA) +
    scale_fill_manual(values = all_cols) +
    coord_cartesian(ylim = c(0, 3)) +
    theme_minimal() + 
    theme(
      axis.line = element_line(),
      axis.title = element_text(face = "bold"),
      legend.position = 'none',
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(size = 10)
    ) +
    labs(
      fill = "",
      x = "",
      y = y_label
    ) +  
    stat_compare_means(
      method = 'kruskal.test',
      label.y = 2.5
    )
  
  # Save plot to PDF
  pdf(output_file, width = 4.5, height = 4)
  print(plot)
  dev.off()
}

# Create and save boxplot for colocalization with Cancer cells
create_boxplot(
  data = cmixing_r50,
  reference = "Cancer",
  mac_levels = mac_level,  
  y_label = "Colocalization with Cancer cells",
  output_file = paste0(output_folder, "/Dismal1K_Colocal_with_Cancer_Mac_spp1_cxcl9_re.pdf")
)

# 6. Boxplot for T cells CD8+ Colocalization -----------------------------------------------

# Create and save boxplot for colocalization with T cells CD8+
create_boxplot(
  data = cmixing_r50,
  reference = "T cells CD8+",
  mac_levels = mac_level,
  y_label = "Colocalization with T cells CD8+",
  output_file = paste0(output_folder, "/Dismal1K_Colocal_with_CD8T_Mac_spp1_cxcl9_re.pdf")
)
