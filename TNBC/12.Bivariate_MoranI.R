# ---------------------------
# 1. Library Loading
# ---------------------------

# Load necessary libraries
library(tidyverse)
library(ggpubr)
library(patchwork)
library(sf)
library(scales)
library(ggnewscale)
library(spdep)      
library(boot)     
library(parallel)   

# ---------------------------
# 2. Directory Setup
# ---------------------------

object_folder <- '/path/object/'
output_folder <- '/path/output'

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# ---------------------------
# 3. Data Loading and Preparation
# ---------------------------

# Load Seurat object
dismal1K <- readRDS(file.path(object_folder, 'Dismal1K_all.Rds'))

# Extract gene counts and metadata
genes <- dismal1K[["SCT"]]$counts
meta <- dismal1K@meta.data %>%
  mutate(
    unique_fov = fov_id,
    cell_id = rownames(.)
  )

# Load gene sets
cd8_gene <- read_csv('/path/gene/cd8_cxcl_ccl.csv')

# ---------------------------
# 4. Bivariate Spatial Autocorrelation (Moran's I) Computation
# ---------------------------

# Parameters
nearest_neighbors <- 5
fovs <- unique(meta$unique_fov)

# Compute Moran's I for each FOV using parallel processing
morans_list2 <- mclapply(setNames(fovs, fovs), function(fov) {
  
  # Subset metadata and gene expression for the current FOV
  fov_meta <- meta %>% filter(unique_fov == fov)
  fov_expression <- genes[, colnames(genes) %in% fov_meta$cell_id] %>%
    t() %>% 
    as.data.frame(check.names = FALSE) %>%
    rownames_to_column("cell_id") %>%
    full_join(fov_meta %>% select(cell_id, CenterX_local_px, CenterY_local_px), by = "cell_id")
  
  # Convert to spatial object
  spdf <- st_as_sf(fov_expression, coords = c("CenterX_local_px", "CenterY_local_px"))
  
  # Find k-nearest neighbors
  knn <- knearneigh(st_coordinates(spdf), k = nearest_neighbors)
  knn_nb <- knn2nb(knn)
  
  # Create spatial weights
  knn_listw <- nb2listw(knn_nb, style = "B")
  
  # Calculate bivariate Moran's I for each ligand-receptor pair
  morans_I_results <- lapply(cd3_gene$lr_pair, function(pair) {
    
    # Extract ligand and receptor genes
    ligand <- cd8_gene %>% filter(lr_pair == pair) %>% pull(ligand_gene_symbol)
    receptor <- cd8_gene %>% filter(lr_pair == pair) %>% pull(receptor_gene_symbol)
    
    # Check if genes exist in the dataset
    if (!(ligand %in% colnames(spdf)) || !(receptor %in% colnames(spdf))) {
      return(data.frame(
        original = NA,
        lr_pair = pair,
        ci_low = NA,
        ci_high = NA
      ))
    }
    
    # Calculate bivariate Moran's I
    moran_I <- moran_bv(spdf[[ligand]], spdf[[receptor]], knn_listw, nsim = 500)
    
    # Calculate bootstrap confidence interval
    moran_ci <- boot.ci(moran_I, conf = 0.95, type = 'basic')
    
    # Return results
    return(data.frame(
      original = moran_I$t0,      # Observed Moran's I
      lr_pair = pair,             # Ligand-receptor pair
      ci_low = moran_ci$basic[4], # Lower confidence interval
      ci_high = moran_ci$basic[5] # Upper confidence interval
    ))
  })
  
  # Combine results and add FOV identifier
  moran_df <- bind_rows(morans_I_results) %>%
    mutate(unique_fov = fov)
  
  return(moran_df)
  
}, mc.cores = detectCores() - 1) # Utilize available cores minus one


# ---------------------------
# 5. Combining Moran's I Results and Statistical Analysis
# ---------------------------

# Combine all Moran's I results into a single dataframe
morans_df2 <- bind_rows(morans_list2) %>%
  arrange(desc(original))

# Merge with DFS metadata
df2 <- meta %>%
  select(fov_id, DFS) %>%
  distinct()

morans_df2 <- left_join(morans_df2, df2, by = c('unique_fov' = "fov_id"))



# ---------------------------
# 6. Moran's I Boxplot
# ---------------------------

# Prepare data for boxplot
cd8A_pairs <- morans_df2 %>%
  filter(grepl('CD8A_', lr_pair)) %>%
  mutate(
    gene = sapply(strsplit(as.character(lr_pair), "_"), function(x) tail(x, n = 1))
  ) %>%
  mutate(
    cat = ifelse(gene %in% cd8_gene$gene, "positive", "negative")
  )

# Calculate mean Moran's I per gene
cd8a_means <- cd8A_pairs %>%
  group_by(gene) %>%
  summarize(mean_original = mean(original, na.rm = TRUE)) %>%
  arrange(desc(mean_original)) %>%
  mutate(
    order = row_number(),
    cat = ifelse(mean_original >= 0, "positive", "negative")
  )

# Define ordered variables
ordered_variables <- cd8a_means$gene

# Boxplot for Moran's I
moran_fig_plot <- ggplot(cd8A_pairs, aes(x = factor(gene, levels = ordered_variables), y = original, fill = cat)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
  scale_fill_manual(values = c(positive = '#1262b3', negative = "#cc3d3d")) +
  labs(x = 'Chemokines', y = 'Bivariate Moran’s I with CD8A', title = '') +
  theme_bw() +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

# Save Moran's I boxplot
ggsave(
  filename = file.path(output_folder, "Moran_with_CD8A_Chemokine.pdf"),
  plot = moran_fig_plot,
  width = 5,
  height = 3.5
)

# ---------------------------
# 7. Spatial Moran Concept Overlap
# ---------------------------


# Select a specific FOV for plotting
meta_plot <- meta %>%
  filter(fov_id == "Dismal01_1_4_FOV41")

# Define plotting parameters
max_value <- 5
breaks <- c(1, 3, 5) # Define breaks for color scales

# Define color gradients
my_blues <- c("lightgray", "#0073AB", "#002453") # Blue gradient
my_reds <- c("lightgray", "#FF6666", "#CC0000")   # Red gradient
my_green <- c("lightgray", "#007A56", "#004224") # Green gradient


# Prepare combined data for plotting
combined_data <- genes[, colnames(genes) %in% meta_plot$cell_id] %>%
  t() %>%
  as.data.frame(check.names = FALSE) %>%
  rownames_to_column("cell_id") %>%
  full_join(meta_plot %>% select(cell_id, CenterX_local_px, CenterY_local_px), by = "cell_id") %>%
  mutate(
    CD8A_scaled = rescale(CD8A, to = c(0, 1)),
    CXCL17_scaled = rescale(CXCL17, to = c(0, 1)),
    CXCL9_scaled = rescale(CXCL9, to = c(0, 1)),
    CCL5_scaled = rescale(CCL5, to = c(0, 1)),
    combined_color2 = case_when(
      CD8A_scaled > 0 ~ "CD8A",
      CCL5_scaled > 0 ~ "CCL5",
      CXCL9_scaled > 0 ~ "CXCL9",
      CXCL17_scaled > 0 ~ "CXCL17",
      TRUE ~ "none"
    )
  )


# Plot for CD8A and CXCL9
combined_plot1 <- combined_data %>%
  ggplot() + 
  geom_point(data = combined_data %>% filter(combined_color2 == "none"),
             aes(x = CenterX_local_px * 0.12, y = CenterY_local_px * 0.12),
             color = "lightgray", alpha = 0.5, size = 1) +
  geom_point(data = combined_data %>% filter(combined_color2 == "CD8A"),
             aes(x = CenterX_local_px * 0.12, y = CenterY_local_px * 0.12, color = CD8A_scaled),
             size = 1.5) +
  scale_color_gradientn(colors = my_blues, limits = c(0, 1), breaks = breaks, oob = scales::squish,
                        name = "CD8A", guide = guide_colorbar(order = 1)) +
  new_scale_color() +
  geom_point(data = combined_data %>% filter(combined_color2 == "CXCL9"),
             aes(x = CenterX_local_px * 0.12, y = CenterY_local_px * 0.12, color = CXCL9_scaled),
             size = 1.5) +
  scale_color_gradientn(colors = my_reds, limits = c(0, 1), breaks = breaks, oob = scales::squish,
                        name = "CXCL9", guide = guide_colorbar(order = 2)) +
  coord_equal() + 
  labs(x = "", y = "", title = "Bivariate Moran's I = 0.34") +
  theme_bw() +
  theme(legend.position = "right") 

# Save Moran's I CD8A and CXCL9 plot
ggsave(
  filename = file.path(output_folder, "Concept_of_Spatial_Moran_CD8A_CXCL9.pdf"),
  plot = combined_plot1,
  width = 4.5,
  height = 4
)

# Plot for CD8A and CXCL17
combined_plot2 <- combined_data %>%
  ggplot() + 
  geom_point(data = combined_data %>% filter(combined_color2 == "none"),
             aes(x = CenterX_local_px * 0.12, y = CenterY_local_px * 0.12),
             color = "lightgray", alpha = 0.5, size = 1) +
  geom_point(data = combined_data %>% filter(combined_color2 == "CD8A"),
             aes(x = CenterX_local_px * 0.12, y = CenterY_local_px * 0.12, color = CD8A_scaled),
             size = 1.5) +
  scale_color_gradientn(colors = my_blues, limits = c(0, 1), breaks = breaks, oob = scales::squish,
                        name = "CD8A", guide = guide_colorbar(order = 1)) +
  new_scale_color() +
  geom_point(data = combined_data %>% filter(combined_color2 == "CXCL17"),
             aes(x = CenterX_local_px * 0.12, y = CenterY_local_px * 0.12, color = CXCL17_scaled),
             size = 1.5) +
  scale_color_gradientn(colors = my_green, limits = c(0, 1), breaks = breaks, oob = scales::squish,
                        name = "CXCL17", guide = guide_colorbar(order = 3)) +
  coord_equal() + 
  labs(x = "", y = "", title = "Bivariate Moran's I = -0.06") +
  theme_bw() +
  theme(legend.position = "right") 

# Save Moran's I CD8A and CXCL17 plot
ggsave(
  filename = file.path(output_folder, "Concept_of_Spatial_Moran_CD8A_CXCL17.pdf"),
  plot = combined_plot2,
  width = 4.5,
  height = 4
)

