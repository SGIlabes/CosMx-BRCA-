# Load Required Libraries -------------------------------------------------------------------------
library(Seurat)
library(SingleCellExperiment)
library(SpatialExperiment)
library(SPIAT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggpubr)

# 1. Data Preparation -----------------------------------------------------------------------------

object_folder <- "/path/object"  
output_folder <- "/path/object/output"

# Load the Seurat object
dismal6K <- readRDS(file.path(object_folder, 'Dismal6K_All.RDS'))


# 2. Colocalization Analysis -----------------------------------------------------------------------

Idents(dismal6K) <- 'celltype'

set.seed(1234)

sce <- as.SingleCellExperiment(dismal6K)

df <- dismal6K@meta.data
df$CenterX_local_px <- df$CenterX_local_px * 0.12  
df$CenterY_local_px <- df$CenterY_local_px * 0.12

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
  sample_id = 'dismal6K',
  spatialCoords = spatialCoords
)
spe$in_tissue <- 1
spe$data_id <- 'dismal6K'

# Get unique field of view IDs
fov_id <- unique(dismal6K@meta.data$fov_id)

# Load SPIAT library for spatial analysis
library(SPIAT)

# Define cancer levels and target cell types
cancer_level <- c("Cancer LumA SC", "Cancer LumB SC", "Cancer Her2 SC", "Cancer Basal SC", "Cancer Unassigned")

target <- setdiff(celltype_order, cancer_level)

# Initialize an empty data frame to store mixing scores
cmixing <- data.frame()

# Calculate mixing scores for each field of view
for (id in fov_id) {
  tryCatch({
    spe_tmp <- spe[, spe$fov_id == id]
    spe_tmp$sample_id <- id
    # Calculate mixing scores at different radii
    mix_list <- lapply(c(10, 25, 50, 75, 100), function(radius) {
      mix <- mixing_score_summary(
        spe_object = spe_tmp,
        reference_celltype = cancer_level,
        target_celltype = target,
        radius = radius,
        feature_colname = "celltype"
      )
      mix$radius <- radius
      return(mix)
    })
    mix_all <- do.call(rbind, mix_list)
    mix_all$fov_id <- id
    cmixing <- rbind(cmixing, mix_all)
  }, error = function(e) {
    # Handle the error here (e.g., print a message)
    cat("Error occurred for id:", id, "\n")
  })
}


# 3. Colocalization with T cells CD8+ --------------------------------------------------------------

# Extract relevant metadata information
infor <- dismal6K@meta.data %>%
  select(fov_id, DFS, OS, bulk_type, Subtype_Her2, Subtype_Her2g) %>%
  distinct()


# Merge mixing data with metadata
cmixing <- left_join(cmixing, infor, by = "fov_id")
cmixing2 <- cmixing %>% filter(Number_of_reference_cells > 0)
cmixing2$Reference <- factor(cmixing2$Reference, levels = cancer_level)
cmixing2$Normalised_mixing_score <- ifelse(is.na(cmixing2$Normalised_mixing_score), 0, cmixing2$Normalised_mixing_score)
cmixing_50 <- cmixing2 %>% filter(radius == 50)

# Plot colocalization with T cells CD8+ (overall)
p <- cmixing_50 %>%
  filter(Target %in% c("T cells CD8+")) %>%
  ggplot(aes(x = Reference, y = Normalised_mixing_score, fill = Reference)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 3)) +
  scale_fill_manual(values = all_cols) +  # Ensure 'all_cols' is defined
  labs(x = "", y = 'Colocalization with T cells CD8+', fill = "Group") +
  stat_compare_means(method = 'kruskal.test', label.x.npc = 0.45, label.y.npc = 0.7) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.title = element_text(face = "bold"),
    legend.position = 'none',
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

# Save the plot
pdf(file.path(output_folder, "Dismal6K_Cancer_ref_Tcells_All.pdf"), width = 4, height = 4)
print(p)
dev.off()

# Plot colocalization with T cells CD8+ (by subtype)
p1 <- cmixing_50 %>%
  filter(Target %in% c("T cells CD8+")) %>%
  ggplot(aes(x = Reference, y = Normalised_mixing_score, fill = Reference)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~Subtype_Her2g) +
  coord_cartesian(ylim = c(0, 3)) +
  scale_fill_manual(values = all_cols) +
  labs(x = "", y = 'Colocalization with T cells CD8+', fill = "Group") +
  stat_compare_means(method = 'kruskal.test', label.x.npc = 0.25, label.y.npc = 0.7) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.title = element_text(face = "bold"),
    legend.position = 'none',
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )


# 4. Colocalization Heatmap ------------------------------------------------------------------------

# Exclude normal epithelial cells
rest_epi <- c("Normal Epithelial", "Myoepithelial")
cmixing_50$Reference <- factor(cmixing_50$Reference, levels = rev(cancer_level))
cmixing_50$Target <- factor(cmixing_50$Target, levels = target)
cmixing_50_tme <- cmixing_50 %>% filter(!Target %in% rest_epi)

# Prepare data for heatmap
data_wide <- reshape2::dcast(cmixing_50_tme, Reference + Target ~ fov_id, value.var = "Normalised_mixing_score")
data <- data_wide %>%
  rowwise() %>%
  mutate(AverageColocalization = mean(c_across(-c(Reference, Target)), na.rm = TRUE)) %>%
  ungroup()

data_avg <- data %>%
  select(Reference, Target, AverageColocalization)

# Create matrix for heatmap
data_matrix <- reshape2::dcast(data_avg, Reference ~ Target, value.var = "AverageColocalization")
rownames(data_matrix) <- data_matrix$Reference
data_matrix <- data_matrix[, -1]  

# Convert to numeric matrix
data_matrix <- as.matrix(data_matrix)

# Define cell order
cellorder <- setdiff(celltype_order, c(cancer_level, rest_epi))
data_matrix <- data_matrix[, cellorder]

# Prepare data for ggplot
data_long <- data_avg %>%
  mutate(score = AverageColocalization) %>%
  mutate(
    Reference = factor(Reference, levels = rev(cancer_level)),
    Target = factor(Target, levels = target)
  )

# Create heatmap
h1 <- ggplot(data_long, aes(x = Target, y = Reference, fill = score)) +
  geom_tile(width = 1, height = 1, color = "black") +
  scale_fill_gradientn(
    colours = c('#008837', "white", "#7b3294"),
    guide = "colorbar"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Save the heatmap
pdf(file.path(output_folder, "Dismal6K_All_TME_coloc_All_renew.pdf"), width = 6, height = 4)
print(h1)
dev.off()

# 5. Colocalization Differences --------------------------------------------------------------------
cmixing_50_tme <- cmixing_50_tme %>%
  mutate(Reference_Target = paste(Reference, Target, sep = "_"))

# Initialize results dataframe
results <- data.frame(
  Reference = character(),
  Target = character(),
  Mean_Group1 = numeric(),
  Mean_Group2 = numeric(),
  p_value = numeric(),
  adjusted_p_value = numeric(),
  stringsAsFactors = FALSE
)

# Get unique Reference_Target combinations
unique_combinations <- unique(cmixing_50_tme$Reference_Target)

# Perform t-test for each combination
for (combo in unique_combinations) {
  # Subset data for the current combination
  subset_data <- cmixing_50_tme %>% filter(Reference_Target == combo)
  
  # Check if both groups are present
  if (length(unique(subset_data$DFS)) == 2) {
    # Split data by group
    group1 <- subset_data %>% filter(DFS == "Dismal") %>% pull(Normalised_mixing_score)
    group2 <- subset_data %>% filter(DFS == "Non-dismal") %>% pull(Normalised_mixing_score)
    
    # Perform t-test
    test <- t.test(group1, group2)
    
    # Calculate means
    mean_group1 <- mean(group1, na.rm = TRUE)
    mean_group2 <- mean(group2, na.rm = TRUE)
    
    # Store results
    results <- rbind(results, data.frame(
      Reference = subset_data$Reference[1],
      Target = subset_data$Target[1],
      Mean_Group1 = mean_group1,
      Mean_Group2 = mean_group2,
      p_value = test$p.value,
      adjusted_p_value = NA
    ))
  }
}

# Adjust p-values for multiple testing
results$adjusted_p_value <- p.adjust(results$p_value, method = "BH")

# Determine which group has higher mean
results$high <- ifelse(results$Mean_Group1 > results$Mean_Group2, "G1", "G2")

# Add significance labels
results <- results %>%
  mutate(sig = ifelse(adjusted_p_value <= 0.05, '**', ifelse(p_value <= 0.05, '*', '')))

# Prepare data for heatmap plotting
data_group1 <- cmixing_50_tme %>%
  filter(DFS == "Dismal") %>%
  group_by(Reference, Target) %>%
  summarize(Score_G1 = mean(Normalised_mixing_score, na.rm = TRUE))

data_group2 <- cmixing_50_tme %>%
  filter(DFS == "Non-dismal") %>%
  group_by(Reference, Target) %>%
  summarize(Score_G2 = mean(Normalised_mixing_score, na.rm = TRUE))

# Merge the data by Reference and Target
merged_data <- merge(data_group1, data_group2, by = c("Reference", "Target"), all = TRUE)
merged_data[is.na(merged_data)] <- 0

merged_data$Reference_num <- as.numeric(factor(merged_data$Reference))
merged_data$Target_num <- as.numeric(factor(merged_data$Target))

# Convert data to long format for ggplot2
long_data_g1 <- merged_data %>%
  select(Reference_num, Target_num, Score_G1, Reference, Target) %>%
  mutate(group = "G1", Reference_num = Reference_num + 0.25, score = Score_G1)

long_data_g2 <- merged_data %>%
  select(Reference_num, Target_num, Score_G2, Reference, Target) %>%
  mutate(group = "G2", Reference_num = Reference_num - 0.25, score = Score_G2)

long_data <- bind_rows(long_data_g1, long_data_g2)

# Merge with results
long_data <- left_join(long_data, results, by = c("Reference", "Target"))

# Remove significance where group does not match higher mean
long_data <- long_data %>%
  mutate(sig = ifelse(high != group, "", sig))

# Define group labels
long_data$group <- factor(long_data$group, levels = c("G1", "G2"), labels = c("Dismal", "Non-dismal"))


# Plot heatmap with differences
h1 <- ggplot(long_data, aes(x = Target_num, y = Reference_num, fill = score, color = group)) +
  geom_tile(width = 1, height = 0.5, linewidth = 0.2) +
  scale_fill_gradientn(
    colours = c('#008837', "white", "#7b3294"),
    guide = "colorbar"
  ) +
  scale_color_manual(
    values = response_colors,
    guide = guide_legend(override.aes = list(fill = NA, size = 1.5, linetype = 1))
  ) +
  scale_x_continuous(
    breaks = unique(merged_data$Target_num),
    labels = unique(merged_data$Target),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = unique(merged_data$Reference_num),
    labels = unique(merged_data$Reference),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Add significance labels
h2 <- h1 + geom_text(aes(label = sig), color = "black", size = 3, vjust = 0.5)

# Save the plot
pdf(file.path(output_folder, "Dismal6K_All_TME_coloc_All_diff_renew.pdf"), width = 6, height = 4)
print(h2)
dev.off()


# 5. Subgroup Colocalization Analysis for HER2+ Patients -------------------------------------------

# Filter data for HER2+ subgroup
cmixing_her2 <- cmixing_50_tme %>% filter(Subtype_Her2g == "HER2+")

# Add a combined column for Reference and Target
cmixing_her2 <- cmixing_her2 %>%
  mutate(Reference_Target = paste(Reference, Target, sep = "_"))

# Initialize results dataframe for HER2+ subgroup
results_her2 <- data.frame(
  Reference = character(),
  Target = character(),
  Mean_Dismal = numeric(),
  Mean_NonDismal = numeric(),
  p_value = numeric(),
  adjusted_p_value = numeric(),
  stringsAsFactors = FALSE
)

# Get unique Reference_Target combinations
unique_combinations_her2 <- unique(cmixing_her2$Reference_Target)

# Perform t-test for each combination in HER2+ subgroup
for (combo in unique_combinations_her2) {
  # Subset data for the current combination
  subset_data <- cmixing_her2 %>% filter(Reference_Target == combo)
  
  # Check if both groups are present
  if (length(unique(subset_data$DFS)) == 2) {
    # Split data by group
    group1 <- subset_data %>% filter(DFS == "Dismal") %>% pull(Normalised_mixing_score)
    group2 <- subset_data %>% filter(DFS == "Non-dismal") %>% pull(Normalised_mixing_score)
    
    # Perform t-test
    test <- t.test(group1, group2)
    
    # Calculate means
    mean_group1 <- mean(group1, na.rm = TRUE)
    mean_group2 <- mean(group2, na.rm = TRUE)
    
    # Store results
    results_her2 <- rbind(results_her2, data.frame(
      Reference = subset_data$Reference[1],
      Target = subset_data$Target[1],
      Mean_Dismal = mean_group1,
      Mean_NonDismal = mean_group2,
      p_value = test$p.value,
      adjusted_p_value = NA
    ))
  }
}

# Adjust p-values for multiple testing
results_her2$adjusted_p_value <- p.adjust(results_her2$p_value, method = "BH")

# Determine which group has higher mean
results_her2$high <- ifelse(results_her2$Mean_Dismal > results_her2$Mean_NonDismal, "Dismal", "Non-dismal")

# Add significance labels
results_her2 <- results_her2 %>%
  mutate(sig = ifelse(adjusted_p_value <= 0.05, '**', ifelse(p_value <= 0.05, '*', '')))

# Prepare data for heatmap plotting
data_group1_her2 <- cmixing_her2 %>%
  filter(DFS == "Dismal") %>%
  group_by(Reference, Target) %>%
  summarize(Score_Dismal = mean(Normalised_mixing_score, na.rm = TRUE))

data_group2_her2 <- cmixing_her2 %>%
  filter(DFS == "Non-dismal") %>%
  group_by(Reference, Target) %>%
  summarize(Score_NonDismal = mean(Normalised_mixing_score, na.rm = TRUE))

# Merge the data by Reference and Target
merged_data_her2 <- merge(data_group1_her2, data_group2_her2, by = c("Reference", "Target"), all = TRUE)
merged_data_her2[is.na(merged_data_her2)] <- 0

# Add numeric labels for positioning
merged_data_her2$Reference_num <- as.numeric(factor(merged_data_her2$Reference))
merged_data_her2$Target_num <- as.numeric(factor(merged_data_her2$Target))

# Convert data to long format for ggplot2
long_data_dismal_her2 <- merged_data_her2 %>%
  select(Reference_num, Target_num, Score_Dismal, Reference, Target) %>%
  mutate(group = "Dismal", Reference_num = Reference_num + 0.25, score = Score_Dismal)

long_data_nondismal_her2 <- merged_data_her2 %>%
  select(Reference_num, Target_num, Score_NonDismal, Reference, Target) %>%
  mutate(group = "Non-dismal", Reference_num = Reference_num - 0.25, score = Score_NonDismal)

long_data_her2 <- bind_rows(long_data_dismal_her2, long_data_nondismal_her2)

# Merge with results
long_data_her2 <- left_join(long_data_her2, results_her2, by = c("Reference", "Target"))

# Remove significance where group does not match higher mean
long_data_her2 <- long_data_her2 %>%
  mutate(sig = ifelse(high != group, "", sig))

# Plot heatmap with differences for HER2+ subgroup
h1_her2 <- ggplot(long_data_her2, aes(x = Target_num, y = Reference_num, fill = score, color = group)) +
  geom_tile(width = 1, height = 0.5, linewidth = 0.2) +
  scale_fill_gradientn(
    colours = c('#008837', "white", "#7b3294"),
    guide = "colorbar"
  ) +
  scale_color_manual(
    values = response_colors,
    guide = guide_legend(override.aes = list(fill = NA, size = 1.5, linetype = 1))
  ) +
  scale_x_continuous(
    breaks = unique(merged_data_her2$Target_num),
    labels = unique(merged_data_her2$Target),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = unique(merged_data_her2$Reference_num),
    labels = unique(merged_data_her2$Reference),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Add significance labels
h2_her2 <- h1_her2 + geom_text(aes(label = sig), color = "black", size = 3, vjust = 0.5)

# Save the plot for HER2+ subgroup
pdf(file.path(output_folder, "Dismal6K_HER2_TME_coloc_diff_renew.pdf"), width = 6, height = 4)
print(h2_her2)
dev.off()

# 6. Subgroup Colocalization Analysis for HR+/HER2- Patients ---------------------------------------

# Filter data for HR+/HER2- subgroup
cmixing_luminal <- cmixing_50_tme %>% filter(Subtype_Her2g == "HR+/HER2-")

# Add a combined column for Reference and Target
cmixing_luminal <- cmixing_luminal %>%
  mutate(Reference_Target = paste(Reference, Target, sep = "_"))

# Initialize results dataframe for HR+/HER2- subgroup
results_luminal <- data.frame(
  Reference = character(),
  Target = character(),
  Mean_Dismal = numeric(),
  Mean_NonDismal = numeric(),
  p_value = numeric(),
  adjusted_p_value = numeric(),
  stringsAsFactors = FALSE
)

# Get unique Reference_Target combinations
unique_combinations_luminal <- unique(cmixing_luminal$Reference_Target)

# Perform t-test for each combination in HR+/HER2- subgroup
for (combo in unique_combinations_luminal) {
  # Subset data for the current combination
  subset_data <- cmixing_luminal %>% filter(Reference_Target == combo)
  
  # Check if both groups are present
  if (length(unique(subset_data$DFS)) == 2) {
    # Split data by group
    group1 <- subset_data %>% filter(DFS == "Dismal") %>% pull(Normalised_mixing_score)
    group2 <- subset_data %>% filter(DFS == "Non-dismal") %>% pull(Normalised_mixing_score)
    
    # Perform t-test
    test <- t.test(group1, group2)
    
    # Calculate means
    mean_group1 <- mean(group1, na.rm = TRUE)
    mean_group2 <- mean(group2, na.rm = TRUE)
    
    # Store results
    results_luminal <- rbind(results_luminal, data.frame(
      Reference = subset_data$Reference[1],
      Target = subset_data$Target[1],
      Mean_Dismal = mean_group1,
      Mean_NonDismal = mean_group2,
      p_value = test$p.value,
      adjusted_p_value = NA
    ))
  }
}

# Adjust p-values for multiple testing
results_luminal$adjusted_p_value <- p.adjust(results_luminal$p_value, method = "BH")

# Determine which group has higher mean
results_luminal$high <- ifelse(results_luminal$Mean_Dismal > results_luminal$Mean_NonDismal, "Dismal", "Non-dismal")

# Add significance labels
results_luminal <- results_luminal %>%
  mutate(sig = ifelse(adjusted_p_value <= 0.05, '**', ifelse(p_value <= 0.05, '*', '')))

# Prepare data for heatmap plotting
data_group1_luminal <- cmixing_luminal %>%
  filter(DFS == "Dismal") %>%
  group_by(Reference, Target) %>%
  summarize(Score_Dismal = mean(Normalised_mixing_score, na.rm = TRUE))

data_group2_luminal <- cmixing_luminal %>%
  filter(DFS == "Non-dismal") %>%
  group_by(Reference, Target) %>%
  summarize(Score_NonDismal = mean(Normalised_mixing_score, na.rm = TRUE))

# Merge the data by Reference and Target
merged_data_luminal <- merge(data_group1_luminal, data_group2_luminal, by = c("Reference", "Target"), all = TRUE)
merged_data_luminal[is.na(merged_data_luminal)] <- 0

# Add numeric labels for positioning
merged_data_luminal$Reference_num <- as.numeric(factor(merged_data_luminal$Reference))
merged_data_luminal$Target_num <- as.numeric(factor(merged_data_luminal$Target))

# Convert data to long format for ggplot2
long_data_dismal_luminal <- merged_data_luminal %>%
  select(Reference_num, Target_num, Score_Dismal, Reference, Target) %>%
  mutate(group = "Dismal", Reference_num = Reference_num + 0.25, score = Score_Dismal)

long_data_nondismal_luminal <- merged_data_luminal %>%
  select(Reference_num, Target_num, Score_NonDismal, Reference, Target) %>%
  mutate(group = "Non-dismal", Reference_num = Reference_num - 0.25, score = Score_NonDismal)

long_data_luminal <- bind_rows(long_data_dismal_luminal, long_data_nondismal_luminal)

# Merge with results
long_data_luminal <- left_join(long_data_luminal, results_luminal, by = c("Reference", "Target"))

# Remove significance where group does not match higher mean
long_data_luminal <- long_data_luminal %>%
  mutate(sig = ifelse(high != group, "", sig))

# Plot heatmap with differences for HR+/HER2- subgroup
h1_luminal <- ggplot(long_data_luminal, aes(x = Target_num, y = Reference_num, fill = score, color = group)) +
  geom_tile(width = 1, height = 0.5, linewidth = 0.2) +
  scale_fill_gradientn(
    colours = c('#008837', "white", "#7b3294"),
    guide = "colorbar"
  ) +
  scale_color_manual(
    values = response_colors,
    guide = guide_legend(override.aes = list(fill = NA, size = 1.5, linetype = 1))
  ) +
  scale_x_continuous(
    breaks = unique(merged_data_luminal$Target_num),
    labels = unique(merged_data_luminal$Target),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = unique(merged_data_luminal$Reference_num),
    labels = unique(merged_data_luminal$Reference),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Add significance labels
h2_luminal <- h1_luminal + geom_text(aes(label = sig), color = "black", size = 3, vjust = 0.5)

# Save the plot for HR+/HER2- subgroup
pdf(file.path(output_folder, "Dismal6K_HR_TME_coloc_diff_renew.pdf"), width = 6, height = 4)
print(h2_luminal)
dev.off()
