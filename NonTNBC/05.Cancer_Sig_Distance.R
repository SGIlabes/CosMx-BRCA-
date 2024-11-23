# Load Required Libraries ---------------------------------------------------------------------
library(Seurat)
library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gtools)   # For mixedsort function
library(Hmisc)    # For rcorr function in correlation analysis

# 1. Data Preparation -------------------------------------------------------------------------

object_folder <- "/path/object"  
output_folder <- "/path/object/output"


# Load the Seurat object containing cancer cells
dismal6K_cancer <- readRDS(paste0(object_folder, '/Dismal6K_Cancer.RDS'))


# 2. Calculate Cancer Module Scores ------------------------------------------------------------

# Load cancer signature gene sets
cancer_score <- read_xlsx('/path/gene/cancer_mp_score.xlsx', sheet = 'MPs')

cancer_score <- cancer_score %>%
  mutate_all(~ gsub("HLA-DRB1", "HLA-DRB", .)) %>%  # Replace 'HLA-DRB1' with 'HLA-DRB'
  mutate_all(~ gsub("HLA-A", "MHC I", .))           # Replace 'HLA-A' with 'MHC I'

# Load BRCA-specific gene sets
brca <- read_xlsx('/path/gene/tumor_cell_brca_geneset.xlsx')
brca <- brca %>% filter(str_detect(cancer_type, 'BRCA'))
meta_program <- unique(brca$meta_program)

# Select relevant meta programs from cancer_score
cancer_score <- cancer_score %>% select(all_of(meta_program), `MP18 Interferon/MHC-II (II)`)

# Sort meta program columns
mp_columns <- colnames(cancer_score)[grep("^MP", colnames(cancer_score))]
mp_columns_sorted <- mixedsort(mp_columns)
cancer_score <- cancer_score[mp_columns_sorted]

# Add module scores to the Seurat object
set.seed(1234)
for (col_name in colnames(cancer_score)) {
  features_list <- list(na.omit(cancer_score[[col_name]]))
  
  dismal6K_cancer <- AddModuleScore(
    object = dismal6K_cancer,
    features = features_list,
    name = col_name,
    nbin = 25,
    ctrl = 30
  )
}

# 3. Prepare Data for Correlation Analysis -----------------------------------------------------

# Load minimum distance data between cancer cells and other cell types
distance_wide <- readRDS('/path/object/dismal6K_distance_min_wide.RDs')

# Select relevant columns from the Seurat object's metadata
selected_columns <- dismal6K_cancer@meta.data %>%
  select(contains("MP") & !contains("score"), fov_id, id)
rownames(selected_columns) <- NULL

# Merge the selected columns with the distance data
df <- left_join(selected_columns, distance_wide, by = c("id" = "Cell1"))


celltype_order <- c("B-cells", "T cells CD4+", "T cells CD8+", "Tregs", "NK cells", "Macrophages", "Dendritic cells", "Plasma cells", "Mast cells", "Neutrophils", "Fibroblasts", "Endothelial cells")

# Prepare the data frame for correlation analysis
df_all <- df %>%
  select(starts_with("MP"), any_of(celltype_order)) %>% 
  select(-starts_with('Cancer')) %>% 
  select(-starts_with('Normal')) %>% 
  select(-starts_with('Myoepithelial'))

# 4. Correlation Analysis and Heatmap Visualization --------------------------------------------


# Identify columns containing module scores and cell types
score_columns <- grep("^MP", colnames(df_all), value = TRUE)
mind_columns <- setdiff(global_order, c("Cancer", "Normal Epithelial", "Myoepithelial"))

# Ensure there are no missing values in module scores and distances
df_all <- df_all %>% drop_na()

# Calculate the correlation matrix and p-values using the Hmisc package
cor_results <- rcorr(as.matrix(df_all), type = "pearson")
cor_matrix <- cor_results$r
p_matrix <- cor_results$P

# Extract subsets for module scores and cell types
cor_matrix_subset <- cor_matrix[score_columns, mind_columns]
p_matrix_subset <- p_matrix[score_columns, mind_columns]

# Adjust p-values using the Benjamini-Hochberg method
adjusted_p_values <- p.adjust(as.vector(p_matrix_subset), method = "BH")
adjusted_p_matrix <- matrix(adjusted_p_values, nrow = nrow(p_matrix_subset), ncol = ncol(p_matrix_subset))
rownames(adjusted_p_matrix) <- rownames(p_matrix_subset)
colnames(adjusted_p_matrix) <- colnames(p_matrix_subset)

# Apply p-value threshold (set correlations with adjusted p > 0.05 to NA)
cor_matrix_subset[adjusted_p_matrix > 0.05] <- NA

# Convert correlation matrix to long format for plotting
cor_long <- melt(cor_matrix_subset, na.rm = TRUE)
cor_long_text <- cor_long %>% filter(abs(value) > 0.1)

# Create the heatmap visualization
p1 <- ggplot(cor_long, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "gray") +
  scale_fill_gradient2(low = '#cc3d3d', high = "#1262b3", mid = "white",
                       midpoint = 0, limit = c(-0.4, 0.4), space = "Lab", na.value = "grey50",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 8, hjust = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  geom_text(data = cor_long_text, aes(label = round(value, 2)), color = "black", size = 2)

# Display the plot
print(p1)

# Save the plot to a PDF file
pdf(file.path(output_folder, "Dismal6K_Cancer_Signature_TME_Min_distance.pdf"), width = 7, height = 4)
print(p1)
dev.off()
