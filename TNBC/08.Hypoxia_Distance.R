# ---------------------------
# 1. Library Loading
# ---------------------------

# Load necessary libraries
library(Seurat)               
library(SpatialExperiment)    
library(SPIAT)               
library(dplyr)               
library(tidyr)                
library(ggplot2)              
library(ggh4x)                
library(smplot2)              
library(Cairo)       

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

# Load precomputed distance data
distance <- readRDS(file.path(object_folder, 'Dismal1K_distance_min_wide_from_cancer.Rds'))

# Load dismal1K cancer cells data
dismal1K_cancer <- readRDS(file.path(object_folder, 'Dismal1K_cancer_cells.Rds'))

# Extract metadata from Seurat object
df <- dismal1K_cancer@meta.data
rownames(df) <- NULL

# Join distance data with metadata based on Cell1 (assuming 'row_id' matches 'Cell1')
df <- df %>% 
  left_join(distance, by = c("row_id" = "Cell1"))

# ---------------------------
# 4. Data Reshaping for Visualization
# ---------------------------

# Define base clusters for analysis
base_clusters <- c(
  "Mac_CXCL9+", "Mac_APOE+", "Mac_MMP9+", "Mac_SPP1+", 
  "Monocytes", "cDCs", "pDCs", "Neutrophils",
  "T cells CD4+" "Tregs", "T cells CD8+", "NK cells",
  "B-cells Naive", "B-cells Memory", "Plasmablasts"
)

# Define groups for plotting
myeloid <- c("Mac_CXCL9+", "Mac_APOE+", "Mac_MMP9+", "Mac_SPP1+", 
             "Monocytes", "cDCs", "pDCs", "Neutrophils")
tnk <- c("CD4T_IL7R+", "CD4T_CXCL13+", "Tregs", "T cells CD8+", "NK cells")
bcell <- c("B-cells Naive", "B-cells Memory", "Plasmablasts IGHA+", "Plasmablasts IGHG+")

# Reshape the dataframe to long format for ggplot2
df_long <- df %>%
  pivot_longer(
    cols = all_of(base_clusters),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(variable %in% base_clusters) %>%
  mutate(
    group = case_when(
      variable %in% myeloid ~ 'Myeloid',
      variable %in% tnk ~ 'T/NK',
      variable %in% bcell ~ 'B/Plasma',
      TRUE ~ 'Other'
    )
  )

# Ensure 'variable' is a factor with specified levels
df_long$variable <- factor(df_long$variable, levels = c(myeloid, tnk, bcell))

# ---------------------------
# 5. Correlation Analysis
# ---------------------------

# Calculate Pearson correlation and adjusted p-values for each cell type
cor_data <- df_long %>%
  group_by(variable) %>%
  summarise(
    cor = cor.test(score_MP6_Hypoxia1, value, method = "pearson")$estimate,
    p.value = cor.test(score_MP6_Hypoxia1, value, method = "pearson")$p.value,
    .groups = 'drop'
  ) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))

# Merge correlation data back into the main dataframe
df_long <- df_long %>%
  left_join(cor_data, by = "variable") %>%
  mutate(rho_color = ifelse(cor < 0, 'neg', 'pos'))

# ---------------------------
# 6. Visualization: Hypoxia Score vs. Minimum Distance
# ---------------------------

# General Scatter Plot with Regression Lines
p1 <- ggplot(df_long, aes(x = score_MP6_Hypoxia1, y = value)) +
  geom_point(alpha = 0.5, size = 0.5, color = "gray") +
  geom_smooth(method = "lm", se = TRUE, aes(color = rho_color)) +
  facet_wrap(~variable, scales = "free_y", nrow = 4) +
  labs(
    x = "Hypoxia Score",
    y = "Minimum Distance (µm) from Cancer Cells",
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(face = "bold", size = 12)
  ) +
  geom_text(
    data = cor_data,
    aes(
      x = Inf, 
      y = Inf, 
      label = paste0("R = ", round(cor, 2), 
                     ", p.adj ", ifelse(p.adj < 0.001, "< 0.001", round(p.adj, 3)))
    ),
    color = "black",
    size = 3,
    hjust = 1.1,
    vjust = 1.5
  ) +
  scale_color_manual(
    values = c('pos' = "#088BBEFF", 'neg' = "#D9565CFF"),
    name = "Correlation"
  ) +
  scale_y_continuous(expand = expansion(mult = 0.2))

# Save the general plot as PDF and PNG
ggsave(
  filename = file.path(output_folder, "Hypoxia_Distance_Immune.pdf"),
  plot = p1,
  width = 18,
  height = 10
)


# ---------------------------
# 7. Focused Visualization: Only Myeloid and T cells CD8+
# ---------------------------

# Filter for specific cell types: Mac_SPP1+, Mac_CXCL9+, T cells CD8+, T cells CD4+
df_mac <- df_long %>%
  filter(variable %in% c('Mac_SPP1+', "Mac_CXCL9+", 'T cells CD8+', 'T cells CD4+')) %>%
  mutate(
    variable = ifelse(str_detect(variable, 'CD4'), 'T cells CD4+', variable)
  ) %>%
  left_join(
    cor_data %>% filter(variable %in% c('Mac_SPP1+', "Mac_CXCL9+", 'T cells CD8+', 'T cells CD4+')),
    by = "variable"
  ) %>%
  mutate(rho_color = ifelse(cor < 0, 'neg', 'pos'))


coi <- c("Mac_CXCL9+", 'Mac_SPP1+', 'T cells CD8+', 'T cells CD4+')
coi_colors <- all_cols[coi]
names(coi_colors) <- NULL  # Remove names if present
colors_with_alpha <- scales::alpha(coi_colors, 0.6)

# Create a themed strip for facets using ggh4x
strip <- ggh4x::strip_themed(background_x = elem_list_rect(fill = colors_with_alpha))

# Focused Scatter Plot with Regression Lines and Correlation Annotations
p2 <- ggplot(df_mac, aes(x = score_MP6_Hypoxia1, y = value)) + 
  geom_point(alpha = 0.5, size = 0.5, color = "black") +
  geom_smooth(method = "lm", se = TRUE, aes(color = rho_color)) +
  stat_cor(
    aes(label = paste0(..r.label.., "~`,`~`p adj <`~", 
                       ifelse(..p.adj.. < 0.001, "0.001", round(..p.adj.., 3)))),
    method = "pearson",
    label.y = 520,
    label.x.npc = 0.38
  ) +
  scale_color_manual(
    values = c('pos' = "#088BBEFF", 'neg' = "#D9565CFF"),
    name = "Correlation"
  ) +
  scale_fill_manual(values = all_cols) +
  coord_cartesian(ylim = c(0, 510)) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.2)),
    breaks = c(0, 100, 300, 500)
  ) +
  ggh4x::facet_wrap2(~variable, strip = strip, nrow = 2) +  # Facet by cell type
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(face = "bold", size = 12)
  ) +
  labs(
    x = "Hypoxia Signature in Cancer Cells",
    y = "Minimum Distance (µm)",
    title = "Hypoxia Score vs. Minimum Distance for Selected Cell Types"
  )


# Save the focused plot as PNG
ggsave(
  filename = file.path(output_folder, "Dismal1K_Hypoxia_Distance_Immune_Focused.png"),
  plot = p2,
  width = 7,
  height = 7,
  dpi = 300
)

# ---------------------------
# 8. Save Results
# ---------------------------

# Save the processed dataframes for future analyses
saveRDS(df_long, file.path(object_folder, 'df_long.Rds'))
saveRDS(df_mac, file.path(object_folder, 'df_mac.Rds'))


# ---------------------------
# End of Script
# ---------------------------
