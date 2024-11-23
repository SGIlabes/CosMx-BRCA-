# 1. Load Required Libraries ---------------------------------------------------------------------
library(Seurat)
library(InSituType)
library(Matrix)
library(ggplot2)
library(ggrastr)
library(openxlsx)
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(viridis)
library(harmony)
library(stringr)
library(forcats)
library(ComplexHeatmap)
library(presto)
library(dittoSeq)
library(reshape2)
library(scales)
library(MAST)
library(circlize)
library(ggthemes)
library(ggsci)
library(wesanderson)
library(RColorBrewer)
library(msigdbr)
library(ggh4x)
library(tidyr)
library(ggradar)
library(ggrepel)
library(cowplot)

# 2. Load Data ------------------------------------------------------------------------------------
object_folder <- "/path/object"  
output_folder <- "/path/object/output"

dismal6K_myeloid <- readRDS(file.path(object_folder, 'Dismal6K_Myeloid.RDS'))

m_level <- c("Monocytes", "Neutrophils", "cDCs", "pDCs", "Mac_CD163+", "Mac_CD68+", "Mac_IFI+", "Mac_SPP1+")

# 3. Data Preprocessing ---------------------------------------------------------------------------

# Preprocess the data
dismal6K_myeloid <- dismal6K_myeloid %>%
  SCTransform(assay = 'Nanostring')

# Run PCA and Harmony integration
dismal6K_myeloid <- RunPCA(dismal6K_myeloid, assay = "SCT", npcs = 30)
dismal6K_myeloid <- RunHarmony(
  dismal6K_myeloid, group.by.vars = "fov_id",
  reduction = "pca", assay.use = "SCT", reduction.save = "harmony"
)
ElbowPlot(dismal6K_myeloid, ndims = 30)

# Run UMAP
dismal6K_myeloid <- RunUMAP(dismal6K_myeloid, reduction = "harmony", assay = "SCT", dims = 1:10)

# 4. UMAP Visualization ----------------------------------------------------------------------------

# Create UMAP plot with custom annotations
arr <- list(x = -8, y = -8.5, x_len = 3.5, y_len = 3)

p3 <- ggplot(DimPlot(dismal6K_myeloid, group.by = 'minor_type', cols = all_cols)$data, aes(UMAP_1, UMAP_2)) +
  ggrastr::rasterise(geom_point(aes(color = ident), alpha = 1, size = 3), dpi = 300, scale = 0.5) +
  scale_color_manual(values = scales::alpha(all_cols, 1)) +
  labs(color = "") +
  theme_void() +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  guides(color = guide_legend(override.aes = list(shape = 19, size = 5))) +
  annotate("segment",
           x = arr$x, xend = arr$x + c(arr$x_len, 0),
           y = arr$y, yend = arr$y + c(0, arr$y_len),
           arrow = arrow(type = "closed", length = unit(10, 'pt'))
  ) +
  annotate("text",
           x = (arr$x + arr$x + arr$x_len) / 2, y = arr$y - 0.5,
           label = "UMAP1", hjust = 0.5, vjust = 0.5, size = 5
  ) +
  annotate("text",
           x = arr$x - 0.5, y = (arr$y + arr$y + arr$y_len) / 2,
           label = "UMAP2", hjust = 0.5, vjust = 0.5, size = 5, angle = 90
  )


# Save the plot
pdf(file.path(output_folder, "Dismal6K_UMAP_myeloid.pdf"), width = 6, height = 5)
print(p3)
dev.off()

# 5. Dot Plot -------------------------------------------------------------------------------------
# Find markers for all clusters
myeloid_deg <- FindAllMarkers(
  dismal6K_myeloid, only.pos = TRUE, verbose = FALSE, assay = 'SCT', test.use = 'wilcox'
)

# Set factor levels for minor_type
dismal6K_myeloid$minor_type <- factor(dismal6K_myeloid$minor_type, levels = rev(m_level))

# Define markers to plot
m_marker <- c(
  "CD14", "LYZ",
  "SOD2", "CXCL8", "G0S2",  # Neutrophil
  "HLA-DRB", "HLA-DPA1", "CSF2RA",
  "PTGDS", "JCHAIN", "PLAC8", "CXCL9",
  "S100A9", "S100A8", "CTSS", "APOC1",
  "APOE", "SELENOP", "STAB1", "F13A1", "CD163", "C1QC",
  "CD68", "STAT1", "IFI6", "ISG15", "SPP1", "TGFBI"
)

# Create dot plot
pp3 <- DotPlot(
  object = dismal6K_myeloid, features = m_marker, cols = "RdBu",
  group.by = 'minor_type', dot.scale = 6, scale.max = 50
) +
  geom_point(aes(size = pct.exp), shape = 21, colour = 'black', stroke = 0.5) +
  guides(size = guide_legend(override.aes = list(shape = 21, colour = 'black', fill = 'white'))) +
  theme(
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11, angle = 90, hjust = 0.5, vjust = 0.5)
  ) +
  ylab('') + xlab('')

# Save dot plot
pdf(file.path(output_folder, "Dismal6K_myeloid_Dotplot_celltype.pdf"), width = 9, height = 4.5)
print(pp3)
dev.off()

# 6. Module Score ---------------------------------------------------------------------------------
# Load gene sets
macrophage_score <- read_xlsx('/path/gene/Macrophage_score.xlsx')  
macrophage_score <- macrophage_score[, -5]  

# Define features list
features_list <- list(
  M1 = na.omit(macrophage_score$M1),
  M2 = na.omit(macrophage_score$M2),
  Angio = na.omit(macrophage_score$Angiogenesis),
  Phago = na.omit(macrophage_score$Phagocytosis)
)

# Add module scores
for (name in names(features_list)) {
  feat <- features_list[[name]]
  dismal6K_myeloid <- AddModuleScore(
    object = dismal6K_myeloid,
    features = list(feat),
    name = name
  )
}

# Prepare data for plotting
data <- dismal6K_myeloid@meta.data[, c("minor_type", 'M11', 'M21', 'Angio1', 'Phago1')]
data <- data %>% filter(str_detect(minor_type, 'Mac') | minor_type == "Monocytes")
colnames(data) <- c("Cell type", "M1 score", "M2 score", "Angiogenesis", "Phagocytosis")
data$`Cell type` <- factor(data$`Cell type`, levels = c("Monocytes", 'Mac_CD163+', 'Mac_CD68+', 'Mac_IFI+', 'Mac_SPP1+'))

# Create violin plots
m1_plot <- data %>% ggplot(aes(x = `Cell type`, y = `M1 score`)) +
  geom_violin(aes(fill = `Cell type`), trim = FALSE) +
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  scale_fill_manual(values = all_cols) + theme_classic() +
  labs(y = "M1 signature score", x = "") + stat_compare_means()

m2_plot <- data %>% ggplot(aes(x = `Cell type`, y = `M2 score`)) +
  geom_violin(aes(fill = `Cell type`), trim = FALSE) +
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  scale_fill_manual(values = all_cols) + theme_classic() +
  labs(y = "M2 signature score", x = "") + stat_compare_means()

angio_score_pt <- data %>% ggplot(aes(x = `Cell type`, y = `Angiogenesis`)) +
  geom_violin(aes(fill = `Cell type`), trim = FALSE) +
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  scale_fill_manual(values = all_cols) + theme_classic() +
  labs(y = "Angiogenesis signature score", x = "") + stat_compare_means()

phago_score_pt <- data %>% ggplot(aes(x = `Cell type`, y = `Phagocytosis`)) +
  geom_violin(aes(fill = `Cell type`), trim = FALSE) +
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  scale_fill_manual(values = all_cols) + theme_classic() +
  labs(y = "Phagocytosis signature score", x = "") + stat_compare_means()

# Combine plots
combined_vln <- ggarrange(
  m1_plot, m2_plot, angio_score_pt, phago_score_pt,
  ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom"
)

# Save the plot
ggsave(file.path(output_folder, 'Macrophage_score.pdf'), combined_vln, width = 11, height = 8, dpi = 300)

# 7. Radar Plot ------------------------------------------------------------------------------------
# Subset macrophages
dismal6K_macrophage <- subset(dismal6K_myeloid, subset = major_type == "Macrophages")

# Prepare data
data <- dismal6K_macrophage@meta.data %>%
  select(minor_type, M11, M21, Angio1, Phago1)

# Aggregate data
agg_data <- data %>%
  group_by(minor_type) %>%
  summarise(across(ends_with("1"), mean, na.rm = TRUE))

# Prepare data for radar plot
agg_data2 <- agg_data %>% select(-minor_type)
rownames(agg_data2) <- agg_data$minor_type
colnames(agg_data2) <- c('M1', 'M2', 'Angiogenesis', 'Phagocytosis')

agg_data2_radar <- agg_data2 %>%
  as_tibble(rownames = "minor_type") %>%
  mutate_at(vars(-minor_type), scales::rescale)

# Create radar plot
p2 <- ggradar(
  agg_data2_radar,
  base.size = 1,
  background.circle.colour = "white",
  gridline.min.linetype = 2,
  gridline.mid.colour = 'gray',
  gridline.mid.linetype = 2,
  gridline.max.colour = 'gray',
  gridline.max.linetype = 2
) +
  scale_color_manual(values = all_cols) +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 2))

# Save the radar plot
pdf(file.path(output_folder, "Dismal6K_Mac_Polarity.pdf"), width = 7, height = 7)
print(p2)
dev.off()

# 8. Box Plot of Macrophage Proportions ------------------------------------------------------------
# Extract unique patient information
infor <- dismal6K_myeloid@meta.data %>%
  select(patient_id, fov_id, DFS, OS, bulk_type, date_op, date_death, date_pr, date_dx, Subtype_Her2, Subtype_Her2g) %>%
  distinct()

# Define 'mac_level' as the levels of macrophage cell types
mac_level <- c('Mac_CD163+', 'Mac_CD68+', 'Mac_IFI+', 'Mac_SPP1+')

# Calculate proportions
df <- dismal6K_myeloid@meta.data %>%
  filter(minor_type %in% mac_level) %>%
  count(fov_id, minor_type) %>%
  group_by(fov_id) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

df <- left_join(df, infor, by = "fov_id")

# Define colors for macrophage types 
mac_colors <- all_cols[mac_level]
names(mac_colors) <- NULL
colors_with_alpha <- scales::alpha(mac_colors, 0.4)

# Create the plot
strip <- ggh4x::strip_themed(background_x = elem_list_rect(fill = colors_with_alpha))

p <- ggplot(df, aes(x = DFS, y = proportion * 100, fill = DFS)) +
  geom_boxplot() +
  ggh4x::facet_wrap2(~minor_type, scales = "free_y", strip = strip) +
  scale_fill_manual(values = response_colors) +
  labs(x = "", y = "Macrophage Proportion (%)", fill = "Group") +
  stat_compare_means(label = "p.signif", method = "wilcox.test", comparisons = list(c('Non-dismal', 'Dismal'))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.title = element_text(face = "bold"),
    legend.position = 'none',
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 12)
  )

# Save the plot
pdf(file.path(output_folder, "Dismal6K_Macrophage_comp.pdf"), width = 4, height = 5)
print(p)
dev.off()

# 9. Proportion Box Plot by Subtype ----------------------------------------------------------------
p2 <- df %>%
  filter(minor_type == "Mac_SPP1+") %>%
  ggplot(aes(x = DFS, y = proportion * 100, fill = DFS)) +
  geom_boxplot() +
  facet_wrap(~Subtype_Her2g) +
  scale_fill_manual(values = response_colors) +
  labs(x = "", y = "Mac_SPP1+ / Total Macrophages (%)", fill = "Group") +
  stat_compare_means(label = "p.signif", method = "wilcox.test", comparisons = list(c('Non-dismal', 'Dismal'))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.title = element_text(face = "bold"),
    legend.position = 'none',
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 12)
  )

# Save the plot
pdf(file.path(output_folder, "Dismal6K_Macrophage_comp_by_Subtype.pdf"), width = 4, height = 3.5)
print(p2)
dev.off()

# 10. Correlation between IFNG/IFNA1 Expression and Macrophage Counts ------------------------------
# Calculate average expression per fov_id
df_ifng <- AverageExpression(
  dismal6K_myeloid, features = c('IFNG', 'IFNA1'),
  group.by = "fov_id", assays = 'SCT', slot = 'data'
)$SCT

df_ifng$Gene <- rownames(df_ifng)
df_ifng_long <- df_ifng %>%
  pivot_longer(
    cols = -Gene,
    names_to = "fov_id",
    values_to = "value"
  ) %>%
  mutate(
    fov_id = gsub("Nanostring.", "", fov_id),
    fov_id = gsub("SCT.", "", fov_id),
    fov_id = gsub("\\.", "_", fov_id)
  )

df_ifng_wide <- df_ifng_long %>%
  pivot_wider(
    id_cols = fov_id,
    names_from = Gene,
    values_from = value
  )

# Get macrophage counts per fov_id
data <- dismal6K_myeloid@meta.data %>%
  group_by(fov_id) %>%
  summarize(
    `Mac_CD68+` = sum(minor_type == "Mac_CD68+"),
    `Mac_SPP1+` = sum(minor_type == "Mac_SPP1+"),
    `Mac_CD163+` = sum(minor_type == "Mac_CD163+"),
    `Mac_IFI+` = sum(minor_type == "Mac_IFI+")
  )

df_ifng_wide <- left_join(df_ifng_wide, data, by = 'fov_id')

# Prepare data for plotting
df_long <- df_ifng_wide %>%
  pivot_longer(cols = starts_with("Mac_"), names_to = "Mac", values_to = "counts") %>%
  pivot_longer(cols = c("IFNA1", "IFNG"), names_to = "Gene", values_to = "Expression")

# Create scatter plot with correlation
if_cor <- ggscatter(
  df_long, x = "Expression", y = "counts",
  color = "Gene", add = "reg.line",
  facet.by = "Mac",
  palette = c("#00AFBB", "#E7B800"),
  show.legend.text = FALSE,
  add.params = list(color = "Gene", fill = "lightgray"),
  conf.int = TRUE
) +
  stat_cor(
    aes(color = Gene), data = df_long %>% filter(Gene == "IFNG"), show.legend = FALSE,
    method = "spearman", p.accuracy = 0.00001, digits = 2, cor.coef.name = 'rho',
    label.x.npc = 0.3, label.y.npc = 0.8, vjust = 1.5
  ) +
  stat_cor(
    aes(color = Gene), data = df_long %>% filter(Gene == "IFNA1"), show.legend = FALSE,
    method = "spearman", p.accuracy = 0.00001, digits = 2, cor.coef.name = 'rho',
    label.x.npc = 0.3, label.y.npc = 0.9, vjust = 1.5
  ) +
  labs(x = "Gene Expression", y = "Cell counts per FOV") +
  theme_bw()

# Save the plot
pdf(file.path(output_folder, "Dismal6K_IFNG_IFNA_cor.pdf"), width = 7, height = 6)
print(if_cor)
dev.off()
