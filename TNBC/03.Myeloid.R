# Load required libraries
library(Seurat)
library(tidyverse)
library(miloR)
library(SingleCellExperiment)
library(ggplot2)
library(Cairo)
library(ggrastr)
library(ggpubr)

# 1. Myeloid Subset ---------------------------------------------------------------------------

# Set identities and subset myeloid cells
Idents(dismal1K) <- 'major_type'
dismal1K_myeloid <- subset(dismal1K, subset = major_type %in% c('DCs', 'Monocytes', 'Macrophages'))

# Define features excluding negative probes and custom genes
feats <- rownames(dismal1K@assays$Nanostring@counts)[
  !grepl("NegPrb|Custom", rownames(dismal1K@assays$Nanostring@counts))
]

# Preprocess data
dismal1K_myeloid <- dismal1K_myeloid %>%
  # NormalizeData() %>%
  # ScaleData() %>%
  SCTransform(assay = 'Nanostring')

# Run PCA and Harmony integration
dismal1K_myeloid <- RunPCA(dismal1K_myeloid, assay = "SCT", npcs = 15, features = feats)
dismal1K_myeloid <- RunHarmony(
  dismal1K_myeloid, group.by.vars = "fov_id", reduction = "pca",
  assay.use = "SCT", reduction.save = "harmony"
)

# Run UMAP and clustering
dismal1K_myeloid <- RunUMAP(dismal1K_myeloid, reduction = "harmony", assay = "SCT", dims = 1:9)
dismal1K_myeloid <- FindNeighbors(dismal1K_myeloid, reduction = "harmony")
dismal1K_myeloid <- FindClusters(dismal1K_myeloid, resolution = c(0.2, 0.4, 0.6, 0.8))

# 2. Subtype Identification -------------------------------------------------------------------

# Identify differentially expressed genes
Idents(dismal1K_myeloid) <- 'SCT_snn_res.0.2'
deg <- FindAllMarkers(dismal1K_myeloid, only.pos = TRUE)
deg_filtered <- deg %>%
  filter(avg_log2FC > 1, p_val_adj < 0.05)

# Select top 15 markers per cluster
top15 <- deg_filtered %>%
  group_by(cluster) %>%
  top_n(n = 15, wt = avg_log2FC)

# Define cell type IDs based on clusters
monocyte_ids <- rownames(dismal1K_myeloid@meta.data[dismal1K_myeloid$SCT_snn_res.0.8 %in% c("0", "2"), ])
mac_apoe_ids <- rownames(dismal1K_myeloid@meta.data[dismal1K_myeloid$SCT_snn_res.0.8 %in% c("1", "5", "6", "7", "14"), ])
mac_spp1_ids <- rownames(dismal1K_myeloid@meta.data[dismal1K_myeloid$SCT_snn_res.0.8 == "4", ])
mac_mmp9_ids <- rownames(dismal1K_myeloid@meta.data[dismal1K_myeloid$SCT_snn_res.0.8 == "12", ])
mac_cxcl9_ids <- rownames(dismal1K_myeloid@meta.data[dismal1K_myeloid$SCT_snn_res.0.8 %in% c("3", "10"), ])
neutrophil_ids <- rownames(dismal1K_myeloid@meta.data[dismal1K_myeloid$SCT_snn_res.0.8 == "9", ])
cdc_ids <- rownames(dismal1K_myeloid@meta.data[dismal1K_myeloid$SCT_snn_res.0.8 == "8", ])
pdc_ids <- rownames(dismal1K_myeloid@meta.data[dismal1K_myeloid$SCT_snn_res.0.8 == "11", ])

# Add minor cell type annotations
dismal1K_myeloid@meta.data$minor_type <- with(dismal1K_myeloid@meta.data, case_when(
  rownames(dismal1K_myeloid@meta.data) %in% monocyte_ids ~ "Monocytes",
  rownames(dismal1K_myeloid@meta.data) %in% mac_apoe_ids ~ "Mac_APOE+",
  rownames(dismal1K_myeloid@meta.data) %in% mac_spp1_ids ~ "Mac_SPP1+",
  rownames(dismal1K_myeloid@meta.data) %in% mac_mmp9_ids ~ "Mac_MMP9+",
  rownames(dismal1K_myeloid@meta.data) %in% mac_cxcl9_ids ~ "Mac_CXCL9+",
  rownames(dismal1K_myeloid@meta.data) %in% neutrophil_ids ~ "Neutrophil",
  rownames(dismal1K_myeloid@meta.data) %in% cdc_ids ~ "cDC",
  rownames(dismal1K_myeloid@meta.data) %in% pdc_ids ~ "pDC",
  TRUE ~ sup_type1
))

# 3. UMAP Visualization -----------------------------------------------------------------------

DimPlot(dismal1K_myeloid, cols = all_cols, pt.size = 1, shuffle = FALSE, group.by = 'minor_type')
arr <- list(x = -8, y = -8.5, x_len = 3.5, y_len = 3.5)

p3 <- ggplot(DimPlot(dismal1K_myeloid)$data, aes(umap_1, umap_2)) +
  ggrastr::rasterise(geom_point(aes(color = ident), alpha = 1, size = 3.5), dpi = 300, scale = 0.5) +
  scale_color_manual(values = scales::alpha(all_cols, 1)) +
  labs(color = "") +
  theme_void() +
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 14)
  ) +
  guides(color = guide_legend(override.aes = list(shape = 19, size = 5))) +
  annotate("segment",
           x = arr$x, xend = arr$x + c(arr$x_len, 0),
           y = arr$y, yend = arr$y + c(0, arr$y_len),
           arrow = arrow(type = "closed", length = unit(10, 'pt'))
  ) +
  annotate("text",
           x = arr$x + arr$x_len / 2, y = arr$y - 0.5,
           label = "UMAP1", hjust = 0.5, vjust = 0.5, size = 5
  ) +
  annotate("text",
           x = arr$x - 0.5, y = arr$y + arr$y_len / 2,
           label = "UMAP2", hjust = 0.5, vjust = 0.5, size = 5, angle = 90
  )

# Save UMAP plot
pdf(file = paste0(output_folder, "/Dismal1K_UMAP_myeloid.pdf"), width = 7, height = 5)
print(p3)
dev.off()

# 4. Dot Plot for Myeloid Cells ----------------------------------------------------------------

# Identify markers
myeloid_deg <- FindAllMarkers(dismal1K_myeloid, only.pos = TRUE, verbose = FALSE, assay = 'SCT', test.use = 'wilcox')

# Define cell type levels
m_level <- c("Monocytes", "Neutrophil", "cDC", "pDC", "Mac_CXCL9+", "Mac_APOE+", "Mac_MMP9+", "Mac_SPP1+")
dismal1K_myeloid$minor_type <- factor(dismal1K_myeloid$minor_type, levels = rev(m_level))

# Define markers for the dot plot
m_marker <- c(
  "CD14", "LYZ",
  "FCGR3A", "LST1", "CDKN1C",
  "SELENOP", "STAB1", "F13A1",
  "S100A9", "S100A8", "CTSS",
  "SOD2", "CXCL8", "G0S2",
  "C1QC", "CD163", "CD68", "MARCO", "HLA-DQA1",
  "CSF2RA", "CCL22", "CCR7", "DAPP1", "BIRC3", "LAMP3", "CD1C",
  "CD86", "CD80",
  "GATA2", "APOE", "MMP9", "APOC1", "SPP1", "PTGDS", "JCHAIN", "ISG15", "CXCL9", "CXCL10"
)

# Create dot plot
pp3 <- DotPlot(
  object = dismal1K_myeloid, features = m_marker, cols = "RdBu",
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
pdf(file = paste0(output_folder, "/Dismal1K_Myeloid_Dotplot_celltype.pdf"), width = 9, height = 4.5)
print(pp3)
dev.off()

# 5. Feature Plots ------------------------------------------------------------------------------

feat_spp1 <- FeaturePlot(dismal1K_myeloid, features = 'SPP1') + labs(x = "UMAP1", y = "UMAP2")
feat_cxcl9 <- FeaturePlot(dismal1K_myeloid, features = 'CXCL9') + labs(x = "UMAP1", y = "UMAP2")
feat_cxcl10 <- FeaturePlot(dismal1K_myeloid, features = 'CXCL10') + labs(x = "UMAP1", y = "UMAP2")

# Save feature plots
pdf(file = paste0(output_folder, "/Dismal1K_Myeloid_Feature_celltype.pdf"), width = 3.5, height = 7)
print(feat_spp1 + feat_cxcl9 + feat_cxcl10)
dev.off()

# 6. Milo Analysis ------------------------------------------------------------------------------

# Prepare data for Milo analysis
dismal1K_myeloid$DFS2 <- factor(dismal1K_myeloid$DFS, levels = c("Non-dismal", "Dismal"), labels = c("0", "1"))
sce <- as.SingleCellExperiment(dismal1K_myeloid)
altExps(sce) <- NULL
reducedDim(sce, "PCA") <- dismal1K_myeloid@reductions$harmony@cell.embeddings
reducedDim(sce, "UMAP") <- dismal1K_myeloid@reductions$umap@cell.embeddings

# Build Milo object and compute neighborhoods
milo <- Milo(sce)
milo <- buildGraph(milo, k = 20, d = 20, reduced.dim = "PCA")
milo <- makeNhoods(milo, prop = 0.2, k = 20, d = 20, refined = TRUE, reduced_dims = "PCA")

# Count cells in neighborhoods
milo <- countCells(milo, meta.data = as.data.frame(colData(milo)), sample = "fov_id")

# Prepare design matrix
design <- distinct(data.frame(colData(milo))[, c("fov_id", "DFS2", "bulk_type", "patient_id")])
rownames(design) <- design$fov_id
design <- design[colnames(nhoodCounts(milo)), , drop = FALSE]

# Calculate neighborhood distances
milo <- calcNhoodDistance(milo, d = 20, reduced.dim = "PCA")

# Differential abundance testing
da_results <- testNhoods(milo, design = ~DFS2, design.df = design, fdr.weighting = "graph-overlap", norm.method = "TMM")
da_results <- annotateNhoods(milo, da_results, coldata_col = "minor_type")

# 7. Milo UMAP Plot -----------------------------------------------------------------------------

# Plot DA results on UMAP
nh_graph_pl <- plotNhoodGraphDA(milo, da_results, layout = "UMAP", alpha = 0.1) +
  scale_fill_gradient2(low = '#1262b3', mid = 'lightgray', high = '#cc3d3d') +
  labs(fill = expression(log[2] * " fold change")) +
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9)
  ) +
  annotate("segment",
           x = arr$x, xend = arr$x + c(arr$x_len, 0),
           y = arr$y, yend = arr$y + c(0, arr$y_len),
           arrow = arrow(type = "closed", length = unit(10, 'pt'))
  ) +
  annotate("text",
           x = arr$x + arr$x_len / 2, y = arr$y - 0.5,
           label = "UMAP1", hjust = 0.5, vjust = 0.5, size = 5
  ) +
  annotate("text",
           x = arr$x - 0.5, y = arr$y + arr$y_len / 2,
           label = "UMAP2", hjust = 0.5, vjust = 0.5, size = 5, angle = 90
  )

# Save Milo UMAP plot
CairoPDF(file = paste0(output_folder, "/Dismal1K_Milo_nhood_myeloid.pdf"), width = 6, height = 5)
print(nh_graph_pl)
dev.off()

# 8. Milo Beeswarm Plot -------------------------------------------------------------------------

# Define cell type levels
da_results$minor_type <- factor(da_results$minor_type, levels = rev(m_level))

# Plot DA results with beeswarm
p_DA_celltype <- plotDAbeeswarm(da_results, group.by = "minor_type") +
  scale_color_gradient2(low = '#1262b3', mid = 'lightgray', high = '#cc3d3d') +
  labs(y = expression(log[2] * " fold change"), x = "") +
  theme_minimal()

# Save beeswarm plot
pdf(file = paste0(output_folder, "/Dismal1K_Myeloid_Milo_ggswarm.pdf"), width = 6, height = 6)
print(p_DA_celltype)
dev.off()

# 9. Macrophage Ratio Analysis ------------------------------------------------------------------

# Calculate ratios of macrophage subtypes
data <- dismal1K_myeloid@meta.data %>%
  filter(major_type == 'Macrophages') %>%
  group_by(fov_id) %>%
  summarize(
    macrophage1_count = sum(minor_type == "Mac_CXCL9+"),
    macrophage2_count = sum(minor_type == "Mac_SPP1+"),
    ratio = ifelse(macrophage2_count > 0, macrophage1_count / macrophage2_count, NA)
  ) %>%
  na.omit()

# Merge with additional information
infor <- dismal1K_myeloid@meta.data %>%
  select(fov_id, DFS, OS) %>%
  distinct()

data <- left_join(data, infor, by = "fov_id")

# Plot macrophage ratio
mac_ratio_plt <- data %>%
  ggplot(aes(x = DFS, y = ratio, fill = DFS)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = response_colors) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.title = element_text(face = "bold"),
    legend.position = 'none',
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(fill = "", x = "", y = "Mac_CXCL9+/Mac_SPP1+ ratio") +
  stat_compare_means(label = "p.value", method = "wilcox.test", label.y = 8.7) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.23)))

# Save macrophage ratio plot
pdf(file = paste0(output_folder, "/Dismal1K_Mac_spp1_cxcl9_ratio.pdf"), width = 3, height = 3.5)
print(mac_ratio_plt)
dev.off()
