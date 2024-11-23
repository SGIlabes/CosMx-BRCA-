# 1. Load Required Libraries --------------------------------------------------------------------
library(Seurat)
library(InSituType)
library(Matrix)
library(ggplot2)
library(ggpubr)
library(viridis)
library(harmony)
library(stringr)
library(purrr)
library(tidyverse)
library(edgeR)
library(SingleCellExperiment)
library(Cairo)

# 2. Read and Prepare Data ----------------------------------------------------------------------

breast_profile <- readRDS('/path/object/scbreast_cancer_custom.profile.rds')


# 5. InSituType Analysis ------------------------------------------------------------------------

# Load dismal6K object (update the path as needed)
dismal6K <- readRDS("dismal6K_seurat_object.Rds")

# Prepare immunofluorescence data
immunofluordata <- cbind(
  as.data.frame(dismal6K@meta.data)[, grep("Mean\\.|^Area$", colnames(dismal6K@meta.data))],
  data.frame(AspectRatio = pmax(dismal6K@meta.data$Height, dismal6K@meta.data$Width) /
               pmin(dismal6K@meta.data$Height, dismal6K@meta.data$Width))
)

cohort <- fastCohorting(immunofluordata, gaussian_transform = TRUE)

# Identify negative probes and features
negs <- grep("NegPrb", rownames(dismal6K@assays$Nanostring@counts), value = TRUE)
feats <- rownames(dismal6K@assays$Nanostring@counts)[
  !grepl("NegPrb|Custom", rownames(dismal6K@assays$Nanostring@counts))
]

# Run fully supervised cell typing
sup <- insitutypeML(
  t(dismal6K@assays$Nanostring@counts[feats, ]),
  neg = Matrix::rowMeans(t(dismal6K@assays$Nanostring@counts[negs, ])),
  cohort = cohort,
  bg = NULL,
  reference_profiles = breast_profile
)

# Save results

# Append results to metadata
dismal6K@meta.data$sup_type1 <- sup$clust
dismal6K@meta.data$sup_type1_p80 <- ifelse(sup$prob < 0.8, "NotDet", sup$clust)
dismal6K@meta.data$sup_type1_probs <- sup$prob

# 6. Refine Cell Type Annotations ---------------------------------------------------------------

# Define merges for major cell types
merges <- c(
  "Cancer Basal SC" = "Cancer",
  "Cancer Her2 SC" = "Cancer",
  "Cancer LumA SC" = "Cancer",
  "Cancer LumB SC" = "Cancer"
)

# Merge cell types
sup_type2 <- refineClusters(
  merges = merges,
  logliks = sup$logliks
)

# Append refined clusters to metadata
dismal6K@meta.data$sup_type2 <- sup_type2$clust
dismal6K@meta.data$sup_type2_p80 <- ifelse(sup_type2$prob < 0.8, "NotDet", sup_type2$clust)
dismal6K@meta.data$sup_type2_probs <- sup_type2$prob

# 7. Visualization ------------------------------------------------------------------------------

# Correct cell type names
sup$clust <- recode(sup$clust, "Macrophage" = "Macrophages", "Monocyte" = "Monocytes")


# Initial flight path plot
fp <- flightpath_plot_custom(
  flightpath_result = NULL,
  insitutype_result = sup,
  col = all_cols[sup$clust]
) + theme_void() +
  scale_fill_manual(values = scales::alpha(all_cols, 0.8))

# Save initial flight path plot
Cairo::CairoPDF("Dismal6K_Flight_Path_Initial.pdf", width = 6, height = 5)
print(fp + annotate(
  "text",
  x = 13,
  y = -8,
  label = "Total cell counts: 132,444 cells",
  hjust = 1,
  vjust = 1,
  size = 3.5,
  colour = "black"
))
dev.off()

# Final flight path plot
sup_final <- list(
  clust = dismal6K$meta.data$sup_type1,
  prob = dismal6K$meta.data$sup_type1_probs,
  logliks = sup$logliks[rownames(sup$logliks) %in% rownames(dismal6K@meta.data), ]
)


# Final plot
set.seed(129)
fp2 <- flightpath_plot_custom(
  flightpath_result = NULL,
  insitutype_result = sup_final,
  col = all_cols[sup_final$clust]
) + theme_void() +
  scale_fill_manual(values = scales::alpha(all_cols, 1))

Cairo::CairoPDF("Dismal6K_Flight_Path_Last_filter.pdf", width = 6, height = 5)
print(fp2 + annotate(
  "text",
  x = 13,
  y = -9,
  label = "Total cell counts: 110,629 cells",
  hjust = 1,
  vjust = 1,
  size = 4,
  colour = "black"
))
dev.off()

# 8. Dot Plot for Cell Types --------------------------------------------------------------------

# Define markers
marker <- c(
  "EPCAM", "EGFR", "ERBB2", "CEACAM6", "ACTA2",
  "S100A8", "S100A9", "CD14", "CD163", "CD68", "C1QA", "CD74", "CLEC10A", "HLA-DRB",
  "CD3D", "CD3G", "CD4", "FOXP3", "CD8A", "GZMA", "GZMB", "NKG7", "PRF1",
  "CD79A", "IGHM", "IGHG1/2", "MS4A1", "JCHAIN", "CD19", "MZB1",
  "TUBB", "IL6",
  "FAP", "TAGLN", "CD34",
  "MKI67", "VWF", "COL1A1", "CXCL12", "PDGFRA", "PDGFRB"
)



dismal6K$meta.data$sup_type2_p80 <- factor(dismal6K$meta.data$sup_type2_p80, levels = rev(sup_order))

# Create dot plot
pp <- DotPlot(
  object = dismal6K,
  features = marker,
  cols = "RdBu",
  group.by = "sup_type2_p80",
  dot.scale = 6,
  scale.max = 50
) +
  geom_point(aes(size = pct.exp), shape = 21, colour = "black", stroke = 0.5) +
  guides(size = guide_legend(override.aes = list(shape = 21, colour = "black", fill = "white"))) +
  theme(
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11, angle = 90, hjust = 0.5, vjust = 0.5)
  ) +
  ylab("") + xlab("")

# Save dot plot
pdf("NonTNBC_Dotplot_celltype_suptype2.pdf", width = 12, height = 4.5)
print(pp)
dev.off()

# 9. Cancer Subtype Analysis --------------------------------------------------------------------

# Define markers for cancer subtypes
cancer_markers <- c(
  "EPCAM", "ESR1", "PGR", "MKI67", "GATA3",
  "CXCL13", "CITED2", "SCUBE2", "JUN",
  "DHRS2", "MSMC5", "MAGED2", "STARD10", "PSAP", "HLA-DRB",
  "ERBB2", "CEACAM6", "S100A9", "AREG", "TPM1", "ATG5",
  "EGFR", "KRT5", "KRT6A/B/C", "S100A2", "KRT14", "TAGLN", "KRT16", "APOE"
)

# Subset cancer cells
cancer <- subset(dismal6K, subset = major_type == "Cancer")

# Define cancer subtype order
cancer_order <- c("Cancer LumA SC", "Cancer LumB SC", "Cancer Her2 SC", "Cancer Basal SC", "Cancer Unassigned")
cancer$meta.data$minor_type <- factor(cancer$meta.data$minor_type, levels = rev(cancer_order))

# Create dot plot for cancer subtypes
pp2 <- DotPlot(
  object = cancer,
  features = cancer_markers,
  cols = "RdBu",
  group.by = "minor_type",
  dot.scale = 6,
  scale.max = 50
) +
  geom_point(aes(size = pct.exp), shape = 21, colour = "black", stroke = 0.5) +
  guides(size = guide_legend(override.aes = list(shape = 21, colour = "black", fill = "white"))) +
  theme(
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11, angle = 90)
  ) +
  ylab("") + xlab("")

# Save cancer subtype dot plot
pdf("Dotplot_celltype_cancer_subtype.pdf", width = 9.5, height = 3)
print(pp2)
dev.off()


