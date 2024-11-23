# Load Required Libraries ---------------------------------------------------------------------
library(Seurat)
library(SingleCellExperiment)
library(edgeR)
library(ggplot2)
library(dplyr)
library(tidyverse)

# 1. Data Preparation -------------------------------------------------------------------------
object_folder <- "/path/object"  
output_folder <- "/path/object/output"

# Load the Seurat object
dismal6K <- readRDS(paste0(object_folder, '/Dismal6K_All.RDS'))

# Convert Seurat object to SingleCellExperiment
sce <- as.SingleCellExperiment(dismal6K)
sce <- logNormCounts(sce)

# 2. Abundance Analysis ------------------------------------------------------------------------
set.seed(1234)

# Create a table of cell type counts per field of view (fov_id)
abundances <- table(sce$celltype, sce$fov_id)
abundances <- as.matrix(abundances)

# Check dimensions
head(abundances)
length(colnames(abundances))
length(unique(sce$fov_id))

# Prepare DGEList object
extra.info <- colData(sce)[match(colnames(abundances), sce$fov_id), ]
y.ab <- DGEList(abundances, samples = extra.info)

design <- model.matrix(~ factor(Subtype) + factor(DFS), y.ab$samples)

# Estimate dispersion
y.ab <- estimateDisp(y.ab, design, trend = "none")
summary(y.ab$common.dispersion)
plotBCV(y.ab, cex = 1)

# Fit the model
fit.ab <- glmQLFit(y.ab, design, robust = TRUE, abundance.trend = FALSE)
summary(fit.ab$var.prior)
summary(fit.ab$df.prior)
plotQLDisp(fit.ab, cex = 1)

# Perform differential abundance testing
res <- glmQLFTest(fit.ab, coef = ncol(design))
summary(decideTests(res))
topTags(res)

# Normalize factors
y.ab2 <- calcNormFactors(y.ab)
y.ab2$samples$norm.factors
y.ab2 <- estimateDisp(y.ab2, design, trend = "none")
fit.ab2 <- glmQLFit(y.ab2, design, robust = TRUE, abundance.trend = FALSE)
res2 <- glmQLFTest(fit.ab2, coef = ncol(design))
topTags(res2)

# Prepare data for plotting
df <- as.data.frame(topTags(res2, n = 30))
df$cell <- rownames(df)
df$label <- ifelse(df$PValue < 0.05, "*", "")
df$cell <- factor(df$cell, levels = celltype_order)


# Create abundance plot for all cell types
abundance_all <- ggplot(data = df, aes(x = cell, y = logFC, fill = cell)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = all_cols) +
  geom_text(aes(label = label), vjust = -0.5) +  
  theme_classic() +
  labs(fill = 'Cell type', x = "", y = expression(log[2] * '-fold change')) +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  ) +
  scale_y_continuous(expand = expansion(mult = 0.1), limits = c(-3.5, 3.5))

# Save the abundance plot
pdf(file = paste0(output_folder, "/Dismal6K_Abundance_All_fig.pdf"), width = 4.5, height = 4)
print(abundance_all)
dev.off()

# Exclude cancer and epithelial cell types for TME analysis
offenders <- c(
  "Cancer LumA SC", "Cancer LumB SC", "Cancer Her2 SC", "Cancer Basal SC",
  "Cancer Unassigned", "Normal Epithelial"
)

# Subset y.ab to exclude offenders
y.ab3 <- y.ab[setdiff(rownames(y.ab), offenders), keep.lib.sizes = FALSE]

# Re-estimate dispersion and fit the model
y.ab3 <- estimateDisp(y.ab3, design, trend = "none")
fit.ab3 <- glmQLFit(y.ab3, design, robust = TRUE, abundance.trend = FALSE)
res3 <- glmQLFTest(fit.ab3, coef = ncol(design))
topTags(res3, n = 10)

# Prepare data for TME plotting
df_tme <- as.data.frame(topTags(res3, n = 30))
summary(decideTests(res3))
df_tme$cell <- rownames(df_tme)
df_tme$label <- ifelse(df_tme$PValue < 0.05, "*", "")
df_tme$cell <- factor(df_tme$cell, levels = setdiff(celltype_order, offenders))

# Create abundance plot for TME
abundance_tme <- ggplot(data = df_tme, aes(x = cell, y = logFC, fill = cell)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = all_cols) +
  geom_text(aes(label = label), vjust = -0.5) +  # Add asterisks above bars
  theme_classic() +
  labs(fill = 'Cell type', x = "", y = expression(log[2] * '-fold change')) +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  ) +
  scale_y_continuous(expand = expansion(mult = 0.2))

# Save the TME abundance plot
pdf(file = paste0(output_folder, "/Dismal6K_Abundance_TME_fig.pdf"), width = 4.5, height = 4)
print(abundance_tme)
dev.off()
