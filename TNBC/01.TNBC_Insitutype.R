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
library(Cairo)                  
library(stringr)                

# ---------------------------
# 1. Reference Preparation
# ---------------------------

# Read in the data (Wu.el)
counts <- ReadMtx(
  mtx = "count_matrix_sparse.mtx", 
  cells = "count_matrix_barcodes.tsv", 
  features = "count_matrix_genes.tsv", 
  feature.column = 1, 
  cell.column = 1
)

meta_data <- read.csv("metadata.csv") %>%
  column_to_rownames(var = "X") %>%  
  select(-X)                          

breast_obj <- CreateSeuratObject(counts = counts, meta.data = meta_data)

breast_obj@meta.data <- breast_obj@meta.data %>%
  mutate(ref_cluster = case_when(
    celltype_major == "Cancer Epithelial" ~ celltype_minor,
    celltype_minor == "Myoepithelial"     ~ 'Myoepithelial',
    celltype_major == "Myeloid"           ~ celltype_minor,
    celltype_minor == "NKT cells"         ~ 'T cells CD8+',
    celltype_major == "CAFs"               ~ celltype_minor,
    celltype_major == "T-cells"           ~ celltype_minor,
    TRUE                                  ~ celltype_major
  ))

# Subset to remove unwanted clusters
`%nin%` <- Negate(`%in%`)
breast_ref <- subset(
  breast_obj, 
  subset = ref_cluster %nin% c("Cycling_Myeloid", 'Cycling T-cells', 'Cancer Cycling')
)

# Normalize data
breast_ref <- NormalizeData(
  breast_ref, 
  normalization.method = "LogNormalize", 
  scale.factor = 10000
)

# Identify highly variable genes (HVGs)
breast_ref <- FindVariableFeatures(
  breast_ref, 
  selection.method = "vst", 
  nfeatures = 2000
)

# Scale data
all.genes <- rownames(breast_ref)
breast_ref <- ScaleData(
  breast_ref, 
  features = all.genes
)

# Run PCA
breast_ref <- RunPCA(
  breast_ref, 
  features = VariableFeatures(object = breast_ref)
)

# SCTransform normalization
breast_ref <- SCTransform(
  breast_ref, 
  vars.to.regress = "nCount_RNA", 
  verbose = FALSE
)

# Run InSituType Estep function
reference_matrix_output <- InSituType::Estep(
  counts = t(breast_ref@assays$RNA@counts),
  clust = as.character(breast_ref@meta.data$ref_cluster),
  neg = rep(0, ncol(breast_ref@assays$RNA@counts))
)

# Store output profile
breast_profile <- reference_matrix_output

# ---------------------------
# 2. InSituType Analysis
# ---------------------------

# Prepare immunofluordata
immunofluordata <- cbind(
  as.data.frame(dismal1K@meta.data)[, grep("Mean\\.|^Area$", colnames(dismal1K@meta.data))],
  data.frame(
    AspectRatio = pmax(dismal1K@meta.data$Height, dismal1K@meta.data$Width) / 
      pmin(dismal1K@meta.data$Height, dismal1K@meta.data$Width)
  )
)

# Perform fast cohorting
cohort <- fastCohorting(
  immunofluordata,
  gaussian_transform = TRUE
)

# Define features and negatives
negs <- grep("NegPrb", rownames(dismal1K@assays$Nanostring@counts), value = TRUE)
feats <- setdiff(
  rownames(dismal1K@assays$Nanostring@counts),
  grep("NegPrb|Custom", rownames(dismal1K@assays$Nanostring@counts), value = TRUE)
)

# Run fully supervised cell typing
sup <- insitutypeML(
  t(dismal1K@assays$Nanostring@counts[feats, ]),
  neg = Matrix::rowMeans(t(dismal1K@assays$Nanostring@counts[negs, ])),
  cohort = cohort,
  bg = NULL, 
  reference_profiles = breast_profile
)

# ---------------------------
# 3. Cell Type Assignment
# ---------------------------

# Assign sup_type1 to metadata
dismal1K@meta.data$sup_type1 <- sup$clust

# Add "NotDet" annotation for low probability cell type calls
dismal1K@meta.data <- dismal1K@meta.data %>%
  mutate(
    sup_type1_p80 = ifelse(sup$prob < 0.8, "NotDet", sup_type1),
    sup_type1_probs = sup$prob
  )


# ---------------------------
# 4. Refining Clusters
# ---------------------------

# Define cluster merges
merges <- c(
  'Cancer Basal SC' = 'Cancer',
  'Cancer Her2 SC' = 'Cancer',
  'Cancer LumA SC' = 'Cancer',
  'Cancer LumB SC' = 'Cancer'
) 

# Refine clusters based on merges
sup_type2 <- refineClusters(
  merges = merges,
  logliks = sup$logliks
)

# Append refined clusters to metadata
dismal1K@meta.data$sup_type2 <- sup_type2$clust

# Add "NotDet" annotation for low probability cell type calls
dismal1K@meta.data <- dismal1K@meta.data %>%
  mutate(
    sup_type2_p80 = ifelse(sup_type2$prob < 0.8, "NotDet", sup_type2),
    sup_type2_probs = sup_type2$prob,
    major_type = case_when(
      str_detect(sup_type1_p80, "Cancer") ~ "Cancer",
      TRUE ~ sup_type1_p80
    )
  )

# Subset to remove "NotDet" annotations
dismal1K <- subset(
  dismal1K,
  subset = sup_type1 != "NotDet"
)

# ---------------------------
# 5. Initial Flightpath Plot
# ---------------------------

# Generate flightpath plot
fp <- flightpath_plot_custom(
  flightpath_result = NULL, 
  insitutype_result = sup,
  col = all_cols[sup$clust]
) +
  theme(
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_manual(values = scales::alpha(all_cols, 0.5)) -> sup_initial

# Display the plot
print(sup_initial)

# Save the initial flightpath plot to PDF
Cairo::CairoPDF(
  filename = paste0(output_folder, "/Dismal1K_Flight_Path_Initial.pdf"), 
  width = 9, 
  height = 7
)
print(sup_initial +
        annotate(
          "text", 
          x = 13, y = -8, 
          label = "Total cell counts: 151,171 cells",
          hjust = 1, vjust = 1, 
          size = 3.5, 
          colour = "black"
        ))
dev.off()

# ---------------------------
# 6. Filtered Analysis
# ---------------------------

# Subset logliks for cells present in metadata
subset_df <- sup$logliks[rownames(sup$logliks) %in% rownames(dismal1K@meta.data), ]

# Create a list for final supervised results
sup_final <- list(
  clust = dismal1K$sup_type1,
  prob = dismal1K$sup_type1_probs,
  logliks = subset_df
)

# Validate consistency
if (!(length(sup_final$clust) == length(sup_final$prob) && 
      length(sup_final$clust) == nrow(sup_final$logliks))) {
  stop("Mismatch in lengths of cluster assignments, probabilities, and logliks.")
}

# Check ID consistency
clust_ids <- names(sup_final$clust)
prob_ids <- names(sup_final$prob)
logliks_ids <- rownames(sup_final$logliks)

if (!(all.equal(clust_ids, prob_ids) && all.equal(clust_ids, logliks_ids))) {
  stop("Mismatch in IDs between cluster assignments, probabilities, and logliks.")
}

# Generate flightpath plot for filtered data
set.seed(1233332)
fp2 <- flightpath_plot_custom(
  flightpath_result = NULL, 
  insitutype_result = sup_final,
  col = all_cols[sup_final$clust]
) +
  theme(
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_manual(values = scales::alpha(all_cols, 1)) -> sup_last

sup_last <- sup_last +
  annotate(
    "text", 
    x = 13, y = -9, 
    label = "Total cell counts: 117,193 cells",
    hjust = 1, vjust = 1, 
    size = 4, 
    colour = "black"
  )

print(sup_last)

save_flightpath_plot <- function(plot, output_path_pdf, output_path_tiff, width = 6, height = 5) {
  Cairo::CairoPDF(
    filename = output_path_pdf, 
    width = width, 
    height = height
  )
  print(plot)
  dev.off()
  
  tiff(
    filename = output_path_tiff, 
    width = width, 
    height = height, 
    units = "in", 
    res = 300
  )
  print(plot)
  dev.off()
}

save_flightpath_plot(
  plot = sup_last,
  output_path_pdf = paste0(output_folder, "/Dismal1K_Flight_Path_Last_filter.pdf"),
  output_path_tiff = paste0(output_folder, "/Dismal1K_Flight_Path_Last.tiff"),
  width = 6, 
  height = 5
)

# ---------------------------
# 7. Further Filtering and Refinement
# ---------------------------

# Define additional cluster merges
additional_merges <- c(
  'Cancer Basal SC' = 'Cancer',
  'Cancer Her2 SC' = 'Cancer',
  'Cancer LumA SC' = 'Cancer',
  'Cancer LumB SC' = 'Cancer'
)

# Refine clusters based on additional merges
sup_type2_final <- refineClusters(
  merges = additional_merges, 
  logliks = sup$logliks
)

# Subset logliks for refined clusters
subset_df_final <- sup_type2_final$logliks[rownames(sup_type2_final$logliks) %in% rownames(dismal1K@meta.data), ]

# Create a list for final supervised results after refinement
sup_final_refined <- list(
  clust = dismal1K$sup_type2,
  prob = dismal1K$sup_type2_probs,
  logliks = subset_df_final
)

# Validate consistency
if (!(length(sup_final_refined$clust) == length(sup_final_refined$prob) && 
      length(sup_final_refined$clust) == nrow(sup_final_refined$logliks))) {
  stop("Mismatch in lengths of cluster assignments, probabilities, and logliks after refinement.")
}

# Check ID consistency
clust_ids_refined <- names(sup_final_refined$clust)
prob_ids_refined <- names(sup_final_refined$prob)
logliks_ids_refined <- rownames(sup_final_refined$logliks)

if (!(all.equal(clust_ids_refined, prob_ids_refined) && 
      all.equal(clust_ids_refined, logliks_ids_refined))) {
  stop("Mismatch in IDs between cluster assignments, probabilities, and logliks after refinement.")
}

set.seed(1233332)
fp2_refined <- flightpath_plot_custom(
  flightpath_result = NULL, 
  insitutype_result = sup_final_refined,
  col = all_cols[sup_final_refined$clust]
) +
  theme(
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_manual(values = scales::alpha(all_cols, 1)) -> sup_last_refined

sup_last_refined <- sup_last_refined +
  annotate(
    "text", 
    x = 13, y = -9, 
    label = "Total cell counts: 117,193 cells",
    hjust = 1, vjust = 1, 
    size = 4, 
    colour = "black"
  )

print(sup_last_refined)

# ---------------------------
# 8. Data Processing for Visualization
# ---------------------------

# Preprocess
dismal1K <- dismal1K %>%
  # NormalizeData() %>%
  # ScaleData() %>%
  SCTransform(assay = 'Nanostring')

# Run PCA
dismal1K <- RunPCA(
  dismal1K, 
  assay = "SCT", 
  npcs = 40
)

# Generate Elbow Plot to determine significant PCs
ElbowPlot(object = dismal1K, ndims = 40)

# Run Harmony for batch correction or integration
dismal1K <- RunHarmony(
  dismal1K, 
  group.by.vars = "Run_Tissue_name", 
  reduction = "pca", 
  assay.use = "SCT", 
  reduction.save = "harmony"
)

# Find neighbors and run UMAP
dismal1K <- FindNeighbors(
  object = dismal1K, 
  dims = 1:40, 
  reduction = 'harmony'
)
dismal1K <- RunUMAP(
  dismal1K, 
  reduction = "harmony", 
  assay = "SCT", 
  dims = 1:40
)

# ---------------------------
# 9. Dot Plot Visualization
# ---------------------------

# Define markers for dot plot
marker <- c(
  'EPCAM','EGFR','ERBB2','CEACAM6','ACTA2',
  'S100A8','S100A9','CD14','CD163','CD68','C1QA','CD74','CLEC10A',
  'CD3D','CD3G','CD4','FOXP3','CD8A','GZMA','GZMB','NKG7','PRF1',
  'CD79A','MS4A1','JCHAIN','CD19','MZB1',
  'TUBB','IL6',
  'FAP','TAGLN','CD34',
  'MKI67','VWF','COL1A1','CXCL12','PDGFRA','PDGFRB'
)

dismal1K@meta.data$sup_type2_p80 <- factor(
  dismal1K@meta.data$sup_type2_p80, 
  levels = rev(sup_order))


pp <- DotPlot(
  object = dismal1K, 
  features = marker, 
  cols = "RdBu",
  group.by = 'sup_type2_p80',
  dot.scale = 6,
  scale.max = 50
) +
  geom_point(
    aes(size = pct.exp), 
    shape = 21, 
    colour = 'black', 
    stroke = 0.5
  ) +
  guides(
    size = guide_legend(
      override.aes = list(
        shape = 21,
        colour = 'black',
        fill = 'white'
      )
    )
  ) +
  theme(
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11, angle = 90, hjust = 0.5, vjust = 0.5),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  ylab('') +
  xlab('')

print(pp)

ggsave(
  filename = paste0(output_folder, "/Dotplot_celltype_suptype2.pdf"),
  plot = pp,
  width = 10, 
  height = 4.5
)

# ---------------------------
# 10. Cancer-Specific Dot Plot
# ---------------------------

cancer_marker <- c(
  'EPCAM','ESR1','PGR','MKI67','GATA3',
  'CXCL13','AZGP1','AGR2','HSPB1','S100P',
  'MALAT1','XBP1','ADIRF','DHRS2','SLC40A1','HLA-DRB',
  'ERBB2','CEACAM6','S100A9','AREG','TPM1','ATG5', # HER2
  'EGFR','KRT5','KRT6A/B/C','S100A2','KRT14','TAGLN','KRT16','APOE'
)

cancer <- subset(
  dismal1K, 
  subset = major_type == "Cancer"
)


cancer_order <- c(
  "Cancer LumA SC",
  'Cancer LumB SC',
  'Cancer Her2 SC',
  'Cancer Basal SC',
  'Cancer Unassigned'
)

cancer@meta.data$minor_type <- factor(
  cancer@meta.data$minor_type, 
  levels = rev(cancer_order)
)

# Generate cancer-specific DotPlot
pp2 <- DotPlot(
  object = cancer, 
  features = cancer_marker, 
  cols = "RdBu",
  group.by = 'minor_type',
  dot.scale = 6,
  scale.max = 50
) +
  geom_point(
    aes(size = pct.exp), 
    shape = 21, 
    colour = 'black', 
    stroke = 0.5
  ) +
  guides(
    size = guide_legend(
      override.aes = list(
        shape = 21,
        colour = 'black',
        fill = 'white'
      )
    )
  ) +
  theme(
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11, angle = 90),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  ylab('') +
  xlab('')

# Display the cancer-specific DotPlot
print(pp2)

ggsave(
  filename = paste0(output_folder, "/Dotplot_celltype_cancer_subtype.pdf"),
  plot = pp2,
  width = 9.5, 
  height = 3
)
