seurat_run <- function(scobj, harmony_object = "donor_id", variance_explained = 0.9) {
  
  # Normalize data
  scobj <- NormalizeData(scobj)
  
  # Find variable features
  scobj <- FindVariableFeatures(scobj, selection.method = "vst", nfeatures = 2000)
  
  # Scale data
  scobj <- ScaleData(scobj, features = rownames(scobj))
  
  # Run PCA
  scobj <- RunPCA(scobj, features = VariableFeatures(object = scobj))
  
  # Calculate the number of dimensions that explain a specified variance
  xx <- cumsum(scobj[["pca"]]@stdev^2)
  xx <- xx / max(xx)
  ndim <- which(xx > variance_explained)[1]
  # Run Harmony for batch correction
  scobj <- RunHarmony(scobj, reduction = "pca", group.by.vars = harmony_object , reduction.save = "harmony")
  # Run UMAP
  scobj <- RunUMAP(scobj, reduction = "harmony", dims = 1:ndim)
  scobj <- FindNeighbors(scobj, reduction = "harmony", dims = 1:ndim)
  scobj <- FindClusters(scobj, resolution = seq(0.2,1.2,0.1))
  return(scobj)
}

# 使用示例:
# new_data <- seurat_run(old_data, harmony_object = "donor_id", variance_explained = 0.9)
