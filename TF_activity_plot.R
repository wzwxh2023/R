TF_activity_plot <- function(seurat_object,module,color) {
  rasMat <- seu[["AUCell"]]@data
  rasMat <- t(rasMat)
  rasMat <-as.data.frame(rasMat)
  cell.info <- seu@meta.data
  cell.info <- cbind(cell.info, FetchData(seu, vars = paste0("UMAP_", 1:2)))
  data.use <- cbind(rasMat,cell.info)
  colnames(data.use) <- gsub("\\(.*\\)", "", colnames(data.use))
   max.val <- quantile(data.use[, module], 0.99)
  low.val <- quantile(data.use[, module], 0.1)
  data.use[, module] <- ifelse(data.use[, module] > max.val, max.val, data.use[, module])
  library(ggrastr)
  library(ggplot2)
  plot <- ggplot(data.use, aes(UMAP_1, UMAP_2, color=get(module))) +
    rasterize(geom_point(size=0.05),
              dpi = 300) +
    theme_bw(base_size = 15) +
    ggtitle(paste(module, 'Activity')) +
    facet_wrap(~group) +
    scale_color_gradientn(name = NULL, colors = color) +
    theme(legend.position = "right",
          legend.title = element_blank(),
          plot.title = element_text(hjust = .5, face = "bold", size = 20),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color="black")
    )
  
  return(plot)
}

