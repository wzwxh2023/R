Umap_plot <- function(data, x_axis = "UMAP1", y_axis = "UMAP2", group_by, split_by = NULL, alpha, size, color_palette = NULL) {
  # 绘制基本图形
  pp2 <- ggplot(data = data, aes_string(x = x_axis, y = y_axis, color = group_by)) +
    rasterise(geom_point(aes_string(color = group_by), size = size, alpha = alpha),
              dpi = 300,
              scale = 0.6) #适当缩小散点
  
  # 如果有指定颜色方案，则添加到图中
  if (!is.null(color_palette)) {
    pp2 <- pp2 + scale_color_manual(values = color_palette)
  }
  
  # 自定义主题
  mytheme <- theme_void() + theme(plot.margin = margin(5.5, 15, 5.5, 5.5))
  
  # 使用自定义主题和其他美化选项进行进一步绘制
  pp3 <- pp2 +
    mytheme +
    theme_dr(xlength = 0.2,
             ylength = 0.2,
             arrow = grid::arrow(length = unit(0.1, "inches"),
                                 ends = 'last', type = "closed")) + #添加箭头坐标系
    theme(panel.grid = element_blank()) +
    guides(color = guide_legend(override.aes = list(size = 5))) #修改图例散点大小
  
  # 如果split_by参数不为空，则添加分面
  if (!is.null(split_by)) {
    pp3 <- pp3 + facet_grid(as.formula(paste("~", split_by)))
  }
  
  return(pp3)
}


