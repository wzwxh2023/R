download_data <- function(dataset_id) {
  # 根据给定的数据集编号构建URL和目标文件名
  url <- paste0("https://www.ncbi.nlm.nih.gov/geo/download/?type=rnaseq_counts&acc=", dataset_id, "&format=file&file=", dataset_id, "_raw_counts_GRCh38.p13_NCBI.tsv.gz")
  destfile <- paste0(dataset_id, "_raw_counts_GRCh38.p13_NCBI.tsv.gz")
  
  # 使用download.file函数下载文件
  download.file(url, destfile, method="auto", mode = "wb")
  
  # 检查是否下载成功
  if (file.exists(destfile)) {
    cat("File downloaded successfully!\n")
    return(TRUE)
  } else {
    cat("Download failed. Please check the URL or internet connection.\n")
    return(FALSE)
  }
}

# 示例调用
# download_geo_data('GSE175685')
