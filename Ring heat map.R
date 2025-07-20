setwd("D:/Rwork")
data <- read.csv("基因丰度总表.csv", header = TRUE, row.names = 1, check.names = FALSE)

## R package download
install.packages("devtools")

devtools::install_github("jokergoo/circlize")
install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")

install.packages("RColorBrewer")
install.packages("dendextend")
install.packages("dendsort")
install.packages("gridBase")
### R loading
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
library(dendextend)
library(dendsort)
library(gridBase)
library(devtools)

### import data
data <- as.matrix(data)
cir1 <- t(scale(t(data)))

### define heatmap color gradient
mycol1 <- colorRamp2(c(-2, 0, 2), c("#57ab81", "white", "#ff9600"))

### create circular heatmap
# 确保 ann_row 是一个正确格式的因子向量
# 将 ann_row 转换为因子向量
ann_row <-  data.frame(pathway=c(rep("Inorganic P solubilization", 8), rep("Organic P mineralization", 25), rep("P-stravation response regulation", 4), rep("P uptake and transport", 16)))
row.names(ann_row) <- rownames(cir1)
ann_row<-as.matrix(ann_row)
# 创建热图
# 绘制环形热图
# 清理之前的绘图
circos.clear()
circos.par(gap.after=c(3, 3, 3, 15)) 
# 绘制热图
circos.heatmap(cir1, col=mycol1,
               dend.side="inside",
               rownames.side="outside",
               track.height = 0.35,
               rownames.col = "black",
               bg.border = NA,
               split = ann_row,
               show.sector.labels = TRUE,
               rownames.cex = 0.7,
               rownames.font = 1,
               cluster = TRUE,
               dend.track.height = 0.18,
               dend.callback = function(dend, m, si) {
                 # 先尝试较小的 k 值
                 k <- min(5, length(unique(cutree(dend, h=max(dend$height)))))
                 color_branches(dend, k=k, col=1:k)
               }
)
### add legend and column names
lg <- Legend(title="Legend", col_fun=mycol1, direction="horizontal")
grid.draw(lg)
circos.track(track.index=get.current.track.index(), panel.fun=function(x, y) {
  if (CELL_META$sector.numeric.index == 4) {  # the last sector
    cn <- colnames(cir1)
    n <- length(cn)
    circos.text(rep(CELL_META$cell.xlim[2], n) + convert_x(2, "mm"),
                (1:n) * 0.34 - 2.15,
                cn, cex=0.8, adj=c(0, 1), facing="inside")
  }
}, bg.border=NA)
circos.clear()    




