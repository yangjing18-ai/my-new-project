rm(list=ls())#clear Global Environments
setwd('D:\\software\\R-WORK')#设置工作路径
getwd()
#安装所需R包#
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggsignif")
install.packages("readxl")
library(readxl)
library(ggplot2)#绘图包
library(ggpubr)
library(ggsignif)
#读取数据（以之前做PCoA分析的OTU表为例）

otu_raw <- read_excel("功能微生物总.xlsx", col_names = TRUE)
row_names <- otu_raw[[1]]  # 提取第一列作为行名
otu_raw <- otu_raw[, -1]   # 删除第一列
rownames(otu_raw) <- row_names  # 将提取的列设置为行名
otu=otu_raw 
otu <- t(otu_raw)
#pca分析(此处使用R内置函数prcomp()函数进行分析)
df_PCA <- prcomp(otu,scal=TRUE)
# 预览PCA分析结果
df_PCA_sum <- summary(df_PCA)

# 提取出PC1及PC2的坐标
PC12 <- df_PCA$x[, 1:2]

# 计算各主成分的解释度（百分比）
pc <- df_PCA_sum$importance[2, ] * 100

# 将PC12从matrix转换为data.frame
PC12 <- as.data.frame(PC12)

# 给PC12添加samples变量（行名作为样本名）
PC12$samples <- row.names(PC12)

# 读取分组信息
group <- read.table("output_file.txt", sep = '\t', header = TRUE)

# 修改分组信息的列名
colnames(group) <- c("samples", "group")

# 将绘图数据（PC12）和分组信息合并
df <- merge(PC12, group, by = "samples")
# 设置颜色方案
color <- c("#1597A5", "#FFC24B", "#FEB3AE")

# 绘制PCA散点图
p1 <- ggplot(data = df, aes(x = PC1, y = PC2, color = group, shape = group)) +
  theme_bw() +  # 设置主题
  geom_point(size = 1.8) +  # 绘制点图并设置点的大小
  theme(panel.grid = element_blank()) +  # 去除网格线
  geom_vline(xintercept = 0, lty = "dashed") +  # 添加垂直虚线
  geom_hline(yintercept = 0, lty = "dashed") +  # 添加水平虚线
  # geom_text(aes(label = samples, y = V2 + 0.03, x = V1 + 0.03, vjust = 0), size = 3.5) +  # 可选：添加数据点标签
  guides(color = guide_legend(title = NULL)) +  # 去除图例标题
  labs(x = paste0("PC1 (", pc[1], "%)"), y = paste0("PC2 (", pc[2], "%)")) +  # 设置x、y轴标题为主成分贡献度
  stat_ellipse(data = df, geom = "polygon", level = 0.95, linetype = 2, size = 0.5, aes(fill = group), alpha = 0.2, show.legend = TRUE) +  # 添加置信椭圆
  scale_color_manual(values = color) +  # 设置点的颜色
  scale_fill_manual(values = color) +  # 设置填充颜色
  theme(
    axis.title.x = element_text(size = 12),  # 修改x轴标题文本大小
    axis.title.y = element_text(size = 12, angle = 90),  # 修改y轴标题文本大小和角度
    axis.t