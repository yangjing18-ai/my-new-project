setwd("D:/Rwork")
data <- read.csv("gene2.csv")
data <- read.csv("理化性质2.csv")
install.packages(ggcor)
library(corrplot)#加载相关热图包

library(vegan)#加载vegan包

library(ggcor)

library(ggplot2)#一定要加载


data_gene<- read.csv("gene.csv", header = TRUE, row.names = 1, check.names = FALSE)
data_lihua<- read.csv("理化性质.csv", header = TRUE, row.names = 1, check.names = FALSE)

# 设置工作目录
setwd("D:/Rwork")

# 读取数据
data_gene <- read.csv("gene3.csv", header = TRUE, row.names = 1, check.names = FALSE)
data_lihua <- read.csv("理化性质3.csv", header = TRUE, row.names = 1, check.names = FALSE)

# 检查数据集的行数
cat("Gene data rows:", nrow(data_gene), "\n")
cat("Lihua data rows:", nrow(data_lihua), "\n")

# 对齐数据集
common_samples <- intersect(rownames(data_gene), rownames(data_lihua))
data_gene <- data_gene[common_samples, , drop = FALSE]
data_lihua <- data_lihua[common_samples, , drop = FALSE]

# 检查对齐后的行数
cat("Aligned gene data rows:", nrow(data_gene), "\n")
cat("Aligned lihua data rows:", nrow(data_lihua), "\n")

# 加载必要的包
library(corrplot)
library(vegan)
library(ggcor)
library(ggplot2)

# 进行 Mantel 检验
mantel <- mantel_test(data_gene, data_lihua, 
                      spec.select = list('Inorganic P solubilization' = 1,
                                         'Organic P mineralization' = 2,
                                         'P uptake and transport' = 3,
                                         'P-stravation response regulation' = 4)) %>%
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p.value, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))

# 查看结果
print(mantel)


mantel <- mantel_test(data_gene, data_lihua, 
                      
                      spec.select = list( 'Inorganic P solubilization' = 1,#依次定义四种物种作为Mantel的分析对象
                                          
                                          'Organic P mineralization' = 2,
                                          
                                          ' P uptake and transport'= 3,
                                          
                                          'P-stravation response regulation' = 4)) %>% 
  
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),#定义Mantel的R值范围标签，便于出图
         
         pd = cut(p.value, breaks = c(-Inf, 0.01, 0.05, Inf),
                  
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))#定义Mantel检验的p值范围标签，便于出图


dev.new(
  
  title = "mantel test",
  
  width = 18,
  
  height = 12,
  
  noRStudioGD = TRUE
  
)


quickcor(data_lihua, type = "upper") +
  geom_square() +
  anno_link(aes(colour = pd, size = rd), data = mantel) +
  scale_size_manual(values = c(0.4, 0.8, 1.2)) +
  guides(
    size = guide_legend(title = "Mantel's r", order = 2),
    colour = guide_legend(title = "Mantel's p", order = 3),
    fill = guide_colorbar(title = "Pearson's r", order = 4)
  ) +
  scale_fill_gradient2(low = "dodgerblue4", mid = "cornsilk", high = "indianred3", midpoint = 0)

# 定义蓝红渐变

ggsave("mantel_plot.pdf", plot = last_plot(), width = 20, height = 10)

