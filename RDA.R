#调用R包
library(vegan)
library(ggrepel)
library(ggplot2)
library(ggpubr)
library(rdacca.hp)
setwd("E:\\桌面\\数据分析\\杨婧\\冗余分析")#设置路径
#读取数据，依次为otu数据、环境因子
sampledata <- read.csv("功能基因.csv", head = TRUE, row.names=1)
env <- read.csv("理化性质.csv", header=TRUE, row.names=1)
group <-  read.csv("分组.csv", header = FALSE,colClasses=c("character"))#分组依据
sampledata <- t(sampledata)
#定义分组的填充颜色
col <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","#007BFF","#33FF57")
#先进行DCA分析
dca <- decorana(veg = sampledata)
dca1 <- max(dca$rproj[,1])
dca2 <- max(dca$rproj[,2])
dca3 <- max(dca$rproj[,3])
dca4 <- max(dca$rproj[,4])
GL <- data.frame(DCA1 = c(dca1), DCA2 = c(dca2), DCA3 = c(dca3), DCA4 = c(dca4))
GL
#RDA分析    
rda <- rda(sampledata, env, scale = TRUE)
rdascore <- scores(rda)
rdascore$sites
rda$CCA$biplot
rdascore$species
RDAE <- as.data.frame(rda$CCA$biplot[,1:2])
RDAS1 <- rdascore$sites[,1]*0.2
RDAS2 <- rdascore$sites[,2]*0.2
plotdata <- data.frame(rownames(rdascore$sites), RDAS1, RDAS2, group$V2)
colnames(plotdata) <- c("sample","RDAS1","RDAS2","group")
rda1 <- round(rda$CCA$eig[1]/sum(rda$CCA$eig)*100,2)
rda2 <- round(rda$CCA$eig[2]/sum(rda$CCA$eig)*100,2)
PP <- ggplot(plotdata, aes(RDAS1, RDAS2)) +
  geom_point(aes(fill = group, color = group), size = 5) +
  scale_fill_manual(values = col) +  # 替换为你的颜色向量
  stat_chull(geom = "polygon", aes(group = group, color = group, fill = group), alpha = 0.1) +
  xlab(paste("RDA1 (", rda1, "%)", sep = "")) +
  ylab(paste("RDA2 (", rda2, "%)", sep = "")) +
  geom_segment(data = RDAE, aes(x = 0, y = 0, xend = RDAE[, 1], yend = RDAE[, 2]),
               colour = "black", size = 0.8,
               arrow = arrow(angle = 30, length = unit(0.4, "cm"))) +
  geom_text_repel(data = RDAE, segment.colour = "black",
                  aes(x = RDAE[, 1], y = RDAE[, 2], label = rownames(RDAE)), size = 8) +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(color = "black", size = 18),
        axis.ticks.length = unit(0.4, "lines"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(colour = "black", size = 18),
        axis.title.y = element_text(colour = "black", size = 18),
        axis.text = element_text(colour = "black", size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.key = element_blank(),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust = 0.5)) +
  theme(text = element_text(family = "Arial", size = 20))

# 显示绘图
print(PP)

library(eoffice)#保存PPT图片
topptx(filename='冗余分析图.pptx',height = 6,width = 8)
#层次分割（这一步计算很慢）
mite.rda.hp <- rdacca.hp(sampledata, env, method = 'RDA', type = 'adjR2', scale = FALSE)
mite.rda.hp
#显著性检验（计算量很大，一般算出来要很久）
#set.seed(123)
#permu_hp <- permu.hp(dv = sampledata, iv = env, method = 'RDA', type = 'adjR2', permutations = 999)
#permu_hp
#置换检验 
envfit <- envfit(mite.rda, env, permutations  = 999)
r <- as.matrix(envfit$vectors$r)
p <- as.matrix(envfit$vectors$pvals)
env.p <- cbind(r,p)
colnames(env.p) <- c("r2","p-value")
KK <- as.data.frame(env.p)
KK
# 1. 提取 mite.rda.hp 的 I.perc(%) 和指标名称
mite_rda_hp_data <- data.frame(  variable = rownames(mite.rda.hp$Hier.part), 
                                 percentage = mite.rda.hp$Hier.part[, "I.perc(%)"])
# 2. 提取 KK 中的 p-value
KK_data <- data.frame(  variable = rownames(KK),  
                        p_value = KK[, "p-value"])
# 3. 合并两个数据框
data_plot <- merge(mite_rda_hp_data, KK_data, by = "variable")
# 4. 设置显著性和颜色条件
data_plot$significance <- ifelse(data_plot$p_value < 0.05, "Significant", "Not Significant")
data_plot$color <- ifelse(data_plot$p_value < 0.05, "#F39B7FFF", "#8491B4FF")
# 5. 按 percentage 降序排列
data_plot_sorted <- data_plot[order(-data_plot$percentage), ]
data_plot_sorted$variable <- factor(data_plot_sorted$variable, levels = data_plot_sorted$variable)
# 绘制柱形图
ggplot(data_plot_sorted, aes(x = variable, y = percentage, fill = significance)) +
  geom_bar(stat = "identity", width = 0.7) +  # 绘制柱形图，调整柱形宽度
  geom_text(aes(label = round(percentage, 2)), vjust = -0.5, size = 3.5) +  # 在柱形上方标记数据
  theme_minimal() +  # 使用简约主题
  ylim(0, max(data_plot_sorted$percentage) + 2) +  # 自动设置 y 轴范围，增加上边距
  labs(
    y = "% Explained Variation",  # y 轴标签
    x = "",  # x 轴标签为空
    fill = "Significance"  # 图例标题
  ) +
  scale_fill_manual(
    values = c("Significant" = "#F39B7FFF", "Not Significant" = "#8491B4FF")  # 自定义颜色
  ) +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # x 轴标签旋转
    panel.background = element_rect(fill = "white")  # 白色背景
  )
