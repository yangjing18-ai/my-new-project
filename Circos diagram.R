#清除工作空间中的所有对象
rm(list=ls())

#设置工作路径
setwd("D:/Rwork")

#加载所需的包
library(ggplot2)
library(RColorBrewer)
library(circlize)#画图包
library(ggsci)
#加载种水平物种丰度表，并设置列名和分隔符
data<-read.table("属1.csv",header=TRUE,sep=",",row.names=1)

#计算每一行的和并按降序排列
data$sum<-rowSums(data)
data1<-data[order(data$sum,decreasing=TRUE),]

#删除sum列
data1<-data1[-1,-7]


#计算丰富度排名前10的物种的相对丰度
data3<-data1[1:10,]/apply(data1,2,sum)


#计算剩下物种的总丰度并合并数据
data4<-1-apply(data3,2,sum)
data3<-rbind(data3,data4)
rownames(data3)[nrow(data3)]<-"Others"#修改最后一行行名others
data3<-as.matrix(data3)
#检查数据
print(row.names(data3))
print(colnames(data3))
print(apply(data3,2,sum))
#先画一个最基本的瞧瞧
chordDiagram(data3)


library(ggsci)
library(scales)
mycolor=pal_d3("category20",alpha=1)(20)
mycolor
cols=mycolor[c(1:8,12,13:14)]
rownames(data1)
grid.col = c(M0 = '#EF9A9A', L0 = '#90CAF9', A0 = '#F3D32C',
             M1 = '#8DD3C7', L1 = '#BEBADA', A1 = '#009933',
             Nocardioides = "#FF7F00", Microvirga = '#CC3366', Sphingomonas = '#c5c5f2',
             Rubrobacter = "#FB8072", Sphingosinicella = "#F781BF", Longimicrobium = "#80B1D3",
             Solirubrobacter = "#A65628", Blastococcus = "#A4566980", Microcoleus = "#0000FF",
             Geodermatophilus = "#FFFF00", Others = "#FF00FF")
# 打开pdf画板，新建一个文件，设置文件名为circle3.pdf，宽度为10，高度为5.1
pdf(file = "circle1.pdf", width = 10, height = 5.1)

# 绘制和弦图，设置颜色相关参数、透明度以及方向等属性
chordDiagram(data3,  
             grid.col = grid.col,  
             column.col = cols,  
             directional = -1,  
             transparency = 0.7)  # directional=1或-1 可在vector与links之间再加一层扇形

# 定义图例颜色对应关系（按照去掉前缀后的名称进行设置）
legend.col = c(
  Nocardioides = "#FF7F00",
  Microvirga = "#CC3366",
  Sphingomonas = '#c5c5f2',
  Rubrobacter = "#FB8072",
  Sphingosinicella = '#F781BF',
  Longimicrobium = '#80B1D3',
  Solirubrobacter = "#A65628",
  Blastococcus = "#A4566980",
  Microcoleus= "#0000FF",
  Geodermatophilus = "#FFFF00",
  Others = "#FF00FF"
)

# 绘制图例，设置图例位置、标题、外观样式、显示内容、符号形状、颜色以及字体大小等参数
legend(x = 0, y = 0.5,
       title = "Genus ", title.adj = 0,
       bty = 'n',  
       legend = c("Nocardioides", "Microvirga", "Sphingomonas",
                  "Rubrobacter", "Sphingosinicella", "Longimicrobium",
                  "Solirubrobacter", "Blastococcus",
                  "Microcoleus", "Geodermatophilus", "Others"),  
       pch = c(16),  # 16表示圆形符号
       col = legend.col,  
       cex = 1, pt.cex = 2,  
       ncol = 1, xpd = T)

# 关闭图形设备，保存pdf文件
dev.off()
circos.clear()