library(vegan)
library(dplyr)
library(ggcor)
library(ggplot2)
setwd("")
#OTU表格-产量
df <- read.table("Field-222.txt",sep="\t",header = T,row.names = 1,check.names = F)
#环境因子数据-无人机性状
env <- read.table("UAV-111.txt",sep="\t",header = T,row.names = 1,check.names = F)
head(df)
head(env)
quickcor(env, type = "lower",method = "spearman") +
  geom_square()+
  scale_fill_gradient2( high = 'orange', mid = 'white',low = 'navyblue')  #颜色设置
df_mantel <- mantel_test(df, env, mantel.fun = 'mantel',
                         spec.dist.method = 'bray', 
                         env.dist.method = 'euclidean',
                         spec.select = list(Morphological_related = 1:2,
                                            Spike_related = 3:8,
                                            Grain_related = 9:11))#将群落数据按组进行分开-产量数据按人工-机器分开
df_mantel <- df_mantel %>%
  mutate(df_r = cut(r, breaks = c(-Inf, 0.1, 0.2, 0.4, Inf),
                    labels = c("< 0.1", "0.1 - 0.2", "0.2 - 0.4", ">= 0.4")),#定义Mantel的R值范围标签
         df_p = cut(p.value, breaks = c(-Inf, 0.01, 0.05, Inf),
                    labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))#定义Mantel的P值范围标签
quickcor(env,method = "spearman",exact=FALSE,type = "upper", cor.test = T, cluster.type = "all") +#环境因子之间的相关性热图
  geom_square() +#相关性显示形式
  #geom_mark(r = NA,sig.thres = 0.05, size = 3.5, colour = "black")+#显著性标签
  scale_fill_gradient2( high = '#FAE17D', mid = 'white',low = '#7FD3FD',limits=c(-1,1.0)) + #颜色设置
  anno_link(df_mantel, aes(color = df_p,
                           size = df_r))+
  scale_size_manual(values = c(0.8, 1.0, 1.8))+#连线粗细设置
  scale_color_manual(values = c("#A9D427","#FF934B","#e0dfdb"))+#线条颜色设置
  guides(fill = guide_colorbar(title = "correlation", order = 1),#图例相关设置
         size = guide_legend(title = "Mantel's r",order = 2),
         color = guide_legend(title = "Mantel's p", order = 3),
         linetype = "none")

quickcor(env,method = "spearman",exact=FALSE,type = "lower", cor.test = T, cluster.type = "all") +#环境因子之间的相关性热图
  geom_square() +#相关性显示形式
  #geom_mark(r = NA,sig.thres = 0.05, size = 3.5, colour = "black")+#显著性标签
  scale_fill_gradient2( high = '#FAE17D', mid = 'white',low = '#7FD3FD',limits=c(-1,1.0)) + #颜色设置
  anno_link(df_mantel, aes(color = df_p,
                           size = df_r))+
  scale_size_manual(values = c(0.8, 1.0, 1.8))+#连线粗细设置
  scale_color_manual(values = c("#A9D427","#FF934B","#e0dfdb"))+#线条颜色设置
  guides(fill = guide_colorbar(title = "correlation", order = 1),#图例相关设置
         size = guide_legend(title = "Mantel's r",order = 2),
         color = guide_legend(title = "Mantel's p", order = 3),
         linetype = "none")
