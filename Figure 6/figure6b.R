dummbell_plot<-ggplot(data_test,aes(x = value, y = set)) +  
  geom_line(aes(group = set)) +
  geom_point(aes(fill = variable), size = 3)+
  theme_classic()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")+
  coord_flip()

library(ggpubr)
boxplot<-ggboxplot(auc_table_long, x = "group", y = "auc", outlier.shape = NA,color = "group",palette = roc_color,add = "jitter") +
  stat_pvalue_manual(roc_test_plot_table,label = '{p.value.signif}',tip.length = 0)+
  xlab("Models") +
  ylab("AUCs") +
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.subtitle = element_text(vjust = -105, hjust = 0.05),
        text = element_text(size = 12),
        axis.text.y = element_text(size = 15, color = "black"))

library(patchwork)
merge_plot<-(boxplot/dummbell_plot)+
  plot_layout(heights = c(4, 1))
