# figure 6a=====
par(pty = "s")

roc(ROC_trad[,1], ROC_trad[,2], plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage",
    col=roc_color[1], lwd=2.5,print.auc.y=55,
    print.auc=TRUE)

plot.roc(ROC_grs[,1],ROC_grs[,2],percent=TRUE,
         col=roc_color[2], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=50)

plot.roc(ROC_trad_grs[,1],ROC_trad_grs[,2],percent=TRUE,
         col=roc_color[3], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=45)

plot.roc(ROC_species_fdr[,1],ROC_species_fdr[,2],percent=TRUE,
         col=roc_color[4], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=40)

plot.roc(ROC_trad_species_fdr[,1],ROC_trad_species_fdr[,2],percent=TRUE,
         col=roc_color[5], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=35)

plot.roc(ROC_lipid_fdr[,1],ROC_lipid_fdr[,2],percent=TRUE,
         col=roc_color[6], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=30)

plot.roc(ROC_trad_lipid_fdr[,1],ROC_trad_lipid_fdr[,2],percent=TRUE,
         col=roc_color[7], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=25)

plot.roc(ROC_all[,1],ROC_all[,2],percent=TRUE,
         col=roc_color[8], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=20)

legend("bottomright", legend=c("Traditional", "Risk alleles","Traditional & Risk alleles","Species","Traditional & Species","Lipids","Traditional & Lipids","All combined"), 
       col=roc_color, lwd=2.5)

# figure 6b=====
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


# figure 6c=====
ggplot (data=imp_top_10)+
  geom_bar (aes(x=reorder(feature,count),y=count,fill=group), stat="identity") +
  scale_fill_manual(values=color_table)+
  coord_flip()+
  theme_classic()+
  labs(x="Feature",y="Counts")+
  theme(
    panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank())

# figure 6d=====
ggplot(score,aes(group,score_z,fill=GDM))+
  geom_boxplot(size=0.4,outlier.colour="grey",outlier.size=1)+  
  labs(y="Score")+
  scale_fill_manual(values =c('#8BABD3','#D7B0B0'))+
  stat_compare_means(method = "wilcox.test",label = "p.format",show.legend = F)+
  theme_classic() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),  
        axis.text.x = element_text(face="bold",color = 'black'),
        axis.ticks.x =  element_blank(),  
        axis.title.x = element_blank(),
        legend.background = element_blank())+
  guides(color=guide_legend(title="Group")) 

