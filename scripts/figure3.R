#figure a===========
library(ggplot2)
p1<-ggplot(data=df,aes(x=PCoA1,y=PCoA2,
                       color=GDM,shape=GDM))+
  theme_bw()+
  geom_point(size=1.8)+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+
  #geom_hline(yintercept=0,linetype=4,color="grey") + 
  #geom_vline(xintercept=0,linetype=4,color="grey")
  #geom_text(aes(label=samples, y=V2+0.03,x=V1+0.03,  vjust=0),size=3.5)+
  #guides(color=guide_legend(title=NULL))+
  labs(x=paste0("PCoA 1 (", pc[1], "%)"),
       y=paste0("PCoA 2 (", pc[2], "%)"),
       title=adonis_label) +
  #scale_shape_manual(values=shape) +
  scale_color_manual(values = color) +
  scale_fill_manual(values = color)+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12,angle=90),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        panel.grid=element_blank()#wanggexian
  )+
  stat_ellipse(data=df,geom = "polygon",level=0.95,linetype = 2,size=0.2,
               aes(fill=GDM),alpha=0.3,show.legend = T)

#figure b===========
##tree=====
library(ggtree)
library(ggtreeExtra)
p_tree<-ggtree(tree,branch.length="none") + 
  geom_tiplab(offset=0.1,color="black")+
  geom_tippoint(aes(fill=class1), shape=21, size=3)+
  scale_fill_manual(values = c("#E99D96","#9DC1E2","#DAD7C4"))+
  geom_hilight(node=tree_tibble[tree_tibble$label=="Biosynthesis",]$node, fill="#E99D96", alpha=.3)+
  geom_hilight(node=tree_tibble[tree_tibble$label=="Degradation/Utilization/Assimilation",]$node, fill="#9DC1E2", alpha=.3)+
  geom_hilight(node=tree_tibble[tree_tibble$label=="GenerationofPrecursorMetabolitesandEnergy",]$node, fill="#DAD7C4", alpha=.3)+
  theme(legend.position = "none")

##barplot=====
mypal<-c("firebrick3","navy" )
pathway_barplot<-ggplot (data=pathway_barplot_table)+
  geom_bar (aes(x=short_name,y=or_total_1,fill=gdm), stat="identity") +
  
  scale_fill_manual(values=mypal)+
  coord_flip()+
  theme_classic()+
  labs(x="",y="Odds Ratio")+
  scale_y_continuous( breaks = c(-0.3,0,0.3), labels = c(0.7,1,1.3))+
  theme(
    panel.grid.major=element_line(colour=NA),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    legend.position = "bottom")



##heatmap=====
library(pheatmap)
heatmap <- pheatmap(association_r,
                    cluster_row = F,
                    cluster_cols =F,
                    display_numbers = matrix(
                      ifelse(association_fdr<0.25,
                             ifelse(association_fdr<0.1,
                                    ifelse(association_fdr<0.05,
                                           "***","**"),"*"),""),
                      nrow(association_fdr)),
                    number_color = "white",
                    color = colorRampPalette(c("navy", "white", "firebrick3"))(paletteLength),
                    breaks = breaklist,
                    cellwidth = 15,
                    cellheight = 15,
                    angle_col = 45
)

library(ggplotify)
heatmap_ggplot = as.ggplot(heatmap)


##merge====
library(patchwork)
merge_plot<-p_tree+heatmap_ggplot+pathway_barplot+
  plot_layout(widths = c(1, 4,1))

#figure d===========
ggplot(ec_abundance,aes(ec,abundance,color=GDM))+
  geom_boxplot(size=0.4,outlier.colour="grey",outlier.size=1)+  
  scale_color_manual(values =c('#8BABD3','#D7B0B0'))+
  labs(x="Enzymes",y="Abundance")+
  geom_jitter(aes(jitter_group+jitter_position, abundance, fill=GDM),
              position=position_jitter(width=0.15,height=0),
              alpha=0.5,
              shape=21, size = 1,show.legend = F) +
  scale_fill_manual(values =c('#8BABD3','#D7B0B0'))+
  stat_compare_means(method = "wilcox.test",label = "p.format",show.legend = F)+
  
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),  
        axis.text.x = element_text(face="bold",color = 'black'),
        axis.title.x = element_blank(),
        plot.title = element_text( hjust = 0.5 ),
        legend.background = element_blank())+
  guides(color=guide_legend(title="Group")) 
#figure e===========
ggplot(fa_abundance,aes(fa,abundance,color=GDM))+
  geom_boxplot(size=0.4,outlier.colour="grey",outlier.size=1)+  
  scale_color_manual(values =c('#8BABD3','#D7B0B0'))+
  labs(x="Enzymes",y="Abundance")+
  geom_jitter(aes(jitter_group+jitter_position, abundance, fill=GDM),
              position=position_jitter(width=0.15,height=0),
              alpha=0.5,
              shape=21, size = 1,show.legend = F) +
  scale_fill_manual(values =c('#8BABD3','#D7B0B0'))+
  stat_compare_means(method = "wilcox.test",label = "p.format",show.legend = F)+
  
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),  
        axis.text.x = element_text(face="bold",color = 'black'),
        axis.title.x = element_blank(),
        plot.title = element_text( hjust = 0.5 ),
        legend.background = element_blank())+
  guides(color=guide_legend(title="Group")) 

