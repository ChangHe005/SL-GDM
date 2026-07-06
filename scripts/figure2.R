#figure 2a=========
library(vegan)
library(tidyverse)
library(ggpubr)
library(patchwork)

simpson_boxplot<-ggplot(alpha_table,aes(GDM,simpson,fill=GDM))+
  geom_boxplot(size=0.4,outlier.colour="grey",outlier.size=0.8)+  
  scale_fill_manual(values = c('#8BABD3','#D7B0B0'))+
  labs(x="",y="Simpson Index")+
  stat_compare_means(comparisons = list(c("Control","GDM")),method = "wilcox.test")+
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),  
        axis.text.x = element_text(face="bold",color = 'black'),
        axis.title.x = element_blank(),
        plot.title = element_text( hjust = 0.5 ),
        legend.background = element_blank())+
  guides(fill=guide_legend(title="Group")) 

shannon_boxplot<-ggplot(alpha_table,aes(GDM,shannon,fill=GDM))+
  geom_boxplot(size=0.4,outlier.colour="grey",outlier.size=0.8)+  
  scale_fill_manual(values = c('#8BABD3','#D7B0B0'))+
  labs(x="",y="Shannon Index")+
  stat_compare_means(comparisons = list(c("Control","GDM")),method = "wilcox.test")+
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),  
        axis.text.x = element_text(face="bold",color = 'black'),
        axis.title.x = element_blank(),
        plot.title = element_text( hjust = 0.5 ),
        legend.background = element_blank())+
  guides(fill=guide_legend(title="Group")) 

alpha_boxplot_merge<-shannon_boxplot+simpson_boxplot+ plot_layout(guides = 'collect')


#figure 2b=========

p1<-ggplot(data=df,aes(x=PCoA1,y=PCoA2,
                       color=GDM,shape=GDM))+
  stat_ellipse(data=df,geom = "polygon",level=0.95,linetype = 2,size=0.2,
               aes(fill=GDM),alpha=0.3,show.legend = T)+
  scale_fill_manual(values = c('#8BABD3','#D7B0B0'))+
  theme_bw()+
  geom_point(size=2)+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+
  labs(x=paste0("PCoA 1 (", pc[1], "%)"),
       y=paste0("PCoA 2 (", pc[2], "%)"),
       title=adonis_label) +
  scale_color_manual(values = c('#8BABD3','#D7B0B0'))+
  
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12,angle=90),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        panel.grid=element_blank()
  )


xplot<-ggplot(df,aes(GDM,PCoA1,fill=GDM))+
  geom_boxplot(size=0.4,outlier.colour="grey",outlier.size=0.8)+  
  stat_compare_means(comparisons = list(c("Control","GDM")))+  
  scale_fill_manual(values = c('#8BABD3','#D7B0B0'))+
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),  
        axis.ticks=element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.position = 'none')+
  ylab("PCoA1")+
  coord_flip()

yplot<-ggplot(df,aes(GDM,PCoA2,fill=GDM))+
  geom_boxplot(size=0.4,outlier.colour="grey",outlier.size=0.8)+  
  stat_compare_means(comparisons =list(c("Control","GDM")))+
  scale_fill_manual(values = c('#8BABD3','#D7B0B0'))+
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),  
        axis.ticks=element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.position = 'none')+
  ylab("PCoA2")

layout <- "
AAAA#
BBBBC
BBBBC
BBBBC
"
p_merge<-xplot + p1 + yplot + 
  plot_layout(design = layout,guides = 'collect')
p_merge

#figure 2c=========
library(tidyverse)
library(haven)
library(RColorBrewer)
library(patchwork)
library(ggbreak)

# 1. GDM Barplot
plot_gdm <- ggplot(data = df_gdm_variance) +
  geom_bar(
    aes(x = reorder(feature, r_total), y = r_total, fill = group), 
    stat = "identity"
  ) +
  coord_flip() + 
  theme_classic() +
  labs(x = "", y = "", title = "GDM") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = color_list) +
  scale_y_continuous(breaks = c(0, 0.02, 0.04), limits = c(0, 0.04)) +
  geom_text(
    aes(
      x = reorder(feature, r_total), 
      y = r_total + 0.002, 
      label = ifelse(p_plot == "NS", "", p_plot)
    ),
    hjust = 0.5, vjust = 0.5, angle = 270
  )

# 2. OGTT Fasting Barplots
plot_fpg_base <- ggplot(data = df_fpg_sig) +
  geom_bar(
    aes(x = reorder(feature, desc(rank)), y = r_value, fill = group), 
    stat = "identity"
  ) +
  coord_flip() + 
  labs(x = "", y = "", title = "OGTT-Fasting") +
  theme_classic() +
  scale_fill_manual(values = color_list)

plot_fpg_standard <- plot_fpg_base +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(breaks = c(0, 0.02, 0.04), limits = c(0, 0.04)) +
  geom_text(
    aes(
      x = reorder(feature, r_value), 
      y = r_value + 0.002, 
      label = ifelse(p_plot == "NS", "", p_plot)
    ),
    hjust = 0.5, vjust = 0.5, angle = 270
  )

plot_fpg_break <- plot_fpg_base +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_y_break(c(0.04, 0.041)) +
  geom_text(
    aes(
      x = reorder(feature, r_value), 
      y = r_value + 0.002, 
      label = ifelse(p_plot == "NS", "", p_plot)
    ),
    hjust = 0.5, vjust = 0.5, angle = 270
  )

# 3. OGTT 1H Barplots
plot_ogtt_1h_base <- ggplot(data = df_ogtt_1h_sig) +
  geom_bar(
    aes(x = reorder(feature, desc(rank)), y = r_value, fill = group), 
    stat = "identity"
  ) +
  coord_flip() + 
  labs(x = "", y = "", title = "OGTT-1H") +
  theme_classic() +
  scale_fill_manual(values = color_list)

plot_ogtt_1h_standard <- plot_ogtt_1h_base +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(breaks = c(0, 0.02, 0.04), limits = c(0, 0.04)) +
  geom_text(
    aes(
      x = reorder(feature, r_value), 
      y = r_value + 0.002, 
      label = ifelse(p_plot == "NS", "", p_plot)
    ),
    hjust = 0.5, vjust = 0.5, angle = 270
  )

plot_ogtt_1h_break <- plot_ogtt_1h_base +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_y_break(c(0.04, 0.041)) +
  geom_text(
    aes(
      x = reorder(feature, r_value), 
      y = r_value + 0.002, 
      label = ifelse(p_plot == "NS", "", p_plot)
    ),
    hjust = 0.5, vjust = 0.5, angle = 270
  )

# 4. OGTT 2H Barplot
plot_ogtt_2h <- ggplot(data = df_ogtt_2h_sig) +
  geom_bar(
    aes(x = reorder(feature, desc(rank)), y = r_value, fill = group), 
    stat = "identity"
  ) +
  coord_flip() + 
  labs(x = "", y = "", title = "OGTT-2H") +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(values = color_list) +
  scale_y_continuous(breaks = c(0, 0.02, 0.04), limits = c(0, 0.04)) +
  geom_text(
    aes(
      x = reorder(feature, r_value), 
      y = r_value + 0.002, 
      label = ifelse(p_plot == "NS", "", p_plot)
    ),
    hjust = 0.5, vjust = 0.5, angle = 270
  )

# 5. OGTT AUC Barplot
plot_ogtt_auc <- ggplot(data = df_ogtt_auc_sig) +
  geom_bar(
    aes(x = reorder(feature, desc(rank)), y = r_value, fill = group), 
    stat = "identity"
  ) +
  coord_flip() + 
  labs(x = "", y = "", title = "OGTT-AUC") +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(values = color_list) +
  scale_y_continuous(breaks = c(0, 0.02, 0.04), limits = c(0, 0.04)) +
  geom_text(
    aes(
      x = reorder(feature, r_value), 
      y = r_value + 0.002, 
      label = ifelse(p_plot == "NS", "", p_plot)
    ),
    hjust = 0.5, vjust = 0.5, angle = 270
  )

# 6. HbA1c Barplot
plot_hba1c_break <- ggplot(data = df_hba1c_sig) +
  geom_bar(
    aes(x = reorder(feature, desc(rank)), y = r_value, fill = group), 
    stat = "identity"
  ) +
  coord_flip() + 
  labs(x = "", y = "", title = "HbA1c") +
  theme_classic() +
  scale_fill_manual(values = color_list) +
  scale_y_break(c(0.04, 0.041)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.y.right = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_blank(),
    legend.position = "none"
  ) +
  geom_text(
    aes(
      x = reorder(feature, r_value), 
      y = r_value + 0.002, 
      label = ifelse(p_plot == "NS", "", p_plot)
    ),
    hjust = 0.5, vjust = 0.5, angle = 270
  )

# 7. Patchwork Merging
merged_plot_standard <- plot_gdm +
  plot_fpg_standard +
  plot_ogtt_1h_standard +
  plot_ogtt_2h +
  plot_ogtt_auc +
  plot_layout(nrow = 1, guides = 'collect') &
  theme(legend.position = "bottom")

merged_plot_break <- plot_fpg_break +
  plot_ogtt_1h_break +
  plot_hba1c_break +
  plot_layout(nrow = 1, widths = c(1, 1, 1), guides = 'collect') &
  theme(legend.position = "bottom")
#figure 2d=========
library(tidyverse)
library(taxonomizr)
library(ggtree)
library(ggtreeExtra)
library(ggnewscale)
library(RColorBrewer)


p_tree<-ggtree(tree,aes(color=phylum),layout='fan',size=0.15,open.angle=90)+
  geom_hilight(node=tree_tibble[tree_tibble$label=="p__Euryarchaeota",]$node, fill="#779649", alpha=.3)+
  geom_hilight(node=tree_tibble[tree_tibble$label=="p__Actinobacteria",]$node, fill="#c67915", alpha=.3)+
  geom_hilight(node=tree_tibble[tree_tibble$label=="p__Bacteroidetes",]$node, fill="#ffee6f", alpha=.3)+
  geom_hilight(node=tree_tibble[tree_tibble$label=="p__Firmicutes",]$node, fill="#007175", alpha=.3)+
  geom_hilight(node=tree_tibble[tree_tibble$label=="p__Verrucomicrobia",]$node, fill="#ba5140", alpha=.3)+
  geom_hilight(node=tree_tibble[tree_tibble$label=="p__Proteobacteria",]$node, fill="#12507b", alpha=.3)


p_output<-gheatmap(p_tree+new_scale_fill(), taxonomy_clinic[,2:5], offset=.4, width=.25,
                   colnames_angle=95,colnames_offset_y = .6,font.size = 2,hjust=1.2)+
  scale_fill_gradient2(low = heatmap_color[2], high = heatmap_color[1], midpoint = 0,na.value = "white")+
  new_scale_fill()+
  new_scale_color()+
  geom_segment(data = df_table, aes(x+3, y =y,xend = x+3 +log2FC_abs, yend = y, color = GDM_sig),size = 2)+
  scale_color_manual(values = c("Enriched in GDM"=point_color[1],"Enriched in Control"=point_color[2],"N.S"="gray90"))+
  geom_tiplab2(aes(subset=node %in% tree_tibble_fdr_sig$node, label=label),offset=5, size=2)+
  geom_tiplab2(data = df_table,aes(subset=node %in% tree_tibble_fdr_sig$node, label=character),hjust=-0.5, size=2.5)


#figure 2e=========
library(ggplot2)

p<-ggplot(data = df_validation, mapping = aes(x = feature, y = coef, fill = cohort)) + 
  geom_rect(data = bg_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "gray90", inherit.aes = FALSE, alpha = 0.5) +
  geom_col(position = position_dodge(width = 0.9)) +            
  scale_fill_manual(values = c("Discovery cohort" = "#377483",   
                               "T2DM replication" = "#C7DFF0")) +
  geom_text(aes(x = x_pos, y = coef, label = sig),
            hjust = ifelse(merge_table_plot$coef >= 0, -0.2, 0.2),
            size = 5, color = "black")+
  
  labs(title = "",
       x = "Species",
       y = "Association with GDM/T2DM",
       fill = "") +
  theme_bw() +                                   
  theme(
    panel.grid.major = element_blank(),               
    panel.grid.minor = element_blank(),               
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  
  )+
  coord_flip()
