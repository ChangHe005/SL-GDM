library(vegan)
library(tidyverse)
library(ggpubr)
library(patchwork)

shannon<-diversity(species_abundance_table,index = "shannon")
simpson<-diversity(species_abundance_table,index = "simpson")

alpha<-data.frame(id=rownames(species_abundance_table),shannon,simpson)
alpha_table<-merge(metadata_table,alpha,by="id")
alpha_table$GDM<-factor(alpha_table$GDM)

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
