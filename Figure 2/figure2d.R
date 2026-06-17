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

