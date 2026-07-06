# figure a======
p1<-ggplot(df1,aes(x=comp1,y=comp2,
                   color=group,shape=group))+
  theme_bw()+
  geom_point(size=1.8)+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+
  labs(x=paste0("P1 (",x_lable*100,"%)"),
       y=paste0("P2 (",y_lable*100,"%)"),
       title=fdr_label)+
  scale_color_manual(values = c('#8BABD3','#D7B0B0')) +
  scale_fill_manual(values = c('#8BABD3','#D7B0B0'))+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12,angle=90),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        panel.grid=element_blank()
  )+
  stat_ellipse(data=df1,geom = "polygon",level=0.95,linetype = 2,size=0.2,
               aes(fill=group),alpha=0.3,show.legend = T)

# figure b======
lipid_class_scatter_plot<-ggplot(data=lipid_or, aes(x=chain_length, y=or)) +
  labs(x="Acyl chain carbons", y="Odds ratio") +
  geom_point(aes(color=class)) +
  geom_smooth(method="loess") +
  theme_classic()+
  theme(panel.grid=element_blank(), panel.background=element_rect(fill='transparent', color='black'))

# figure c======
library(circlize)
pdf("./circos.pdf",width = 12,height = 10)
chordDiagram(mat, group = group, grid.col = grid.col,
             col=col_fun,
             big.gap = 5,transparency = 0.5,
             annotationTrack = c("grid"),
             preAllocateTracks = list(
               track.height = mm_h(1),
               track.margin = c(mm_h(1), 0))
)

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
              facing = "clockwise", niceFacing = T,  adj = c(-0.1, 0.5),cex = 0.8)
}, bg.border = NA)
legend("right",pch=20,legend=unique(variable_name_table$group),col=unique(variable_name_table$col),bty="n",cex=1,pt.cex=2,border="black")

circos.clear()
dev.off()

# figure d======
p_correlation_species_count<-ggplot (data=correlation_species)+
  geom_bar (aes(x=reorder(from,count),y=count,fill="#DF536B"), stat="identity") +
  scale_fill_manual(values = c("#DF536B"))+
  coord_flip()+
  theme_classic()+
  labs(x="",y="")+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# figure e======
p_correlation_pathway_count<-ggplot (data=correlation_pathway)+
  geom_bar (aes(x=reorder(from,count),y=count,fill="#61D04F"), stat="identity") +
  scale_fill_manual(values = c("#61D04F"))+
  coord_flip()+
  theme_classic()+
  labs(x="",y="")+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# figure f======
p_correlation_lipid_count<-ggplot (data=correlation_lipid)+
  geom_bar (aes(x=reorder(to,count),y=count,fill="#79C9D1"), stat="identity") +
  scale_fill_manual(values = c("#79C9D1"))+
  coord_flip()+
  theme_classic()+
  labs(x="",y="")+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        legend.position = "none")