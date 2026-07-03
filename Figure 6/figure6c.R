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