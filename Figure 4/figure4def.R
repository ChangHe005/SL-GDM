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
