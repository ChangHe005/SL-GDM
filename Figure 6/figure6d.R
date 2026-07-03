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