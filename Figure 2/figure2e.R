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