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