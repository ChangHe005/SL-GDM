required_pkgs <- c("dplyr", "ggplot2", "cowplot", "tibble")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_pkgs, collapse = ", "),
    ". Install them before running this script.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(cowplot)
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grepl("^--file=", args)]
  if (length(file_arg) == 0) {
    return(normalizePath(getwd()))
  }
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
}

root_dir <- normalizePath(file.path(get_script_dir(), ".."), mustWork = FALSE)
data_dir <- file.path(root_dir, "data")
results_dir <- file.path(root_dir, "results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

calc_sem <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(0)
  sd(x) / sqrt(length(x))
}

safe_ttest <- function(x, y) {
  tryCatch(t.test(x, y)$p.value, error = function(e) NA_real_)
}

p_to_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "ns",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}

calc_auc <- function(time, value) {
  ord <- order(time)
  time <- time[ord]
  value <- value[ord]
  sum(diff(time) * (head(value, -1) + tail(value, -1)) / 2)
}

theme_cell_like <- theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5),
    legend.title = element_blank()
  )

sheet_name <- "ITT_LacCer"
group_order <- c("Control", "LacCer")
group_labels <- c("Control" = "DMSO", "LacCer" = "LacCer 24:1")
colors <- c("Control" = "#6F6F6F", "LacCer" = "#C0392B")
fills <- c("Control" = "#E6E6E6", "LacCer" = "#F3C7C2")

itt_list <- readRDS(file.path(data_dir, "figure5_itt_long.rds"))

raw <- itt_list[[sheet_name]] %>%
  transmute(
    ID = as.character(ID),
    Time = as.numeric(Time),
    Group = factor(as.character(Group), levels = group_order),
    Glucose = as.numeric(Glucose)
  ) %>%
  filter(!is.na(ID), !is.na(Time), !is.na(Group), !is.na(Glucose))

baseline <- raw %>%
  filter(Time == 0) %>%
  select(ID, baseline = Glucose)

ratio_df <- raw %>%
  left_join(baseline, by = "ID") %>%
  mutate(Ratio = Glucose / baseline)

summary_df <- ratio_df %>%
  group_by(Group, Time) %>%
  summarise(mean_ratio = mean(Ratio), sem_ratio = calc_sem(Ratio), .groups = "drop")

time_stats <- ratio_df %>%
  group_by(Time) %>%
  summarise(
    p_value = safe_ttest(Ratio[Group == group_order[1]], Ratio[Group == group_order[2]]),
    y = max(Ratio) * 1.08,
    .groups = "drop"
  ) %>%
  mutate(label = p_to_stars(p_value))

auc_df <- ratio_df %>%
  group_by(ID, Group) %>%
  summarise(AUC = calc_auc(Time, Ratio), .groups = "drop")

auc_summary <- auc_df %>%
  group_by(Group) %>%
  summarise(mean_auc = mean(AUC), sem_auc = calc_sem(AUC), .groups = "drop")

auc_p <- safe_ttest(
  auc_df$AUC[auc_df$Group == group_order[1]],
  auc_df$AUC[auc_df$Group == group_order[2]]
)

line_plot <- ggplot(ratio_df, aes(Time, Ratio, color = Group)) +
  geom_point(position = position_jitter(width = 1.0, height = 0), size = 2.3, alpha = 0.55) +
  geom_errorbar(
    data = summary_df,
    aes(y = mean_ratio, ymin = mean_ratio - sem_ratio, ymax = mean_ratio + sem_ratio),
    width = 3,
    linewidth = 0.65
  ) +
  geom_line(data = summary_df, aes(y = mean_ratio, group = Group), linewidth = 1.15) +
  geom_point(data = summary_df, aes(y = mean_ratio), size = 3.1, fill = "white", stroke = 1.1, shape = 21) +
  geom_text(data = time_stats, aes(x = Time, y = y, label = label), inherit.aes = FALSE, size = 4.3) +
  scale_color_manual(values = colors, labels = group_labels) +
  scale_x_continuous(breaks = c(0, 15, 30, 60, 90, 120)) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.18))) +
  labs(x = "Time (min)", y = "Glucose / baseline") +
  theme_cell_like +
  theme(
    legend.position = c(0.12, 0.1),
    legend.background = element_blank()
  )

auc_plot <- ggplot(auc_df, aes(Group, AUC, color = Group, fill = Group)) +
  geom_col(
    data = auc_summary,
    aes(y = mean_auc),
    width = 0.56,
    color = "black",
    linewidth = 0.35
  ) +
  geom_errorbar(
    data = auc_summary,
    aes(y = mean_auc, ymin = mean_auc - sem_auc, ymax = mean_auc + sem_auc),
    width = 0.14,
    linewidth = 0.65,
    color = "black"
  ) +
  geom_point(position = position_jitter(width = 0.08, height = 0), size = 2.3, alpha = 0.9) +
  annotate("segment", x = 1, xend = 2, y = max(auc_df$AUC) * 1.10, yend = max(auc_df$AUC) * 1.10, linewidth = 0.7) +
  annotate("text", x = 1.5, y = max(auc_df$AUC) * 1.16, label = paste0("p=", signif(auc_p, 3), " (", p_to_stars(auc_p), ")"), size = 4) +
  scale_color_manual(values = colors, labels = group_labels) +
  scale_fill_manual(values = fills, labels = group_labels) +
  scale_x_discrete(labels = group_labels) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.24))) +
  labs(x = NULL, y = "AUC of glucose / baseline (min)") +
  theme_cell_like +
  theme(legend.position = "none")

combined <- ggdraw() +
  draw_plot(line_plot, 0, 0, 1, 1) +
  draw_plot(auc_plot, 0.56, 0.50, 0.39, 0.42)

ggsave(file.path(results_dir, "figure5_panelD.pdf"), combined, width = 8.0, height = 5.2, device = cairo_pdf)

message("\nPanel D time course statistics")
print(time_stats %>% select(Time, p_value, label))
message("\nPanel D AUC statistics")
print(tibble::tibble(comparison = "DMSO vs LacCer 24:1", p_value = auc_p, label = p_to_stars(auc_p)))
message("\nDone. PDF file was written to: ", file.path(results_dir, "figure5_panelD.pdf"))
