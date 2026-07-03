required_pkgs <- c("dplyr", "ggplot2", "tibble")
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

theme_cell_like <- theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5),
    legend.title = element_blank()
  )

dat <- readRDS(file.path(data_dir, "figure5_laccer_production.rds"))

mw_laccer <- 979.42
dat <- dat %>%
  mutate(LacCer_uM = LacCer_ng_ml / mw_laccer)

baseline <- dat %>%
  filter(Group == "G2") %>%
  summarise(b = mean(LacCer_uM, na.rm = TRUE)) %>%
  pull(b)

dat <- dat %>%
  mutate(LacCer_corr = pmax(LacCer_uM - baseline, 0))

panel_a_levels <- c("G1_0", "G3", "G1")
panel_a_labels <- c(
  "G1_0" = "GlcCer+D-Gal\n0 h",
  "G3" = "GlcCer\n48 h",
  "G1" = "GlcCer+D-Gal\n48 h"
)
panel_a_fill <- c("G1_0" = "#BDBDBD", "G3" = "#F6D6C2", "G1" = "#DCEAF7")
panel_a_color <- c("G1_0" = "#4D4D4D", "G3" = "#D95F02", "G1" = "#2E86C1")

panel_a_dat <- dat %>%
  filter((GluCer_uM == 100 & Group %in% c("G1", "G3")) | Group == "G1_0") %>%
  mutate(Group = factor(Group, levels = panel_a_levels))

panel_a_summary <- panel_a_dat %>%
  group_by(Group) %>%
  summarise(mean_value = mean(LacCer_corr), sem_value = calc_sem(LacCer_corr), .groups = "drop")

panel_a_stats <- tibble::tibble(
  group1 = c("G1_0", "G3"),
  group2 = c("G1", "G1")
) %>%
  mutate(
    p_value = mapply(
      function(a, b) {
        safe_ttest(
          panel_a_dat %>% filter(Group == a) %>% pull(LacCer_corr),
          panel_a_dat %>% filter(Group == b) %>% pull(LacCer_corr)
        )
      },
      group1,
      group2
    ),
    label = p_to_stars(p_value),
    x_start = match(group1, panel_a_levels),
    x_end = match(group2, panel_a_levels),
    y = c(max(panel_a_dat$LacCer_corr) * 1.07, max(panel_a_dat$LacCer_corr) * 0.97)
  )

p_a <- ggplot(panel_a_dat, aes(x = Group, y = LacCer_corr, color = Group, fill = Group)) +
  geom_col(
    data = panel_a_summary,
    aes(y = mean_value),
    width = 0.56,
    color = "black",
    linewidth = 0.35
  ) +
  geom_point(position = position_jitter(width = 0.08, height = 0), size = 2.4, alpha = 0.9) +
  geom_errorbar(
    data = panel_a_summary,
    aes(y = mean_value, ymin = mean_value - sem_value, ymax = mean_value + sem_value),
    width = 0.16,
    linewidth = 0.65,
    color = "black"
  ) +
  geom_segment(data = panel_a_stats, aes(x = x_start, xend = x_end, y = y, yend = y), inherit.aes = FALSE, linewidth = 0.5) +
  geom_text(data = panel_a_stats, aes(x = (x_start + x_end) / 2, y = y * 1.02, label = label), inherit.aes = FALSE, size = 4) +
  scale_fill_manual(values = panel_a_fill) +
  scale_color_manual(values = panel_a_color) +
  scale_x_discrete(labels = panel_a_labels) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.16))) +
  labs(x = NULL, y = "LacCer (uM, background-corrected)") +
  theme_cell_like +
  theme(legend.position = "none", axis.text.x = element_text(size = 10))

ggsave(file.path(results_dir, "figure5_panelA.pdf"), p_a, width = 3.1, height = 3.0, device = cairo_pdf)

message("\nPanel A statistics")
print(panel_a_stats %>% select(group1, group2, p_value, label))
message("\nDone. PDF file was written to: ", file.path(results_dir, "figure5_panelA.pdf"))
