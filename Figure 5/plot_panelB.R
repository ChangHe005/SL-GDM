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

panel_b_dat <- dat %>%
  filter(Group %in% c("G1", "G3")) %>%
  mutate(Group = factor(Group, levels = c("G1", "G3")))

panel_b_summary <- panel_b_dat %>%
  group_by(Group, GluCer_uM) %>%
  summarise(mean_value = mean(LacCer_corr), sem_value = calc_sem(LacCer_corr), .groups = "drop")

panel_b_stats <- panel_b_dat %>%
  group_by(GluCer_uM) %>%
  summarise(
    p_value = safe_ttest(LacCer_corr[Group == "G1"], LacCer_corr[Group == "G3"]),
    y = max(LacCer_corr) * 1.10,
    .groups = "drop"
  ) %>%
  mutate(label = p_to_stars(p_value))

p_b <- ggplot(panel_b_dat, aes(x = GluCer_uM, y = LacCer_corr, color = Group)) +
  geom_point(position = position_jitter(width = 1.2, height = 0), size = 2.4, alpha = 0.75) +
  geom_errorbar(
    data = panel_b_summary,
    aes(y = mean_value, ymin = mean_value - sem_value, ymax = mean_value + sem_value),
    width = 3,
    linewidth = 0.65
  ) +
  geom_line(data = panel_b_summary, aes(y = mean_value, group = Group), linewidth = 1.05) +
  geom_point(data = panel_b_summary, aes(y = mean_value), size = 3, fill = "white", stroke = 1.1, shape = 21) +
  geom_text(data = panel_b_stats, aes(x = GluCer_uM, y = y, label = label), inherit.aes = FALSE, size = 4) +
  scale_color_manual(values = c("G1" = "#2E86C1", "G3" = "#D95F02"), labels = c("G1" = "GlcCer+D-Gal", "G3" = "GlcCer")) +
  scale_x_continuous(breaks = c(10, 25, 50, 100)) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.16))) +
  labs(x = "GlcCer (uM)", y = "LacCer (uM, background-corrected)") +
  theme_cell_like +
  theme(legend.position = c(0.19, 0.93), legend.background = element_blank())

ggsave(file.path(results_dir, "figure5_panelB.pdf"), p_b, width = 3.4, height = 3.0, device = cairo_pdf)

message("\nPanel B statistics")
print(panel_b_stats %>% select(GluCer_uM, p_value, label))
message("\nDone. PDF file was written to: ", file.path(results_dir, "figure5_panelB.pdf"))
