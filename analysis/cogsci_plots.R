library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggtext)

# ── Data ─────────────────────────────────────────────────────────────────────

two_parts_df_exp1 <- read.csv("../data/experiment1/two_parts_df_exp1.csv")
two_parts_df_exp2 <- read.csv("../data/experiment2/two_parts_df_exp2.csv")

dir.create("figures", showWarnings = FALSE)

# ── Constants ────────────────────────────────────────────────────────────────

p1_colour <- '#69b3a2'
p2_colour <- '#404080'
phase_pal <- c("part1" = p1_colour, "part2" = p2_colour)
pd <- position_dodge(width = 0.2)

# ── Helper functions ─────────────────────────────────────────────────────────

make_bins <- function(max_trial, bin_width = 5) {
  breaks <- seq(0.5, max_trial + 0.5, by = bin_width)
  starts <- seq(1, max_trial, by = bin_width)
  ends   <- pmin(starts + bin_width - 1, max_trial)
  labels <- paste0(starts, "–", ends)
  list(breaks = breaks, labels = labels)
}

make_phase_plot_dynamic <- function(df,
                                    max_trial,
                                    plot_subtitle = NULL,
                                    plot_title = NULL,
                                    bin_width = 5,
                                    ylim = c(0.15, 0.37)) {
  bins       <- make_bins(max_trial = max_trial, bin_width = bin_width)
  bin_labels <- bins$labels

  df_long <- df %>%
    pivot_longer(
      cols = c(chose_onlystones, chose_onlystones_p2),
      names_to = "phase_raw",
      values_to = "chose_onlystones"
    ) %>%
    mutate(
      phase = case_when(
        phase_raw == "chose_onlystones"    ~ "part1",
        phase_raw == "chose_onlystones_p2" ~ "part2",
        TRUE ~ NA_character_
      ),
      phase = factor(phase, levels = c("part1", "part2")),
      bin   = cut(trial_num, breaks = bins$breaks, labels = bin_labels,
                  include.lowest = TRUE, right = TRUE),
      bin   = factor(bin, levels = bin_labels)
    ) %>%
    filter(!is.na(phase))

  group_summary <- df_long %>%
    group_by(subj_id, phase, bin) %>%
    summarise(bias = mean(chose_onlystones, na.rm = TRUE), .groups = "drop") %>%
    group_by(phase, bin) %>%
    summarise(m = mean(bias, na.rm = TRUE),
              se = sd(bias, na.rm = TRUE) / sqrt(n()),
              .groups = "drop")

  ggplot(group_summary, aes(x = bin, y = m, color = phase, shape = phase, group = phase)) +
    geom_hline(yintercept = 1/3, linewidth = 0.3) +
    geom_line(linewidth = 0.7, position = pd) +
    geom_point(size = 2, position = pd) +
    geom_errorbar(aes(ymin = m - se, ymax = m + se),
                  width = 0.2, position = pd, linetype = "solid") +
    scale_color_manual(values = phase_pal, name = NULL,
                       labels = c("part1" = "Collect the gems!",
                                  "part2" = "Repeat your guesses!")) +
    scale_shape_discrete(name = NULL,
                         labels = c("part1" = "Collect the gems!",
                                    "part2" = "Repeat your guesses!")) +
    scale_linetype_discrete(name = NULL,
                            labels = c("part1" = "Collect the gems!",
                                       "part2" = "Repeat your guesses!")) +
    scale_y_continuous(limits = ylim) +
    labs(x = "Trial Bin", y = "P(*stones only*)",
         title = plot_title, subtitle = plot_subtitle) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_markdown(size = 12, hjust = 0.5),
      legend.position = "none",
      axis.title.y    = element_markdown(size = 14),
      axis.title.x    = element_markdown(size = 14),
      axis.text.x     = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y     = element_text(size = 12)
    )
}

fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("p < .001")
  sprintf("p = %.3f", p)
}

# ── fig1_1: P(stones only) learning curve, Exp. 1 ───────────────────────────

fig1_1 <- make_phase_plot_dynamic(
  two_parts_df_exp1 %>% filter(!is_catch_trial),
  max_trial = 60,
  plot_title = NULL,
  plot_subtitle = ""
)

ggsave("figures/fig1_1.pdf", plot = fig1_1,
       width = 5, height = 4, units = "in")

# ── fig1_2: P(remembered) by Part 1 outcome, Exp. 1 ─────────────────────────

exp1_accuracy <- two_parts_df_exp1 %>%
  filter(!is_catch_trial) %>%
  group_by(subj_id, chose_gems) %>%
  summarise(acc = mean(is_consistent), .groups = "drop") %>%
  mutate(chose_gems = factor(chose_gems)) %>%
  group_by(chose_gems) %>%
  summarise(mean_acc = mean(acc),
            se_acc   = sd(acc, na.rm = TRUE) / sqrt(n()),
            n        = n())

exp1_acc_wide <- two_parts_df_exp1 %>%
  filter(!is_catch_trial) %>%
  group_by(subj_id, chose_gems) %>%
  summarise(subj_acc = mean(is_consistent), .groups = "drop") %>%
  pivot_wider(names_from = chose_gems, values_from = subj_acc) %>%
  filter(!is.na(`TRUE`) & !is.na(`FALSE`))

exp1_acc_t <- t.test(exp1_acc_wide$`TRUE`, exp1_acc_wide$`FALSE`, paired = TRUE)

x1 <- 1; x2 <- 2
y <- 0.52; cap_h <- 0.03; text_y <- 0.47

fig1_2 <- ggplot(exp1_accuracy, aes(x = chose_gems, y = mean_acc)) +
  geom_errorbar(aes(ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
                width = 0.12, linewidth = 0.8) +
  geom_point(aes(color = chose_gems, fill = chose_gems), shape = 23, size = 3) +
  geom_hline(yintercept = 1/3, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_y_continuous(limits = c(0.3, 0.8), breaks = seq(0.1, 1.1, by = 0.2),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Part 1 Outcome", y = "P(*remembered*)") +
  scale_x_discrete(labels = c(`FALSE` = "Stones", `TRUE` = "Gems")) +
  scale_color_manual(values = c(`TRUE` = "#22C55E", `FALSE` = "grey70")) +
  scale_fill_manual(values = c(`TRUE` = "#22C55E", `FALSE` = "grey70")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.title.y = element_markdown(size = 14),
        axis.title.x = element_markdown(size = 14),
        axis.text = element_text(size = 12)) +
  geom_segment(aes(x = x1, xend = x2, y = y, yend = y),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_segment(aes(x = x1, xend = x1, y = y, yend = y + cap_h),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_segment(aes(x = x2, xend = x2, y = y, yend = y + cap_h),
               inherit.aes = FALSE, linewidth = 0.6) +
  annotate("text", x = (x1 + x2) / 2, y = text_y, label = "***",
           size = 6, vjust = 0, fontface = "bold")

ggsave("figures/fig1_2.pdf", plot = fig1_2,
       width = 2.5, height = 4, units = "in")

# ── fig2_1: P(stones only) learning curve, Exp. 2 ───────────────────────────

fig2_1 <- make_phase_plot_dynamic(
  two_parts_df_exp2 %>% filter(!is_catch_trial),
  max_trial = 50,
  plot_title = NULL,
  plot_subtitle = ""
)

ggsave("figures/fig2_1.pdf", plot = fig2_1,
       width = 5, height = 4, units = "in")

# ── fig2_2: P(remembered) + confidence by Part 1 outcome, Exp. 2 ────────────

exp2_accuracy <- two_parts_df_exp2 %>%
  filter(!is_catch_trial) %>%
  group_by(subj_id, chose_gems) %>%
  summarise(acc = mean(is_consistent), .groups = "drop") %>%
  mutate(chose_gems = factor(chose_gems)) %>%
  group_by(chose_gems) %>%
  summarise(mean_acc = mean(acc),
            se_acc   = sd(acc, na.rm = TRUE) / sqrt(n()),
            n        = n())

exp2_acc_wide <- two_parts_df_exp2 %>%
  filter(!is_catch_trial) %>%
  group_by(subj_id, chose_gems) %>%
  summarise(subj_acc = mean(is_consistent), .groups = "drop") %>%
  pivot_wider(names_from = chose_gems, values_from = subj_acc) %>%
  filter(!is.na(`TRUE`) & !is.na(`FALSE`))

exp2_acc_t <- t.test(exp2_acc_wide$`TRUE`, exp2_acc_wide$`FALSE`, paired = TRUE)

acc_y_line <- 0.55; acc_cap_h <- 0.02; acc_text_y <- 0.5

premembered_exp2_plot <- ggplot(exp2_accuracy, aes(x = chose_gems, y = mean_acc)) +
  geom_errorbar(aes(ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
                width = 0.12, linewidth = 0.8) +
  geom_point(aes(color = chose_gems, fill = chose_gems), shape = 23, size = 3) +
  geom_hline(yintercept = 1/3, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_y_continuous(limits = c(0.3, 0.8), breaks = seq(0.1, 1.1, by = 0.2),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Part 1 Outcome", y = "P(*remembered*)") +
  scale_color_manual(values = c(`TRUE` = "#22C55E", `FALSE` = "grey70")) +
  scale_fill_manual(values = c(`TRUE` = "#22C55E", `FALSE` = "grey70")) +
  scale_x_discrete(labels = c(`FALSE` = "Stones", `TRUE` = "Gems")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.title.y = element_markdown(size = 14),
        axis.title.x = element_markdown(size = 14),
        axis.text = element_text(size = 12)) +
  geom_segment(aes(x = 1, xend = 2, y = acc_y_line, yend = acc_y_line),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_segment(aes(x = 1, xend = 1, y = acc_y_line, yend = acc_y_line + acc_cap_h),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_segment(aes(x = 2, xend = 2, y = acc_y_line, yend = acc_y_line + acc_cap_h),
               inherit.aes = FALSE, linewidth = 0.6) +
  annotate("text", x = 1.5, y = acc_text_y, label = "**",
           fontface = "bold", size = 5, vjust = 0)

conf_subj_all <- two_parts_df_exp2 %>%
  filter(!is_catch_trial) %>%
  mutate(correctness = ifelse(is_consistent, "Correct", "Incorrect")) %>%
  group_by(subj_id, correctness, chose_gems) %>%
  summarise(mean_conf_subj = mean(confidence, na.rm = TRUE), .groups = "drop")

conf_summary_all <- conf_subj_all %>%
  group_by(correctness, chose_gems) %>%
  summarise(mean_conf = mean(mean_conf_subj),
            se_conf   = sd(mean_conf_subj) / sqrt(n()),
            n = n(), .groups = "drop")

conf_subj_correct <- conf_subj_all %>%
  filter(correctness == "Correct") %>%
  pivot_wider(names_from = chose_gems, values_from = mean_conf_subj) %>%
  filter(!is.na(`TRUE`) & !is.na(`FALSE`))
conf_t_correct <- t.test(conf_subj_correct$`TRUE`, conf_subj_correct$`FALSE`, paired = TRUE)

conf_subj_incorrect <- conf_subj_all %>%
  filter(correctness == "Incorrect") %>%
  pivot_wider(names_from = chose_gems, values_from = mean_conf_subj) %>%
  filter(!is.na(`TRUE`) & !is.na(`FALSE`))
conf_t_incorrect <- t.test(conf_subj_incorrect$`TRUE`, conf_subj_incorrect$`FALSE`, paired = TRUE)

x1 <- 1; x2 <- 2; x_mid <- (x1 + x2) / 2
y_correct <- 53; cap_h_correct <- 1.5; text_y_correct <- y_correct - 2.5
y_incorrect <- 55; cap_h_incorrect <- 1.5; text_y_incorrect <- y_incorrect + 2.5

conf_combined_plot <- ggplot(conf_summary_all,
                             aes(x = factor(chose_gems), y = mean_conf)) +
  geom_errorbar(aes(ymin = mean_conf - se_conf, ymax = mean_conf + se_conf),
                width = 0.12, linewidth = 0.8) +
  geom_point(aes(color = factor(chose_gems), fill = factor(chose_gems)),
             shape = 23, size = 3) +
  scale_y_continuous(name = "Confidence", limits = c(40, 70), breaks = seq(40, 70, by = 10)) +
  labs(x = "Part 1 Outcome") +
  scale_color_manual(values = c(`TRUE` = "#22C55E", `FALSE` = "grey70")) +
  scale_fill_manual(values = c(`TRUE` = "#22C55E", `FALSE` = "grey70")) +
  scale_x_discrete(labels = c(`FALSE` = "Stones", `TRUE` = "Gems")) +
  facet_wrap(~ correctness, nrow = 1,
             labeller = labeller(correctness = c("Correct" = "Correct",
                                                 "Incorrect" = "Incorrect"))) +
  theme_minimal(base_size = 12) +
  theme(legend.position  = "none",
        axis.title.y     = element_text(size = 14),
        axis.title.x     = element_markdown(size = 14),
        axis.text        = element_text(size = 12),
        strip.text       = element_text(size = 14, face = "bold"),
        strip.background = element_blank()) +
  geom_segment(data = data.frame(correctness = "Correct"),
               aes(x = x1, xend = x2, y = y_correct, yend = y_correct),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_segment(data = data.frame(correctness = "Correct"),
               aes(x = x1, xend = x1, y = y_correct, yend = y_correct + cap_h_correct),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_segment(data = data.frame(correctness = "Correct"),
               aes(x = x2, xend = x2, y = y_correct, yend = y_correct + cap_h_correct),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_text(data = data.frame(correctness = "Correct"),
            aes(x = x_mid, y = text_y_correct, label = "***", fontface = "bold"),
            inherit.aes = FALSE, size = 5) +
  geom_segment(data = data.frame(correctness = "Incorrect"),
               aes(x = x1, xend = x2, y = y_incorrect, yend = y_incorrect),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_segment(data = data.frame(correctness = "Incorrect"),
               aes(x = x1, xend = x1, y = y_incorrect, yend = y_incorrect - cap_h_incorrect),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_segment(data = data.frame(correctness = "Incorrect"),
               aes(x = x2, xend = x2, y = y_incorrect, yend = y_incorrect - cap_h_incorrect),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_text(data = data.frame(correctness = "Incorrect"),
            aes(x = x_mid, y = text_y_incorrect, label = "***", fontface = "bold"),
            inherit.aes = FALSE, size = 5)

fig2_2 <- premembered_exp2_plot + conf_combined_plot +
  plot_layout(widths = c(1, 2), axis_titles = "collect")

ggsave("figures/fig2_2.pdf", plot = fig2_2,
       width = 6, height = 3.5, units = "in")

# ── fig4: P(stones only) and P(higher number) on incorrect trials ─────────────

exp1_inconsistent <- two_parts_df_exp1 %>%
  filter(!is_consistent, !is_catch_trial, chose_gems) %>%
  group_by(subj_id) %>%
  summarise(pstone = mean(chose_onlystones_p2), .groups = "drop")

exp2_inconsistent <- two_parts_df_exp2 %>%
  filter(!is_consistent, !is_catch_trial, chose_gems) %>%
  group_by(subj_id) %>%
  summarise(pstone = mean(chose_onlystones_p2), .groups = "drop")

summary_pstones <- bind_rows(
  exp1_inconsistent %>% mutate(exp = "2"),
  exp2_inconsistent %>% mutate(exp = "3")
) %>%
  mutate(exp = factor(exp, levels = c("2", "3"))) %>%
  group_by(exp) %>%
  summarise(mean_pstone = mean(pstone, na.rm = TRUE),
            se_pstone   = sd(pstone, na.rm = TRUE) / sqrt(n()),
            n = n(), .groups = "drop")

pstones_only_plot <- ggplot(summary_pstones, aes(x = exp, y = mean_pstone)) +
  geom_errorbar(aes(ymin = mean_pstone - se_pstone, ymax = mean_pstone + se_pstone),
                width = 0.12, linewidth = 0.8) +
  geom_point(shape = 23, size = 3, fill = "black", color = "black") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_y_continuous(limits = c(0.3, 0.8), breaks = seq(0.1, 1.1, by = 0.1),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Experiment", y = "P(*stones only*)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.title.y = element_markdown(size = 14),
        axis.title.x = element_markdown(size = 14),
        axis.text = element_text(size = 12)) +
  geom_text(data = summary_pstones %>% mutate(label = "***",
                                               y = mean_pstone + se_pstone + 0.01),
            aes(x = exp, y = y, label = label),
            inherit.aes = FALSE, size = 6, vjust = 0, fontface = "bold")

make_higher_df <- function(df) {
  df %>%
    group_by(subj_id) %>%
    filter(!is_catch_trial, !is_consistent) %>%
    mutate(
      num_chosen = case_when(
        response_position_p2 == "middle" ~ middle_number,
        response_position_p2 == "left"   ~ left_number,
        response_position_p2 == "right"  ~ right_number
      ),
      num_unchosen = case_when(
        response_position_p2 == "middle" & response_position == "left"   ~ right_number,
        response_position_p2 == "middle" & response_position == "right"  ~ left_number,
        response_position_p2 == "right"  & response_position == "left"   ~ middle_number,
        response_position_p2 == "right"  & response_position == "middle" ~ left_number,
        response_position_p2 == "left"   & response_position == "middle" ~ right_number,
        response_position_p2 == "left"   & response_position == "right"  ~ middle_number
      )
    ) %>%
    filter(num_chosen != num_unchosen) %>%
    mutate(higher = num_chosen > num_unchosen) %>%
    group_by(subj_id) %>%
    summarise(p_higher = mean(higher, na.rm = TRUE), .groups = "drop")
}

summary_higher <- bind_rows(
  make_higher_df(two_parts_df_exp1) %>% mutate(exp = "2"),
  make_higher_df(two_parts_df_exp2) %>% mutate(exp = "3")
) %>%
  mutate(exp = factor(exp, levels = c("2", "3"))) %>%
  group_by(exp) %>%
  summarise(mean_ph = mean(p_higher, na.rm = TRUE),
            se_ph   = sd(p_higher, na.rm = TRUE) / sqrt(n()),
            n = n(), .groups = "drop")

phigher_number_plot <- ggplot(summary_higher, aes(x = exp, y = mean_ph)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_errorbar(aes(ymin = mean_ph - se_ph, ymax = mean_ph + se_ph),
                width = 0.12, linewidth = 0.8) +
  geom_point(shape = 23, size = 3, fill = "black", color = "black") +
  scale_y_continuous(limits = c(0.3, 0.8), breaks = seq(0.1, 1.1, 0.1),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Experiment", y = "P(*higher number*)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.title.y = element_markdown(size = 14),
        axis.title.x = element_markdown(size = 14),
        axis.text = element_text(size = 12)) +
  geom_text(data = summary_higher %>% mutate(label = "***",
                                              y = mean_ph + se_ph + 0.01),
            aes(x = exp, y = y, label = label),
            inherit.aes = FALSE, size = 6, vjust = 0, fontface = "bold")

fig4 <- pstones_only_plot + phigher_number_plot + plot_layout(axis_titles = "collect")

ggsave("figures/fig4.pdf", plot = fig4,
       width = 8 * 2/3, height = 3, units = "in")
