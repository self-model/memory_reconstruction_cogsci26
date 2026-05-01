source('./loadAndFormatSymmetry.R')


Exp1.n_misses <- Exp1.filtered_click_log %>%
  group_by(condition,subj_id,test_part,round) %>%
  summarise(n_misses = sum(ifelse(hit==0,1,0))) %>%
  mutate(round=ifelse(test_part=='memory',1,round))

Exp1.n_misses %>%
  group_by(condition,round,test_part) %>%
  summarise(se=sd(n_misses)/sqrt(length(n_misses)),
            n_misses=mean(n_misses)) %>%
  ggplot(aes(x=round,y=n_misses,ymin=n_misses-se,ymax=n_misses+se,fill=test_part))+
  geom_point()+
  geom_errorbar() +
  scale_x_continuous(breaks = c(1,2,3,4))+
  facet_grid(rows=~condition)

Exp1.n_misses %>%
  mutate(round=ifelse(test_part=='memory',1,round))%>%
  group_by(condition, round, test_part) %>%
  summarise(
    se = sd(n_misses) / sqrt(n()),
    n_misses = mean(n_misses),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = round, y = n_misses, 
             ymin = n_misses - se, ymax = n_misses + se, 
             color = test_part, group = test_part)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3, alpha=0.7) +
  geom_errorbar(width = 0.2) +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  scale_color_manual(
    values = c("game" = "#69b3a2", "memory" = "#404080"),
    labels = c("game" = "Game", "memory" = "Memory"),
    name = "Phase"
  ) +
  facet_grid(~ condition) +
  labs(
    x = "Round",
    y = "Number of misses"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.background = element_blank()
  )

ggsave('figures/num_misses.pdf', width=3.5,height=2.2,dpi=300)

###
# Get estimates
Exp1.estimates <- Exp1.df %>% 
  filter(
    subj_id %in% c(Exp1.asymmetric_participants[1:150],
                   Exp1.symmetric_participants[1:150]),
    test_part == 'estimate'
  ) %>%
  mutate(estimate = case_when(
    grepl('\\bfewer\\b', responses, ignore.case = TRUE) ~ 'fewer',
    grepl('\\bmore\\b', responses, ignore.case = TRUE) ~ 'more',
    TRUE ~ 'same'
  )) %>%
  select(condition, subj_id, estimate)

Exp1.touched_white <- Exp1.filtered_click_log %>%
  group_by(condition,subj_id,test_part,round) %>%
  summarise(n_misses = sum(ifelse(hit==0,1,0)))


# Combine with performance data
Exp1.performance_and_estimates <- Exp1.touched_white %>%
  filter(test_part == 'memory' | (test_part == 'game' & round == 1)) %>%
  select(condition, subj_id, test_part, n_misses) %>%
  pivot_wider(names_from = test_part, values_from = n_misses) %>%
  inner_join(Exp1.estimates, by = c("condition", "subj_id"))  # Explicit join

# Calculate means and SE for each estimate group and test part
plot_data <- Exp1.performance_and_estimates %>%
  pivot_longer(cols = c(game, memory), 
               names_to = "test_part", 
               values_to = "n_misses") %>%
  group_by(estimate, test_part) %>%
  summarise(
    mean_misses = mean(n_misses, na.rm = TRUE),
    se_misses = sd(n_misses, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )%>%
  mutate(estimate = factor(estimate, levels = c("fewer", "same", "more")))

# Plot
ggplot(plot_data, aes(x = estimate, y = mean_misses, color = test_part)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = mean_misses - se_misses, 
                    ymax = mean_misses + se_misses),
                width = 0.2,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(
    values = c("game" = "#69b3a2", "memory" = "#404080"),
                 labels = c("game" = "Game", "memory" = "Memory"),
               name = "Phase"
    ) +
  labs(x = "How many misses relative to part 1?", 
       y = "Number of misses",  # Changed ± to +/-
       color = "Phase") +
  theme_minimal() +
  theme(legend.position='none')

ggsave('figures/misses_by_insight.pdf', width=3.5,height=2.2,dpi=300)

## same, by condition

# Calculate means and SE for each estimate group and test part
plot_data <- Exp1.performance_and_estimates %>%
  pivot_longer(cols = c(game, memory), 
               names_to = "test_part", 
               values_to = "n_misses") %>%
  group_by(estimate, test_part, condition) %>%
  summarise(
    mean_misses = mean(n_misses, na.rm = TRUE),
    se_misses = sd(n_misses, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )%>%
  mutate(estimate = factor(estimate, levels = c("fewer", "same", "more")))

# Plot
pd <- position_dodge(width = 0.45)

ggplot(
  plot_data,
  aes(
    x = estimate,
    y = mean_misses,
    group = condition   # <-- key change
  )
) +
  geom_errorbar(
    aes(ymin = mean_misses - se_misses,
        ymax = mean_misses + se_misses,
        color = test_part),
    width = 0.15,
    position = pd
  ) +
  geom_point(
    aes(color = test_part),
    shape = 21,
    fill = "white",
    size = 4.2,
    stroke = 1,
    position = pd
  ) +
  geom_text(
    aes(
      label = ifelse(condition == "symmetric", "S", "A"),
      color = test_part
    ),
    position = pd,
    size = 3.3,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(game = "#69b3a2", memory = "#404080")
  ) +
  labs(
    x = "How many misses relative to part 1?",
    y = "Number of misses"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave('figures/misses_by_insight_and_condition.pdf', width=3.5,height=2.2,dpi=300)

