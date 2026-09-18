library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(ggplot2)

######################
# TRIAL SUMMARY DATA #
######################

# 1. Simplify the data: One row per unique choice/trial
# This removes the extra rows created by the multiple emotion ratings
unique_choices <- data %>%
  select(Participant.Public.ID, videoset1, videoset2, Choice) %>%
  distinct() %>%
  mutate(
    v1_cat = case_when(
      str_detect(videoset1, "natpos") ~ "natpos",
      str_detect(videoset1, "natneg") ~ "natneg",
      str_detect(videoset1, "citpos") ~ "citpos",
      str_detect(videoset1, "citneg") ~ "citneg",
      TRUE ~ "other"
    ),
    v2_cat = case_when(
      str_detect(videoset2, "natpos") ~ "natpos",
      str_detect(videoset2, "natneg") ~ "natneg",
      str_detect(videoset2, "citpos") ~ "citpos",
      str_detect(videoset2, "citneg") ~ "citneg",
      TRUE ~ "other"
    ),
    picked_cat = case_when(
      str_detect(Choice, "natpos") ~ "natpos",
      str_detect(Choice, "natneg") ~ "natneg",
      str_detect(Choice, "citpos") ~ "citpos",
      str_detect(Choice, "citneg") ~ "citneg",
      TRUE ~ "other"
    ),
    # The category not picked is whichever one doesn't match the picked_cat
    not_picked_cat = if_else(picked_cat == v1_cat, v2_cat, v1_cat)
  )


head_to_head <- unique_choices %>%
  filter(picked_cat != "other", not_picked_cat != "other") %>%
  count(winner = picked_cat, loser = not_picked_cat, name = "wins") %>%
  # Name both pmin and pmax explicitly
  group_by(
    cat_a = pmin(winner, loser), 
    cat_b = pmax(winner, loser)
  ) %>%
  mutate(
    total_matchups = sum(wins),
    head_to_head_win_rate = (wins / total_matchups) * 100
  ) %>%
  ungroup() %>%
  select(-cat_a, -cat_b)

print(head_to_head)

ggplot(head_to_head, aes(x = loser, y = winner, fill = head_to_head_win_rate)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(
    aes(label = sprintf("%d wins\n(%.1f%%)", wins, head_to_head_win_rate)),
    color = "black",
    size = 3.5
  ) +
  scale_fill_gradient(
    low = "#e0f2fe", 
    high = "#0284c7", 
    name = "Win Rate (%)",
    limits = c(0, 100)
  ) +
  labs(
    title = "Head-to-Head Preference Matrix",
    subtitle = "Y-axis category beat X-axis category",
    x = "Losing Category (Not Picked)",
    y = "Winning Category (Picked)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold")
  )

