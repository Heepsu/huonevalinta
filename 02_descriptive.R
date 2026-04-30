# includes dplyr and ggplot2
library(tidyverse)
# for non-overlapping labels 
library(ggrepel)

se <- function(x) {
  x_clean <- na.omit(x)
  if (length(x_clean) < 2) {
    return(NA) 
  }
  return(sd(x_clean) / sqrt(length(x_clean)))
}

data <- data %>%
  mutate(
    environment = substr(Choice, 1, 6)
  )

# Calculate mean and standard error of the mean (SEM) for each Object.Name
mean_data <- data %>%
  group_by(Object.Name, environment) %>%
  summarise(
    mean_response = mean(Response, na.rm = TRUE),
    std_err = se(Response),
    .groups = 'drop'
  )

write.csv(mean_data,
          file = "mean_data_huonevalinta.csv",
          row.names = FALSE)


#########
# PLOTS #
#########

target_emotions <- c("Anxiety", "Awe", "Excitement", "Fear", "Joy", "Liking")

mean_data_uni <- mean_data %>% 
  filter(`Object.Name` %in% target_emotions)

target_emotions2 <- c("Calm-Aroused", "Constricted Spacious", "Leave-Stay", "Relaxed-Tense", "Unpleasant-Pleasant", "Unsafe-Safe")

mean_data_bi <- mean_data %>% 
  filter(`Object.Name` %in% target_emotions2)

plot_mean_emotions <- ggplot(
  data = mean_data_bi, 
  aes(
    x = Object.Name,                 
    y = mean_response,              
    fill = environment           
  )
) +
  geom_bar(
    stat = "identity",           
    position = position_dodge(0.9) # Dodge/separate the bars for each environment
  ) +
  
  labs(
    title = "",
    x = "",
    y = "Mean Response",
    fill = "Environment"
  ) +
  
  theme_minimal() + 
  theme(
    # plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "bottom" 
  ) +
  
  
  scale_fill_brewer(palette = "Set2")

print(plot_mean_emotions)

############
# BOX PLOT # 
############

data_bi <- data %>% 
  filter(`Object.Name` %in% target_emotions2)

plot_raw_emotions_boxplot <- ggplot(
  data = data, 
  aes(
    x = Object.Name, 
    y = Response, 
    fill = environment 
  )
) +
  geom_boxplot(
    width = 0.8, 
    position = position_dodge(0.9) 
  ) +
  
  scale_y_continuous(
    limits = c(1, 9), 
    breaks = seq(1, 9, by = 1)
  ) +
  
  labs(
    title = "", 
    x = "",
    y = "Response", 
    fill = "Environment"
  ) +
  
  theme_minimal() + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1) # rotation of x-axis labels
  ) +
  
  scale_fill_brewer(palette = "Set2")

print(plot_raw_emotions_boxplot)

###

# mean response for each video
video_means <- data %>%
  group_by(Object.Name, environment, Choice) %>%
  summarize(mean_response = mean(Response, na.rm = TRUE), .groups = "drop")

video_means <- video_means %>% 
  filter(`Object.Name` %in% target_emotions2)

plot_raw_emotions_boxplot <- ggplot() +

  geom_boxplot(
    data = video_means,
    aes(x = Object.Name, y = mean_response, fill = environment),
    width = 0.8,
    position = position_dodge(0.9),
    outlier.shape = NA,
    alpha = 0.5
  ) +

  geom_jitter(
    data = video_means,
    aes(
      x = Object.Name, 
      y = mean_response, 
      fill = environment, 
      group = environment
    ),
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9),
    color = "black", 
    shape = 21,      
    size = 2,
    alpha = 0.8
  ) +
  scale_y_continuous(limits = c(1, 9), breaks = 1:9) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(plot_raw_emotions_boxplot)

###

# Target list 1: Discrete Emotions
plot_uni <- ggplot(mean_data_uni, aes(x = reorder(Object.Name, mean_response), y = mean_response, color = environment)) +
  # Using points and lines instead of bars for a cleaner look
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(aes(ymin = mean_response - 0.2, ymax = mean_response + 0.2), # Replace 0.2 with SE if you have it
                position = position_dodge(0.5), width = 0.2) +
  coord_flip() + # Horizontal is easier to read for lists
  labs(title = "Mean Emotional Responses",
       subtitle = "Discrete emotion categories (1-9 scale)",
       x = NULL, y = "Mean Rating", color = "Environment") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold")
  ) +
  scale_color_brewer(palette = "Dark2")

print(plot_uni)

###

# Filter for bipolar emotions
data_bi <- data %>% filter(Object.Name %in% target_emotions2)

plot_bi_boxplot <- ggplot(data_bi, aes(x = environment, y = Response, fill = environment)) +
  # Add a slight jitter to see the individual data points density
  geom_jitter(alpha = 0.1, width = 0.2, color = "grey40") + 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.5) +
  # Facet by Object.Name so each scale has its own mini-plot
  facet_wrap(~Object.Name, scales = "free_x", nrow = 2) +
  scale_y_continuous(breaks = seq(1, 9, 1), limits = c(1, 9)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribution of Dimensional Scales",
       x = NULL, y = "Rating (1-9)") +
  theme_bw() + # BW theme often looks better with facets
  theme(
    legend.position = "none", # Hide legend because x-axis labels cover it
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0) 
  )

print(plot_bi_boxplot)


##################################
# VALENCE AND AROUSAL OF STIMULI #
##################################

plot_data <- data %>%
  filter(Object.Name %in% c("Unpleasant-Pleasant", "Aroused-Calm")) %>%
  group_by(Choice, Object.Name) %>%
  summarise(Mean_Rating = mean(Response, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Object.Name, values_from = Mean_Rating)

ggplot(plot_data, aes(x = `Unpleasant-Pleasant`, y = `Aroused-Calm`)) +
  # Add quadrant lines (the midpoint is 5)
  geom_vline(xintercept = 5, linetype = "dashed", color = "gray70") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "gray70") +
  
  # Add points for each stimulus
  geom_point(color = "#2c3e50", size = 3, alpha = 0.7) +
  
  # Increase max.overlaps to show all labels
  geom_text_repel(aes(label = Choice), size = 3.5, max.overlaps = Inf) +
  
  labs(
    title = "Stimuli Distribution: Valence vs. Arousal",
    subtitle = "",
    x = "Valence (Unpleasant ↔ Pleasant)",
    y = "Arousal (Aroused ↔ Calm)"
  ) +
  scale_x_continuous(limits = c(1, 9), breaks = 1:9) +
  scale_y_continuous(limits = c(1, 9), breaks = 1:9) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )
