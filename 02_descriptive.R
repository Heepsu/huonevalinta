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

get_emotion_label <- function(emotion_scale) {
  labels <- c(
    'Unpleasant-Pleasant'           = "Unpleasant – Pleasant",
    'Aroused-Calm'                  = "Calm — Aroused",
    'Relaxed Tense'                 = "Relaxed — Tense",
    'Constricted-Spacious'          = "Constricted — Spacious",
    'Wanttoremainhere-Wanttomoveon' = "Stay — Leave"
  )
  return(labels[emotion_scale])
}

# adds column environment to the data
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

# --- PREPARE DATA FOR PLOTTING --- #

target_emotions <- c("Anxiety", "Awe", "Excitement", "Fear", "Joy", "Liking")

mean_data_uni <- mean_data %>% 
  filter(`Object.Name` %in% target_emotions)

target_emotions2 <- c("Aroused-Calm", "Constricted Spacious", "Leave-Stay", "Relaxed-Tense", "Unpleasant-Pleasant", "Unsafe-Safe")

mean_data_bi <- mean_data %>% 
  filter(`Object.Name` %in% target_emotions2)

# now means are calculated for each video, but there is not that many stimuli
# would it be better to plot raw data? 
# mean response for each video
video_means <- data %>%
  group_by(Object.Name, environment, Choice) %>%
  summarize(mean_response = mean(Response, na.rm = TRUE), .groups = "drop")

video_means1 <- video_means %>% filter(`Object.Name` %in% target_emotions)
video_means2 <- video_means %>% filter(`Object.Name` %in% target_emotions2)

# --- PLOT EMOTION MEANS --- # 

plot_emotion_means <- function(df, plot_title = "") {
  
  # 1. Filter and calculate mean + standard error
  summary_data <- df %>%
   
    group_by(Object.Name) %>%
    summarize(
      mean_val = mean(mean_response, na.rm = TRUE),
      sd_val   = sd(mean_response, na.rm = TRUE),
      n        = n(),
      se_val   = sd_val / sqrt(n),
      .groups  = "drop"
    )
  
  # 2. Build plot
  ggplot(summary_data, aes(x = reorder(Object.Name, mean_val), y = mean_val)) +
    geom_col(fill = "#6D9EC1", width = 0.6, alpha = 0.85) +
    geom_errorbar(
      aes(ymin = mean_val - se_val, ymax = mean_val + se_val),
      width = 0.2,
      color = "black",
      linewidth = 0.7
    ) +
    geom_text(
      aes(label = sprintf("%.2f", mean_val)),
      hjust = -0.3,
      size = 4,
      fontface = "bold"
    ) +
    coord_flip() + # Horizontal flip makes long emotion names easy to read
    scale_y_continuous(limits = c(0, 9.5), breaks = 1:9) +
    labs(
      x = "",
      y = "Mean Response (± SE)",
      title = plot_title
    ) +
    theme_classic(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
      axis.line = element_blank(),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.5, linetype = "dashed"),
      plot.title = element_text(face = "bold", size = 14)
    )
}

# 1. Plot Unipolar Target Emotions
plot_uni <- plot_emotion_means(
  df = mean_data_uni, 
  plot_title = "Unipolar Emotion Means"
)
print(plot_uni)

# 2. Plot Bipolar Target Emotions
plot_bi <- plot_emotion_means(
  df = mean_data_bi, 
  plot_title = "Bipolar Emotion Means"
)
print(plot_bi)

# --- PLOT VIDEO MEANS --- # 

create_boxplot <- function(df){
  
  boxplot <- ggplot(df, aes(x = environment, y = mean_response, fill = environment)) +
    geom_boxplot(
      width = 0.6,
      position = position_dodge(0.8),
      outlier.shape = NA,
      alpha = 0.5
    ) +
    geom_jitter(
      position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
      color = "black", 
      shape = 21,      
      size = 2,
      alpha = 0.8
    ) +
    facet_wrap(~ Object.Name) +
    scale_y_continuous(limits = c(1, 9), breaks = 1:9) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "", y = "Mean Rating / Video", fill = "Environment") +
    theme_classic(base_size = 14, base_family = "sans") +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7), 
      axis.line = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.5, linetype = "dashed"),
      legend.position = "bottom",
      # strip.background = element_blank(),
      strip.background = element_rect(fill = NA, color = "black", linewidth = 0.7), # Box around facet titles
      #    strip.text = element_text(size = 12, face = "bold", margin = margin(t = 4, b = 4))
    )
  
  return(boxplot)
}

boxplot_uni <- create_boxplot(video_means1)
print(boxplot_uni)

boxplot_bi <- create_boxplot(video_means2)
print(boxplot_bi)


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

