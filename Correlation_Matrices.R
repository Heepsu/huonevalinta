library(dplyr)
library(tidyr)
library(ggcorrplot) 

plot_emotion_correlations <- function(df, target_emotions, plot_title = "") {
  
  # Reshape data
  wide_data <- df %>%
    filter(Object.Name %in% target_emotions) %>%
    group_by(Participant.Public.ID, Object.Name) %>% 
    summarize(Response = mean(Response, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Object.Name, values_from = Response) %>%
    select(-Participant.Public.ID)
  
  # Compute correlations & p-values
  corr_matrix <- cor(wide_data, use = "pairwise.complete.obs")
  p_mat <- cor_pmat(wide_data)
  
  corr_plot <- ggcorrplot(
    corr_matrix,
    p.mat = p_mat,
    # options: lower, upper, full
    type = "full",            
    lab = TRUE,                   
    lab_size = 4,
    method = "square",
    sig.level = 0.05,             
    insig = "pch",                
    colors = c("#6D9EC1", "white", "#B2182B"),
    title = plot_title,
    ggtheme = theme_minimal(base_size = 14)
  ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  return(corr_plot)
}


# Plot for unipolar scales
plot_emotions_1 <- plot_emotion_correlations(
  df = data, 
  target_emotions = target_emotions, 
  plot_title = ""
)
print(plot_emotions_1)

# Plot for bipolar scales 
plot_emotions_2 <- plot_emotion_correlations(
  df = data, 
  target_emotions = target_emotions2, 
  plot_title = ""
)
print(plot_emotions_2)
