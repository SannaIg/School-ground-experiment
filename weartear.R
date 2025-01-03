# Analysis of Wear and Tear Data
# Author: [Your Name]
# Purpose: Analyze and visualize wear and tear data for plants in different square sizes.

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(glmmTMB)
library(emmeans)

# Data Preprocessing -------------------------------------------------------

# Filter out the first measurement and check for missing values
df_WandT <- df_tot %>% 
  filter(date != "firstmeasure")
sum(is.na(df_WandT$WandT))

# Convert plant ID to a factor
df_WandT$plid <- as.factor(df_WandT$plid)

# Data Summarization and Visualization ------------------------------------

# Summarize wear and tear data and visualize with bar plots
df_WandT %>% 
  group_by(species, sqsize, date) %>% 
  summarise(
    mWandT = mean(WandT, na.rm = TRUE), 
    sd = sd(WandT, na.rm = TRUE), 
    l = sum(!is.na(WandT))
  ) %>% 
  ggplot(aes(
    x = factor(date, levels = c("june23", "23au", "nov23", "24a", "june24", "24au")), 
    y = mWandT, 
    fill = sqsize
  )) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mWandT - sd, ymax = mWandT + sd), 
                position = position_dodge(0.9), width = 0.1) +
  facet_wrap("species") +
  labs(
    title = "Mean Wear and Tear by Date",
    x = "Date",
    y = "Mean Wear and Tear"
  ) +
  theme_minimal()

# Data Transformation -----------------------------------------------------

# Normalize WandT values
df_WandT <- df_WandT %>% 
  mutate(WandT_divided = WandT / 100)

# Modeling Wear and Tear --------------------------------------------------

# Fit a GLMM with beta family
model_WandT <- glmmTMB(
  WandT_divided ~ species * date + plsize * site * species + zon * site * species + (1 | plid/id),
  family = beta_family(),
  ziformula = ~ species * date + plsize * site * species + zon * site * species + (1 | plid/id),
  data = df_WandT
)

# ANOVA for zero-inflation and conditional components
Anova(model_WandT, component = "zi")
Anova(model_WandT, component = "cond")

# Estimated marginal means and contrasts
zi_emmeans <- emmeans(model_WandT, ~ planting | species, component = "zi")
zi_contrasts <- contrast(zi_emmeans, method = "pairwise")
summary(zi_contrasts)

# Cumulative Wear and Tear ------------------------------------------------

# Calculate cumulative wear and tear
df_cumuWT <- df_WandT %>%
  filter(!is.na(WandT)) %>%
  arrange(date) %>%
  group_by(id) %>%
  mutate(cumulative_WandT = cummean(WandT))

# Heatmaps for Different Square Sizes -------------------------------------

# Define a function for generating heatmaps
generate_heatmap <- function(data, sqsize, rows, cols) {
  data %>%
    mutate(
      row = ceiling(placement / cols),
      col = (placement - 1) %% cols + 1
    ) %>%
    ggplot(aes(x = col, y = row, fill = cumulative_WandT)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "black", limits = c(0, 100)) +
    labs(
      title = paste("Cumulative Wear and Tear Heatmap for", sqsize, "x", sqsize, "Squares"),
      x = "Column",
      y = "Row"
    ) +
    theme_minimal() +
    facet_wrap(~sqid)
}

# Generate heatmaps for each square size
generate_heatmap(df_cumuWT %>% filter(sqsize == "6"), "6", 6, 12)
generate_heatmap(df_cumuWT %>% filter(sqsize == "4"), "4", 4, 8)
generate_heatmap(df_cumuWT %>% filter(sqsize == "2"), "2", 2, 4)
