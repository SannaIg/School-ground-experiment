# Load necessary libraries
library(ggplot2)
library(dplyr)
library (glmmTMB)

df_WandT<- df_tot %>% filter(date!= "firstmeasure")
sum(is.na(df_WandT$WandT))


df_WandT$plid<-as.factor(df_WandT$plid)


df_WandT %>% 
  group_by(species, sqsize, date) %>% 
  summarise(mWandT = mean(WandT, na.rm = TRUE), 
            sd = sd(WandT, na.rm = TRUE), 
            l = sum(!is.na(WandT))) %>% 
  ggplot(aes(factor(date, levels = c("june23", "23au", "nov23","24a", "june24", "24au")), mWandT, fill = sqsize)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap("species")+
  geom_errorbar(aes(ymin = mWandT - sd, ymax = mWandT + sd), 
                position = position_dodge(0.9), width = 0.1)


df_WandT_compare_plantings <- df_WandT[df_WandT$species %in% c("Cornus sanguinea", "Ribes rubrum", "Diervilla lonicera"), ]

model_compare <- glmmTMB(
  WandT/100 ~ planting  * date + sqsize + (1 | id),
  family = beta_family(),
  ziformula = ~ planting  * date + sqsize,
  data = df_WandT
)

summary(model_compare)

#Logging ´99% wear and tear to dead plants
df_WandT$WandT[is.na(df_WandT$length)] <- 99

# Replace 'WandT' with NA for rows where 'height' is NA
df_WandT$WandT[is.na(df_WandT$length)] <- NA

#Changing NAs to 0 in WandT
df_WandT$WandT[is.na(df_WandT$WandT)] <- 0

#Dividing WandT by 100, to give a value between 0 and 1
df_WandT$WandT_divided <- df_WandT$WandT / 100


#Model for wear and tear 
model_WandT <- glmmTMB(
  WandT_divided ~  species * date + plsize * site * species + zon * site * species + (1 | plid/id),
  family = beta_family(),
  ziformula = ~  species * date + plsize * site *species + zon * site * species + (1 | plid/id),
  data = df_WandT
)

summary(model_WandT)
Anova(model_WandT, component = "zi")

df_WandT$date <- relevel(df_WandT$date, ref = "june23")


# Get estimated marginal means for zero inflation part of the model
cld(emmeans(model_WandT, ~ zon|species*planting+ sqsize, component = "zi"), Letters = letters)


zi_emmeans <- emmeans(model_WandT, ~ planting | species, component = "zi")

# Contrast or pairwise comparisons
zi_contrasts <- contrast(zi_emmeans, method = "pairwise")
summary(zi_contrasts)






##Cumulative wear and tear visualization


#Calculate cumulative wear and tear
df_cumuWT <- df_WandT %>%
  filter(!is.na(WandT))
#Remove NAs when calculating cumulative wear and tear. The dead plants are not part of
#the calculation, makes sense as they should not be representing neither wear and tear
#or no wear and tear. 

# Define a lookup table for custom date order
date_order <- c("june23", "23au", "23no", "24a", "june24", "24au")
date_order_df <- data.frame(
  date = date_order,
  date_order = seq_along(date_order)
)

# Merge to add order column to df_cumuWT
df_cumuWT <- df_cumuWT %>%
  left_join(date_order_df, by = "date") %>%
  arrange(date_order)   


# Calculate cumulative WandT for each plant (grouped by id)
df_cumuWT <- df_cumuWT %>%
  arrange(id) %>%  # Sort by id before grouping
  group_by(id) %>%
  mutate(cumulative_WandT = cummean(WandT))


###
df_cumu6<-df_cumuWT %>% filter(sqsize == "6")

df_cumu6 <- df_cumu6 %>%
  mutate(row = ceiling(placement / 12), 
         col = (placement - 1) %% 12 + 1)  


  ggplot(df_cumu6, aes(x = col, y = row, fill = cumulative_WandT)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "black", limits =c(0,100)) + 
  labs(title = "Cumulative Wear and Tear Heatmap for 6x6 squares", 
       x = "Column", y = "Row") +
  theme_minimal() +
  facet_wrap(~sqid)
  
  # Step 1: Aggregate cumulative_WandT by row and col
  df_cumu6_avg <- df_cumu6 %>%
    group_by(id, row, col, sqid) %>%
    summarise(avg_WandT = mean(WandT, na.rm = TRUE)) %>%
    ungroup()
  
  # Step 2: Plot using the aggregated values
  ggplot(df_cumu6_avg, aes(x = col, y = row, fill = avg_WandT)) + 
    geom_tile() + 
    scale_fill_gradient(low = "white", high = "red") + 
    labs(title = "Average Wear and Tear Heatmap for 6x6 squares", 
         x = "Column", y = "Row") +
    theme_minimal()+
    facet_wrap(~sqid)
  
  
  
df_cumu4<-df_cumuWT %>% filter(sqsize == "4")
df_cumu4 <- df_cumu4 %>%
  mutate(row = ceiling(placement / 8),     
         col = (placement - 1) %% 8 + 1)
  
# Create heatmap with actual layout
ggplot(df_cumu4, aes(x = col, y = row, fill = cumulative_WandT)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "black", limits= c(0,100)) + 
  labs(title = "Cumulative Wear and Tear Heatmap for 4x4 Squares", 
       x = "Column", y = "Row") +
  theme_minimal() +
  facet_wrap(~sqid)


df_cumu2<-df_cumuWT %>% filter(sqsize == "2")

df_cumu2 <- df_cumu2 %>%
  mutate(row = ceiling(placement / 4),     
         col = (placement - 1) %% 4 + 1)   

# Visualize heatmap with actual layout (row/col instead of placement)
ggplot(df_cumu2, aes(x = col, y = row, fill = cumulative_WandT)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "black", limits= c(0,100)) + 
  labs(title = "Cumulative Wear and Tear Heatmap for 2x2 Squares", 
       x = "Column", y = "Row") +
  theme_minimal() +
  facet_wrap(~sqid)

  

