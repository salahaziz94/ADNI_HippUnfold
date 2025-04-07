# Descriptives

library(dplyr)
library(psych)

summary(Data_With_Slopes_March_12)

# Grouped summary statistics by Progressive_Change
grouped_stats_age <- Data_With_Slopes_March_12 %>%
  group_by(Progressive_Change) %>%
  summarize(
    Mean = mean(PHC_AGE, na.rm = TRUE),     # Mean
    Median = median(PHC_AGE, na.rm = TRUE), # Median
    SD = sd(PHC_AGE, na.rm = TRUE),         # Standard Deviation
    Min = min(PHC_AGE, na.rm = TRUE),       # Minimum
    Max = max(PHC_AGE, na.rm = TRUE)        # Maximum
  )

# View the grouped descriptive statistics
print(grouped_stats_age)

# Grouped summary statistics by Progressive_Change
grouped_stats_education <- Data_With_Slopes_March_12 %>%
  group_by(Progressive_Change) %>%
  summarize(
    Mean = mean(PHC_Education, na.rm = TRUE),     # Mean
    Median = median(PHC_Education, na.rm = TRUE), # Median
    SD = sd(PHC_Education, na.rm = TRUE),         # Standard Deviation
    Min = min(PHC_Education, na.rm = TRUE),       # Minimum
    Max = max(PHC_Education, na.rm = TRUE)        # Maximum
  )

# View the grouped descriptive statistics
print(grouped_stats_education)

# Tests -------------------------------------------------------------------
library(dplyr)
library(effectsize)
# Aggregate data by unique participant and visit, taking the mean of Age and the first instance of Progressive_Change
# Aggregate data by unique participant or visit
aggregated_data <- Data_With_Slopes_March_12 %>%
  group_by(PTID, VISCODE2) %>%
  summarise(PHC_AGE = mean(PHC_AGE, na.rm = TRUE),  # Use PHC_AGE instead of Age
            Progressive_Change = first(Progressive_Change),  # Assuming this is the same for each participant-visit combination
            .groups = 'drop')

# Perform ANOVA on the aggregated data
anova_PHC_AGE <- aov(PHC_AGE ~ Progressive_Change, data = aggregated_data)

# Display the ANOVA summary
summary(anova_PHC_AGE)




tukey_age <- TukeyHSD(anova_PHC_AGE)
print(tukey_age)
eta_squared(anova_PHC_AGE, partial = FALSE)
#Education
aggregated_data <- Data_With_Slopes_March_12 %>%
  group_by(PTID, VISCODE2) %>%
  summarise(Education = mean(Education, na.rm = TRUE), 
            Progressive_Change = first(Progressive_Change),  # Assuming this is the same for each participant-visit combination
            .groups = 'drop')


anova_education <- aov(Education ~ Progressive_Change, data = aggregated_data)
summary(anova_education)

tukey_education <- TukeyHSD(anova_education)
print(tukey_education)
eta_squared(anova_education, partial = FALSE)


#Sex 
# Create a contingency table
contingency_table <- table(Data_With_Slopes_March_12$Sex, Data_With_Slopes_March_12$Progressive_Change)

# Perform the Chi-Square test
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

# Calculate the standardized residuals
standardized_residuals <- chisq.test(contingency_table)$stdres
print(standardized_residuals)

#Cognitive Scores
aggregated_data <- Data_With_Slopes_March_12 %>%
  group_by(PTID, VISCODE2) %>%
  summarise(PHC_MEM = mean(PHC_MEM, na.rm = TRUE), 
            Progressive_Change = first(Progressive_Change),  # Assuming this is the same for each participant-visit combination
            .groups = 'drop')


anova_PHC_MEM <- aov(PHC_MEM ~ Progressive_Change, data = aggregated_data)
summary(anova_PHC_MEM)

tukey_PHC_MEM <- TukeyHSD(anova_PHC_MEM)
print(tukey_PHC_MEM)
eta_squared(anova_PHC_MEM, partial = FALSE)


aggregated_data <- Data_With_Slopes_March_12 %>%
  group_by(PTID, VISCODE2) %>%
  summarise(PHC_VSP = mean(PHC_VSP, na.rm = TRUE), 
            Progressive_Change = first(Progressive_Change),  # Assuming this is the same for each participant-visit combination
            .groups = 'drop')


anova_PHC_VSP <- aov(PHC_VSP ~ Progressive_Change, data = aggregated_data)
summary(anova_PHC_VSP)

tukey_PHC_VSP <- TukeyHSD(anova_PHC_VSP)
print(tukey_PHC_VSP)
eta_squared(anova_PHC_VSP, partial = FALSE)

aggregated_data <- Data_With_Slopes_March_12 %>%
  group_by(PTID, VISCODE2) %>%
  summarise(PHC_LAN = mean(PHC_LAN, na.rm = TRUE), 
            Progressive_Change = first(Progressive_Change),  # Assuming this is the same for each participant-visit combination
            .groups = 'drop')


anova_PHC_LAN <- aov(PHC_LAN ~ Progressive_Change, data = aggregated_data)
summary(anova_PHC_LAN)

tukey_PHC_LAN <- TukeyHSD(anova_PHC_LAN)
print(tukey_PHC_LAN)
eta_squared(anova_PHC_LAN, partial = FALSE)

aggregated_data <- Data_With_Slopes_March_12 %>%
  group_by(PTID, VISCODE2) %>%
  summarise(PHC_EXF = mean(PHC_EXF, na.rm = TRUE), 
            Progressive_Change = first(Progressive_Change),  # Assuming this is the same for each participant-visit combination
            .groups = 'drop')


anova_PHC_EXF <- aov(PHC_EXF ~ Progressive_Change, data = aggregated_data)
summary(anova_PHC_EXF)

tukey_PHC_EXF <- TukeyHSD(anova_PHC_EXF)
print(tukey_PHC_EXF)
eta_squared(anova_PHC_EXF, partial = FALSE)


# Duration

aggregated_data <- Data_With_Slopes_March_12 %>%
  group_by(PTID, VISCODE2) %>%
  summarise(Duration = mean(Duration, na.rm = TRUE), 
            Progressive_Change = first(Progressive_Change),  # Assuming this is the same for each participant-visit combination
            .groups = 'drop')


anova_Duration <- aov(Duration ~ Progressive_Change, data = aggregated_data)
summary(anova_Duration)

tukey_Duration <- TukeyHSD(anova_Duration)
print(tukey_Duration)
eta_squared(anova_Duration, partial = FALSE)
