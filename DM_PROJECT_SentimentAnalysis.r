library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)



# Load your data
reddit_df_2 <- read_csv("~/Desktop/Final/reddit_df 2.csv")

df <- reddit_df_2

listy<-unique(df$`Search Query`)

print(listy)

# Ensure data is filtered for the years you're interested in and contains only the necessary columns
extracted_data <- df %>%
  select(Year, Insight) %>%
  filter(Year >= 2014 & Year <= 2024)

# Count occurrences of each Insight type per year
counted_data <- extracted_data %>%
  group_by(Year, Insight) %>%
  summarise(Count = n(), .groups = 'drop')

# Spread the counts into separate columns
spread_data <- counted_data %>%
  pivot_wider(names_from = Insight, values_from = Count, values_fill = list(Count = 0))

# Add a total count per year
spread_data <- spread_data %>%
  mutate(Total = `Negative` + `Neutral` + `Positive`)

# Set seed for reproducibility
set.seed(123)

# Perform bootstrapping: sampling up to 800 entries per year, with replacement if necessary
bootstrapped_data <- spread_data %>%
  rowwise() %>%
  mutate(Sample_Size = ifelse(Total >= 1000, 1000, Total)) %>%
  do({
    data.frame(
      Year = .$Year,
      Insight = sample(
        x = c(rep("Negative", .$Negative), rep("Neutral", .$Neutral), rep("Positive", .$Positive)),
        size = .$Sample_Size,
        replace = TRUE
      )
    )
  }) %>%
  ungroup()

# Count and spread the bootstrapped samples by Insight type
final_data <- bootstrapped_data %>%
  count(Year, Insight) %>%
  spread(Insight, n, fill = 0) %>%
  mutate(Total = `Negative` + `Neutral` + `Positive`)

# Calculate ratios
ratio_data <- final_data %>%
  mutate(Negative_Ratio = Negative / Total,
         Positive_Ratio = Positive / Total,
         Neutral_Ratio = Neutral / Total) %>%
  select(Year, Negative_Ratio, Positive_Ratio, Neutral_Ratio)

print(ratio_data)

# Assuming 'ratio_data' contains the ratio data with Year, Negative_Ratio, Positive_Ratio, Neutral_Ratio columns

# Negative ratio regression
negative_model <- lm(Negative_Ratio ~ Year, data = ratio_data)

# Positive ratio regression
positive_model <- lm(Positive_Ratio ~ Year, data = ratio_data)

# Neutral ratio regression
neutral_model <- lm(Neutral_Ratio ~ Year, data = ratio_data)

# Summary of regression results
summary(negative_model)

summary(positive_model)
summary(neutral_model)


# Line plot of neutral ratio over time
ggplot(ratio_data, aes(x = Year, y = Neutral_Ratio)) +
  geom_line() +
  geom_point() +
  labs(title = "Neutral Ratio Over Time",
       x = "Year",
       y = "Neutral Ratio")

# Line plot of positive ratio over time
ggplot(ratio_data, aes(x = Year, y = Positive_Ratio)) +
  geom_line() +
  geom_point() +
  labs(title = "Positive Ratio Over Time",
       x = "Year",
       y = "Positive Ratio")

# Line plot of positive ratio over time
ggplot(ratio_data, aes(x = Year, y = Negative_Ratio)) +
  geom_line() +
  geom_point() +
  labs(title = "Negative Ratio Over Time",
       x = "Year",
       y = "Negative Ratio")



# Combine all three ratios in a single plot
combined_plot <- ggplot(ratio_data, aes(x = Year)) +
  geom_line(aes(y = Neutral_Ratio), color = "blue") +
  geom_point(aes(y = Neutral_Ratio), color = "blue") +
  geom_line(aes(y = Positive_Ratio), color = "green") +
  geom_point(aes(y = Positive_Ratio), color = "green") +
  geom_line(aes(y = Negative_Ratio), color = "red") +
  geom_point(aes(y = Negative_Ratio), color = "red") +
  labs(title = "Sentiment Ratios Over Time",
       x = "Year",
       y = "Ratio")

# Display the combined plot
print(combined_plot)


# Assume you have a data frame called 'ratio_data' with columns 'Year', 'Neutral_Ratio', 'Positive_Ratio', and 'Negative_Ratio'

# Combine all three ratios in a single plot
combined_plot3 <- ggplot(ratio_data, aes(x = Year)) +
  geom_line(aes(y = Neutral_Ratio), color = "blue") +
  geom_point(aes(y = Neutral_Ratio), color = "blue") +
  geom_line(aes(y = Positive_Ratio), color = "green") +
  geom_point(aes(y = Positive_Ratio), color = "green") +
  geom_line(aes(y = Negative_Ratio), color = "red") +
  geom_point(aes(y = Negative_Ratio), color = "red") +
  geom_smooth(aes(y = Neutral_Ratio), method = "lm", color = "blue", se = FALSE, linetype = "dotted", alpha = 0.5) +  # Dotted trend line for Neutral_Ratio
  geom_smooth(aes(y = Positive_Ratio), method = "lm", color = "green", se = FALSE, linetype = "dotted", alpha = 0.5) +  # Dotted trend line for Positive_Ratio
  geom_smooth(aes(y = Negative_Ratio), method = "lm", color = "red", se = FALSE, linetype = "dotted", alpha = 0.5) +  # Dotted trend line for Negative_Ratio
  labs(title = "Sentiment Ratios Over Time",
       x = "Year",
       y = "Ratio")

# Display the combined plot
print(combined_plot3)
aggregated_data
