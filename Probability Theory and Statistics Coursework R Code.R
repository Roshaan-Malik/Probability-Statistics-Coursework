library(tidyverse)
setwd("C:/Users/rosha/OneDrive/Desktop/MMU/Semester 2/Probability Theory and Statistics Coursework")
incomeFile <- read.csv("Income Adults.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Q1

#Tables

#Education
educationTable <- incomeFile %>%
  group_by(Education) %>%
  summarise(
    meanIncome = mean(Income, na.rm = TRUE),
    medianIncome = median(Income, na.rm = TRUE),
    sdIncome = sd(Income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(socEcoGroup = Education) %>% mutate(groupType = "Education")

#Marital Status
maritalStatusTable <- incomeFile %>%
  group_by(Marital.Status) %>%
  summarise(
    meanIncome = mean(Income, na.rm = TRUE),
    medianIncome = median(Income, na.rm = TRUE),
    sdIncome = sd(Income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(socEcoGroup = Marital.Status) %>% mutate(groupType = "Marital.Status")

#Gender
genderTable <- incomeFile %>%
  group_by(Gender) %>%
  summarise(
    meanIncome = mean(Income, na.rm = TRUE),
    medianIncome = median(Income, na.rm = TRUE),
    sdIncome = sd(Income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(socEcoGroup = Gender) %>% mutate(groupType = "Gender")

#Combined Table
table <- bind_rows(educationTable, maritalStatusTable, genderTable)
print(table)


#Plots

table$socEcoGroup <- factor(table$socEcoGroup, levels = table$socEcoGroup)

#Mean Plot
ggplot(table, aes(x = socEcoGroup, y = meanIncome, fill = groupType)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = seq(0, max(table$meanIncome), by = 5)) +
  labs(
    title = "Mean Income by Socio-Economic Group",
    x = "Socio-Economic Group",
    y = "Mean Income"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggsave("Mean Plot.png")

#SD Plot
ggplot(table, aes(x = socEcoGroup, y = sdIncome, fill = groupType)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = seq(0, max(table$meanIncome), by = 5)) +
  labs(
    title = "Standard Deviation Income by Socio-Economic Group",
    x = "Socio-Economic Group",
    y = "SD of Income"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggsave("SD Plot.png")

#Median Plot
ggplot(table, aes(x = socEcoGroup, y = medianIncome, fill = groupType)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(breaks = seq(0, max(table$meanIncome), by = 5)) +
  labs(
    title = "Median Income by Socio-Economic Group",
    x = "Socio-Economic Group",
    y = "Median Income"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggsave("Median Plot.png")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Q4

# Comparison of Q2 Test Statistic
incomeFile$Education <- factor(incomeFile$Education)
educationModel <- lm(Income ~ Education, data = incomeFile)
summary(educationModel)

# Comparison of Q3 F-Value
ageModel <- lm(Income ~ Age, data = incomeFile)
summary(ageModel)


# Output for above R code:
# Test statistic = 174.2, for comparison, hand-written notes had a test statistic of 173.2, almost the same.

# Output for above R code:
# F-Statistic = 0.4587, for comparison, hand-written notes had an F-Statistic of 0.6154, almost the same.

# The square of the test statistic (t^2) from Q2 redo was; t^2 = 30345.64, 
# which is close to the R code's F-statistic = 3.034e+04 (or 30340) in the summary(educationModel) output.
# Therefore, we can perform linear regressions instead of both t-tests and the ANOVA table.