setwd("/Users/alyssaantonian/Desktop/cis412/r-self-study/rProject/csv_files")
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

hiVotes <- read.csv("hiVotes.csv")
scoreVotes <- read.csv("scoreVotes.csv")
companyMetadata <- read.csv("companyMetadata.csv")

# ----- Question 1 -----

mean_hiVotes <- mean(hiVotes$hiVote, na.rm=TRUE)
sd_hiVotes <- sd(hiVotes$hiVote, na.rm=TRUE)
cat("Mean hiVote: " , mean_hiVotes, " +/- ", sd_hiVotes,"\n")

mean_scoreVotes <- mean(scoreVotes$scoreVote, na.rm=TRUE)
sd_scoreVotes <- sd(scoreVotes$scoreVote, na.rm=TRUE)

filtered_data <- subset(scoreVotes, scoreId == "5dd6e4a49a5137000450ff1e") # wellness id
mean_scoreVote_wellness <- mean(filtered_data$scoreVote, na.rm = TRUE)
sd_scoreVote_wellness <- sd(filtered_data$scoreVote, na.rm = TRUE)
filtered_data <- subset(scoreVotes, questionId == "5dd6e4a49a5137000450ff1d") # question id
mean_scoreVote_stress <- mean(filtered_data$scoreVote, na.rm = TRUE)
sd_scoreVote_stress <- sd(filtered_data$scoreVote, na.rm = TRUE)

cat("Mean scoreVote: ",mean_scoreVotes, " +/- ", sd_scoreVotes,"\n")
cat("Mean scoreVote for wellness: ", mean_scoreVote_wellness," +/- ",sd_scoreVote_wellness,"\n")
cat("Mean scoreVote for stressQ: ", mean_scoreVote_stress," +/- ",sd_scoreVote_stress,"\n")

# ----- Question 2 -----

companyMetadata <- companyMetadata %>% filter(industry != "")

top10 <- companyMetadata %>%
  group_by(industry) %>%
  summarize(numCompanies = n()) %>%
  arrange(desc(numCompanies)) %>%
  slice(1:10)

ggplot(top10, aes(x = reorder(industry, numCompanies), y = numCompanies)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(
    x = "Industry",
    y = "numCompanies",
    title = "Top 10 Most Frequent Industries"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ----- Question 3 -----

scoreMerged <- merge(scoreVotes, scoreMetadata, by = "scoreId")

ggplot(scoreMerged, aes(x = name, y = scoreVote)) +
  geom_boxplot() +
  labs(
    x = "Category",
    y = "Score Vote",
    title = "Distribution of Score Vote by Category"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ----- Question 4 -----

wellbeingId <- scoreMetadata %>%
  filter(name == "Wellbeing") %>%
  select(scoreId)

wellbeingScores <- merge(wellbeingId, scoreVotes, by = "scoreId")
avgs <- wellbeingScores %>%
  group_by(companyId) %>%
  summarize(avgsVote = mean(scoreVote, na.rm = TRUE))

highscore <- avgs %>%
  top_n(1, avgsVote)

cat("Company Id of company with highest avg. wellbeing:\n")
print(highscore)

# ----- Question 5 -----

industries <- companyMetadata %>%
  filter(industry %in% c("ARTS_ENTERTAINMENT_RECREATION", "FINANCIAL_SERVICES_INSURANCE"))
merged <- merge(industries, hiVotes, by = "companyId")

arts <- merged %>%
  filter(industry == "ARTS_ENTERTAINMENT_RECREATION") %>%
  select(hiVote)

finance <- merged %>%
  filter(industry == "FINANCIAL_SERVICES_INSURANCE") %>%
  select(hiVote)

ttest <- t.test(arts$hiVote, finance$hiVote)
cat("Avg. happiness in arts:", mean(arts$hiVote), "\n")
cat("Abg. happiness for finance:", mean(finance$hiVote), "\n")
cat("p-value:", ttest$p.value, "\n")

# ----- Question 6 -----

merged <- merge(companyMetadata, hiVotes, by = "companyId")

avgs <- merged %>%
  group_by(companyId) %>%
  summarize(avgs = mean(hiVote, na.rm = TRUE))

happiest <- avgs %>%
  filter(avgs == max(avgs))

cat("Happiest company: ", happiest$companyId, "\n")

# ----- Question 7 -----

medians <- merged %>%
  group_by(companyId) %>%
  summarize(medians = median(hiVote, na.rm = TRUE))

happiest <- medians %>%
  filter(medians == max(medians))

cat("Happiest company: ", happiest$companyId, "\n")

# ----- Question 8 -----

anova <- aov(hiVote ~ industry, data = merged)
summary(anova)

numIndustries <- companyMetadata %>%
  distinct(industry) %>%
  nrow()
cat("numIndustries:", numIndustries, "\n")

# ----- Question 9 -----

merged <- merge(scoreVotes, companyMetadata, by = "companyId")
linRegress <- lm(scoreVote ~ timezone, data = merged)
summary(linRegress)

# ----- Part 2, Question 5 -----

mergedScore <- merge(scoreVotes, scoreMetadata, by = "scoreId")
merged <- merge(mergedScore, companyMetadata, by = "companyId")

avgIndustry <- merged %>%
  group_by(industry) %>%
  summarize(avgs = mean(scoreVote, na.rm = TRUE))

highest <- avgIndustry %>%
  filter(avgs == max(avgs))

cat("Industry w/ the highest eNPS:", highest$industry, "\n")
