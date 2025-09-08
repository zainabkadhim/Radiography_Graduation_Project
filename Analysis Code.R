library(readxl)
library(tidyverse)

raw_data <- read_excel("C:/Users/user/Desktop/Analysis/raw data.xlsx", sheet = "Table1", range = "$B$1:$W$34")
View(raw_data)

colnames(raw_data) <- gsub("\\s*\\r\\n|\\s+", " ", colnames(raw_data))
colnames(raw_data) <- trimws(colnames(raw_data))

# Normality Testing  

shapiro.test(raw_data$PCS)
shapiro.test(raw_data$SS)
shapiro.test(raw_data$OS)
shapiro.test(raw_data$TCS)
shapiro.test(raw_data$Q1)

# The Self-Reported Competency Levels  

# Data Summary
raw_data %>% summarize(across(c(PCS, SS, OS, TCS, Q1), mean, na.rm = TRUE, .names = "Mean_{.col}")) 
raw_data %>% summarize(across(c(PCS, SS, OS, TCS, Q1), sd, na.rm = TRUE, .names = "SD_{.col}"))
raw_data %>% summarize(across(c(PCS, SS, OS, TCS, Q1), median, na.rm = TRUE, .names = "Med_{.col}"))

# One Sample Wilcoxon Signed-Rank Test
wilcox.test(raw_data$PCS, mu = 3.9 , alternative = "less")
wilcox.test(raw_data$SS, mu = 3.9 , alternative = "less")
wilcox.test(raw_data$OS, mu = 3.9 , alternative = "less")
wilcox.test(raw_data$TCS, mu = 3.9 , alternative = "less")
wilcox.test(raw_data$Q1, mu = 3.9 , alternative = "less")

# Comparison Between Participants’ General Perception and Total Competency Scores
wilcox.test(raw_data$Q1, raw_data$TCS, alternative = "two.sided")


# Impact of Demographic and Occupational Variables on Competency

wilcox.test(raw_data$TCS ~ raw_data$`Gender `)
wilcox.test(raw_data$TCS ~ raw_data$`Education Country`)
kruskal.test(raw_data$TCS ~ raw_data$`Education Level`)
kruskal.test(raw_data$TCS ~ raw_data$`Years of Experience`)
kruskal.test(raw_data$TCS ~ raw_data$`Training Courses`)

# Factors Influencing Competency Development
wilcox.test(raw_data$TCS ~ raw_data$Q11)
wilcox.test(raw_data$TCS ~ raw_data$Q12)
wilcox.test(raw_data$TCS ~ raw_data$Q13)

raw_data <- raw_data %>% mutate(rank_TCS = (rank(TCS, ties.method = "average")))
raw_data %>% group_by(Q11) %>% summarise(sum_rank = sum(rank_TCS), mean_rank = mean(rank_TCS), n = n())
raw_data %>% group_by(Q12) %>% summarise(sum_rank = sum(rank_TCS), mean_rank = mean(rank_TCS), n = n())
raw_data %>% group_by(Q13) %>% summarise(sum_rank = sum(rank_TCS), mean_rank = mean(rank_TCS), n = n())
