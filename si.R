library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(tidytext)
library(R.utils)
library(wordcloud)

sust_data <- read.csv("D:\\MSc\\3rd sem\\SI\\Project\\New folder\\sust_behaviour.csv")

sust_lookup <- read.csv("D:\\MSc\\3rd sem\\SI\\Project\\New folder\\sust_lookup.csv")
data(stop_words)

unique(sust_data$saves_my_time)
sust_data$saves_my_time <- factor(sust_data$saves_my_time,
                                               levels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"), 
                                               ordered = TRUE)

head(sust_data)

sust_lookup
sust_data$saves_my_time

sust_think_summ_wide <- sust_data %>%
  group_by(gender, saves_my_time) %>%  # grouping by these two variables
  tally() %>%  # counting the number of responses
  mutate(perc = n / sum(n) * 100) %>%
  dplyr::select(-n) %>%
  group_by(gender) %>%
  spread(saves_my_time, perc)

sust_think_summ_hi_lo <- sust_think_summ_wide %>%
  mutate(midlow = Neutral / 2,
         midhigh = Neutral / 2) %>%
  dplyr::select(gender, midlow, midhigh, Agree, 'Strongly Agree') %>%
  gather(key = response, value = perc, 2:5) %>%
  `colnames<-`(c("gender", "response", "perc"))

sust_think_summ_hi <- sust_think_summ_hi_lo %>%
  filter(response %in% c("Strongly Agree", "Agree", "midhigh")) %>%
  mutate(response = factor(response, levels = c("Strongly Agree", "Agree", "midhigh")))

sust_think_summ_lo <- sust_think_summ_hi_lo %>%
  filter(response %in% c("midlow", "Disagree", "Strongly Disagree")) %>%
  mutate(response = factor(response, levels = c("Strongly Disagree", "Disagree", "midlow")))

# Use RColorBrewer to store a preset diverging colour palette as a vector of colour codes 
legend_pal <- brewer.pal(name = "RdBu", n = 5)

# Duplicate the middle value, remember that "Sometimes" is actually two groups, "midhigh" and "midlow"
legend_pal <- insert(legend_pal, ats = 3, legend_pal[3])

# Replace the ugly white colour for "Sometimes" with a pleasant dishwater grey
legend_pal <- gsub("#F7F7F7", "#9C9C9C", legend_pal)

# Assign names to the vector based on the colours we want for each group
names(legend_pal) <- c("Strongly Agree", "Agree", "midhigh", "midlow", "Disagree", "Strongly Disagree" )

ggplot() + 
  geom_bar(data=sust_think_summ_hi, aes(x = gender, y=perc, fill = response), stat="identity") +
  geom_bar(data=sust_think_summ_lo, aes(x = gender, y=-perc, fill = response), stat="identity") + 
  geom_hline(yintercept = 0, color =c("black")) + 
  scale_fill_manual(values = legend_pal, 
                      breaks = c("Strongly Agree", "Agree", "midhigh", "Disagree", "Strongly Disagree"),
                      labels = c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")) +
  coord_flip() + 
  labs(x = "Gender", y = "Percentage of respondents (%)") + 
  ggtitle(sust_lookup$survey_question[sust_lookup$column_title == "saves_my_time"]) +
  theme_classic() 

# Create tally by age and how often thinking about actions
sust_bubble <- sust_data %>%
  group_by(age, saves_my_time) %>%
  tally()

# Create plot
ggplot(sust_bubble, aes(x = age, y = saves_my_time)) +
  geom_point(aes(size = n)) + 
  theme_classic()


ggplot(sust_data, aes(x = age, fill = gender)) + 
  geom_bar()

gender_think_chi <- chisq.test(sust_data$gender, sust_data$saves_my_time)
gender_think_chi


