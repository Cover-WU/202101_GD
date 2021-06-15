# author: Cover Wu
# date: 2021-01-27
# edited on: 2021-02-01
# aim: Continue to combine the data into a complete dataframe for later analysis.

# number of participants: 72.

# Section 1 ---------------------------------------------------------------

setwd("D:/Programming/R/project_Graduation_Design")
rm(list = ls()); 
load("20210126_Data.RData"); load("20210201_PersonalityData.RData")
library(tidyverse)

# label the scenario of the trials with Order.txt
order <- read_file("Order.txt") %>% str_split(pattern = "") %>% .[[1]]
participant4 <- timeline$Participant %>% rep(each = 4)
visit_scale <- visit_scale_waiting %>% mutate(Key = participant4, Scenario = factor(order)) %>% 
  mutate(Scenario = fct_recode(Scenario, 
                               "Pedestrian Zone" = "P", 
                               "Business District" = "F",
                               "Metro Station" = "M",
                               "Night Traffic" = "N"))
# compute the PRS Scale score.
visit_scale_score <- visit_scale %>% rowwise() %>% 
  mutate(Escape = mean(c_across(Escape1:Escape5)),
         Fascination = mean(c_across(Fascination1:Fascination5)),
         Coherence = mean(c_across(Coherence1:Coherence4)),
         Compatibility = mean(c_across(Compatibility1:Compatibility5))) %>% 
  select(Key, RecordedDate, Scenario, Satisfaction, Escape, Escape, Fascination, Coherence, Compatibility)

# combine score of several scales: OCEAN and ITS (upper level)
personality <- full_join(trait_ocean_score, trait_trust_score, by = "Key") %>% 
  full_join(group, by = c("Key" = "Participant")) %>% 
  mutate(`Level of Vitality` = factor(Timeline)) %>% 
  mutate(`Level of Vitality` = fct_recode(`Level of Vitality`, "Low" = "Urban0", "High" = "Urban1")) %>% 
  select(-Timeline)

# note: check the answer:
# in visit_scale, line 21.
which(visit_scale$check != 6)
# in trait_ocean, line 64.
which(trait_ocean$check1 != 2 | trait_ocean$check2 != 4)


# Section 2 ---------------------------------------------------------------

# aim: to preprocess the SVS data.
vital_raw <- read_tsv("status_raw.tsv", col_names = TRUE,
         locale = locale(encoding = "UTF-8"))

# SVS recode and reverse scoring
vital_subjective <- vital_raw %>% select(Q1_1:Q1_7) %>% 
  rename_with(~ gsub("Q1_", "vital_", .)) %>% 
  mutate(across(everything(), ~ str_replace_all(., c("^完全反对$" = "1", 
                                                     "^反对$" = "2", 
                                                     "^比较反对$" = "3", 
                                                     "^中立$" = "4",
                                                     "^比较同意$" = "5", 
                                                     "^同意$" = "6", 
                                                     "^完全同意$" = "7")) %>% 
                  as.numeric)) %>% 
  mutate(across(vital_4, ~ (8 - .)))
vital_subjective_score <- vital_subjective %>% 
  mutate(Key = timeline$Participant %>% rep(each = 2), 
         stage = c("before", "after") %>% rep(times = 72)) %>% rowwise() %>% 
  mutate(`Subjective Vitality` = mean(c_across(vital_1:vital_7)))

save(personality, vital_subjective_score, vital_subjective, visit_scale, visit_scale_score,
     trait_ocean, trait_trust, file = "20200201_Data.RData")
