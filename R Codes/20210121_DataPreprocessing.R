# author: Cover Wu
# date: 2021-01-21
# aim: Preprocess the tsv files to provide a tidy data.

setwd("D:/Programming/R/project_Graduation_Design")
rm(list = ls())
library(tidyverse)

# Section 1 ---------------------------------------------------------------

# rawdata files:
  # traits_raw.tsv: OCEAN & ITS (modified on 2021/2/1 as "traits_raw_plus.tsv")
  # status_raw.tsv: SVS
  # visit_raw.tsv: PRS
  # timeline.csv: paricipants' order


# read in the rawdata from tsv file
trait_raw <- read_tsv("traits_raw_plus.tsv", col_names = TRUE,
                      locale = locale(encoding = "UTF-8"))
# pairlist of variable names and alias
trait_variables_name <- read_tsv("traits_variables_name.tsv")

# recode the label into digits, and reverse scoring.
trait_trust <- trait_raw %>% select(Key = Q2, starts_with("Q1_")) %>% 
  rename_with(~ gsub("Q1_", "trust_", .), -Key) %>% 
  mutate(across(-Key, ~ str_replace_all(., c("完全不同意" = "1",
                                         "比较反对" = "2", 
                                         "中立" = "3",
                                         "比较同意" = "4",
                                         "完全同意" = "5")) %>% 
                  as.numeric)) %>% 
  mutate(across(trust_1:trust_6, ~ (6 - .)))

# compute the average score of ITS
trait_trust_score <- trait_trust %>% rowwise() %>% 
  transmute(Key, `Interpersonal Trust` = mean(c_across(trust_1:trust_10))) %>% 
  mutate(Key = as.character(Key))


# OCEAN model score, recoding and reverse scoring.
var_ocean <- paste0(rep(c("N","C","A","O","E"), each = 8) , rep(paste(1:8), times = 5)) %>% 
  append(c("check1", "check2"))

trait_ocean <- trait_raw %>% select(Key = Q2, starts_with("Q4_")) %>% 
  rename_with(~ var_ocean, -Key) %>% 
  mutate(across(-Key, ~ str_replace_all(., c("^非常反对$" = "1",
                                             "^反对$" = "2", 
                                             "^有点反对$" = "3",
                                             "^有点同意$" = "4",
                                             "^同意$" = "5",
                                             "^非常同意$" = "6")) %>% 
                  as.numeric)) %>% 
  mutate(across(c(N4,C1,A4,A5,A7,E2,E3), ~ (7 - .)))

# compute the average score
trait_ocean_score <- trait_ocean %>% rowwise() %>% 
  transmute(Key, Neuroticism = mean(c_across(N1:N8)),
         Conscientiousness = mean(c_across(C1:C8)),
         Agreeableness = mean(c_across(A1:A8)),
         Openness = mean(c_across(O1:O8)),
         Extraversion = mean(c_across(E1:E8))) %>% 
  mutate(Key = as.character(Key))


# visiting experience (PRS): importing the file.
visit_raw <- read_tsv("visit_raw.tsv", 
                      col_types = cols(RecordedDate = col_datetime(format = "%Y/%m/%d %H:%M")),
                      locale = locale(encoding = "UTF-8"))
visit_variables_name <- read_tsv("visit_variables_name.tsv")

library(lubridate)
# rename the variables name.
var_visit <- rep(c("Escape", "Fascination", "Coherence", "Compatibility"), c(5,5,4,5)) %>% 
  paste0(c(1:5,1:5,1:4,1:5)) %>% append(c("check", "Satisfaction"))

# recode the labels into digits
visit_scale <- visit_raw %>% select(RecordedDate, starts_with("Q21_"), Q22) %>% 
  mutate(RecordedDate = RecordedDate + dhours(15)) %>% 
  rename_with(~ var_visit, -RecordedDate) %>% 
  mutate(across(Escape1:Satisfaction, ~ str_replace_all(., c("^完全反对$" = "1", "^极不满意$" = "1",
                                             "^反对$" = "2", "^一般不满意$" = "2",
                                             "^比较反对$" = "3", "有点不满意" = "3",
                                             "^中立$" = "4",
                                             "^比较同意$" = "5", "有点满意" = "5",
                                             "^同意$" = "6", "一般满意" = "6",
                                             "^完全同意$" = "7", "极为满意" = "7")) %>% 
                  as.numeric))

write_csv(visit_scale, "visit_waiting_for_label.csv")
# and then, correct the csv file manually.
# specifically, add one empty line to LiKexin
# ...

# Section 2 ---------------------------------------------------------------

# date: 2021-01-26
# aim: to add the Key and ensure the order.

# import the data again.
visit_scale_waiting <- read_csv("visit_waiting_for_label.csv")

# participant list: Key and IndependentVariable
group <- read_tsv("calibration.tsv") %>% select(Participant, Timeline) %>% distinct() %>% 
  mutate(Participant = as.character(Participant)) %>% 
  filter(Participant != "201711061127")
fix(group)
# manually correct a mistake of Key: 201731061046 -> 201731061007

timeline <- read_tsv("calibration.tsv") %>% select(Recording, Participant) %>% distinct() %>% 
  mutate(Participant = as.character(Participant))
write_csv(timeline, "timeline.csv")
# and then, correct the timeline csv manually.
# ...
timeline <- read_csv("timeline.csv", col_types = cols(Participant = col_character())) %>% 
  select(Participant) %>% distinct()

save(ans, group, timeline, trait_ocean, trait_ocean_score, trait_trust, trait_trust_score, 
     visit_scale_waiting, file = "20210126_Data.RData")
