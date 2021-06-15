# author: Cover Wu
# date: 2021-02-18
# aim: Preprocess the eyetracking data.

rm(list = ls())
library(tidyverse)

# global eye metrics ------------------------------------------------------

# number of records: 288.
eye_global_raw <- read_tsv("UrbanVitality_20210110 Metrics_interval_edited.tsv", 
                           col_types = cols(Participant = col_character()))

eye_global <- eye_global_raw %>% mutate(Scenario = str_split(Media, "_", simplify = TRUE) %>% .[,1]) %>% 
  rename(`Level of Vitality` = Timeline, Key = Participant) %>% 
  mutate(Scenario = fct_recode(Scenario, 
                               "Pedestrian Zone" = "PedestrianZone", 
                               "Business District" = "FinancialSt",
                               "Metro Station" = "Metro",
                               "Night Traffic" = "Night"),
         `Level of Vitality` = fct_recode(`Level of Vitality`, 
                                          "High" = "Urban1", "Low" = "Urban0")) %>% 
  select(-c(Recording, TOI:Media), -Start_of_interval) %>% 
  mutate(Fixation_frequency = 1000 * Number_of_whole_fixations / Duration_of_interval,
         Duration_proportion_of_fixations = Total_duration_of_whole_fixations / Duration_of_interval,
         Saccade_frequency = 1000 * Number_of_saccades / Duration_of_interval) %>% 
  select(Key, Scenario, `Level of Vitality`, everything())

# event eye metrics -------------------------------------------------------

eye_event_raw <- read_csv("labelled_eventlist.csv", 
                           col_types = cols(Participant = col_character())) %>% 
  mutate(Scenario = str_split(Media, "_", simplify = TRUE) %>% .[,1]) %>% 
  rename(`Level of Vitality` = Timeline, Key = Participant) %>% 
  mutate(Scenario = fct_recode(Scenario, 
                               "Pedestrian Zone" = "PedestrianZone", 
                               "Business District" = "FinancialSt",
                               "Metro Station" = "Metro",
                               "Night Traffic" = "Night"),
         `Level of Vitality` = fct_recode(`Level of Vitality`, 
                                          "High" = "Urban1", "Low" = "Urban0")) %>% 
  mutate(Label_recode = fct_collapse(Label, 
                                     vital = c("person", "rider"),
                                     semivital = c("bus", "car", "truck", "train", "bicycle", "motorcycle"),
                                     no = c("building", "wall", "road", "sidewalk", "terrain","vegetation",
                                            "fence", "traffic sign", "sky", "pole", "traffic light"))) %>% 
  select(-c(Recording, TOI:Media)) %>% 
  select(Key, Scenario, `Level of Vitality`, everything())

area_ratio_computation <- read_csv("area_computation_results.csv") %>% select(-video_1) %>% 
  separate(video, sep = '_', into = c("Scenario", "Level of Vitality")) %>% 
  mutate(Scenario = fct_recode(Scenario, 
                               "Pedestrian Zone" = "PedestrianZone", 
                               "Business District" = "FinancialSt",
                               "Metro Station" = "Metro",
                               "Night Traffic" = "Night"),
         `Level of Vitality` = fct_recode(`Level of Vitality`, 
                                          "High" = "H", "Low" = "L"))

person_only = c("Metro Station", "Pedestrian Zone")
area_vital_ratio_computation <- area_ratio_computation %>% rowwise() %>% 
  mutate(Vital_area_ratio = if_else(Scenario %in% person_only, 
                                    sum(c_across(c("person", "rider"))),
                                    sum(c_across(c("person", "rider", "bus", "car", "truck", "train", "bicycle", 
                                                   "motorcycle")))))

eye_event <- eye_event_raw %>% group_by(Key, Scenario) %>% 
  summarize(Total_duration = sum(Duration[Label_recode != "unlabelled"], na.rm = TRUE),
            Vitality_attention = if_else(Scenario[1] %in% person_only, 
                                         sum(Duration[Label_recode == "vital"], na.rm = TRUE)/Total_duration,
                                         sum(Duration[Label_recode == "vital" | Label_recode == "semivital"], 
                                             na.rm = TRUE)/Total_duration),
            Average_pupil_size = mean(Average_pupil_size, na.rm = TRUE)) %>% ungroup() 


  
eye_data <- inner_join(eye_global, eye_event, by = c("Key", "Scenario")) %>% 
  left_join(area_vital_ratio_computation %>% select(-(road:unlabelled)), by = c("Scenario", "Level of Vitality")) %>% 
  mutate(Vitality_attention_standardized = Vitality_attention / Vital_area_ratio)

save(eye_data, file = "20200219_EyeData_plus.RData")
