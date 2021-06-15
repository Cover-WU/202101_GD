# author: Cover Wu
# date: 2021-03-02
# aim: Compute the personal information.

library(tidyverse)
library(lubridate)

pair <- read_csv("participant.csv", col_types = cols(Key = col_character(),
                                             Birthday = col_date())) %>% select(Key, Birthday)
experiment <- read_csv("timeline.csv", col_types = cols(Participant = col_character())) %>% 
  rename(Key = Participant) %>% 
  select(Key) %>% distinct(Key)

experiment_date = as_date(ymd(20210112))
age <- pair %>% semi_join(experiment) %>% mutate(Exp = experiment_date,
  Age = Exp - Birthday) %>% filter(Age > days(1000))

ans <- age %>% summarise(age_max = max(Age), age_min = min(Age), age_mean = mean(Age), age_std = sd(Age))
