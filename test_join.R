#Packages
library(tidyverse)
library(readxl) 
library(purrr)
library(profootballref)

#Load
score_2018 <- profootballref::gen_tables(2018)

test_2018 <- score_2018 %>% 
  mutate(temp = gsub("+", "", Player, fixed = TRUE),
         player_name = gsub("*", "", temp, fixed = TRUE)) %>% 
  select(-Player, -temp)

full_2018 <- left_join(test_2018, draft_2018, by = "player_name")


#Will need to add in Le'Veon Bell to 2018 because he is missing due to not playing at all?
