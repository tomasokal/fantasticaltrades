#Packages
library(tidyverse)
library(readxl) 
library(purrr)
library(profootballref)

#Load
score_2008 <- profootballref::gen_tables(2008)
test_2008 <- score_2008 %>% 
  mutate(temp = gsub("+", "", Player, fixed = TRUE),
         player_name_temp = gsub("*", "", temp, fixed = TRUE),
         player_name = tolower(player_name_temp),
         year = 2008) %>% 
  select(-Player, -temp)
full_2008 <- left_join(test_2008, draft_2008, by = c("player_name", "year"))

score_2009 <- profootballref::gen_tables(2009)
test_2009 <- score_2009 %>% 
  mutate(temp = gsub("+", "", Player, fixed = TRUE),
         player_name_temp = gsub("*", "", temp, fixed = TRUE),
         player_name = tolower(player_name_temp),
         year = 2009) %>% 
  select(-Player, -temp)
full_2009 <- left_join(test_2009, draft_2009, by = c("player_name", "year"))

score_2010 <- profootballref::gen_tables(2010)
test_2010 <- score_2010 %>% 
  mutate(temp = gsub("+", "", Player, fixed = TRUE),
         player_name_temp = gsub("*", "", temp, fixed = TRUE),
         player_name = tolower(player_name_temp),
         year = 2010) %>% 
  select(-Player, -temp)
full_2010 <- left_join(test_2010, draft_2010, by = c("player_name", "year"))

score_2011 <- profootballref::gen_tables(2011)
test_2011 <- score_2011 %>% 
  mutate(temp = gsub("+", "", Player, fixed = TRUE),
         player_name_temp = gsub("*", "", temp, fixed = TRUE),
         player_name = tolower(player_name_temp),
         year = 2011) %>% 
  select(-Player, -temp)
full_2011 <- left_join(test_2011, draft_2011, by = c("player_name", "year"))

score_2012 <- profootballref::gen_tables(2012)
test_2012 <- score_2012 %>% 
  mutate(temp = gsub("+", "", Player, fixed = TRUE),
         player_name_temp = gsub("*", "", temp, fixed = TRUE),
         player_name = tolower(player_name_temp),
         year = 2012) %>% 
  select(-Player, -temp)
full_2012 <- left_join(test_2012, draft_2012, by = c("player_name", "year"))

score_2013 <- profootballref::gen_tables(2013)
test_2013 <- score_2013 %>% 
  mutate(temp = gsub("+", "", Player, fixed = TRUE),
         player_name_temp = gsub("*", "", temp, fixed = TRUE),
         player_name = tolower(player_name_temp),
         year = 2013) %>% 
  select(-Player, -temp)
full_2013 <- left_join(test_2013, draft_2013, by = c("player_name", "year"))

score_2014 <- profootballref::gen_tables(2014)
test_2014 <- score_2014 %>% 
  mutate(temp = gsub("+", "", Player, fixed = TRUE),
         player_name_temp = gsub("*", "", temp, fixed = TRUE),
         player_name = tolower(player_name_temp),
         year = 2014) %>% 
  select(-Player, -temp)
full_2014 <- left_join(test_2014, draft_2014, by = c("player_name", "year"))

score_2015 <- profootballref::gen_tables(2015)
test_2015 <- score_2015 %>% 
  mutate(temp = gsub("+", "", Player, fixed = TRUE),
         player_name_temp = gsub("*", "", temp, fixed = TRUE),
         player_name = tolower(player_name_temp),
         year = 2015) %>% 
  select(-Player, -temp)
full_2015 <- left_join(test_2015, draft_2015, by = c("player_name", "year"))

score_2016 <- profootballref::gen_tables(2016)
test_2016 <- score_2016 %>% 
  mutate(temp = gsub("+", "", Player, fixed = TRUE),
         player_name_temp = gsub("*", "", temp, fixed = TRUE),
         player_name = tolower(player_name_temp),
         year = 2016) %>% 
  select(-Player, -temp)
full_2016 <- left_join(test_2016, draft_2016, by = c("player_name", "year"))

score_2017 <- profootballref::gen_tables(2017)
test_2017 <- score_2017 %>% 
  mutate(temp = gsub("+", "", Player, fixed = TRUE),
         player_name_temp = gsub("*", "", temp, fixed = TRUE),
         player_name = tolower(player_name_temp),
         year = 2017) %>% 
  select(-Player, -temp)
full_2017 <- left_join(test_2017, draft_2017, by = c("player_name", "year"))

score_2018 <- profootballref::gen_tables(2018)
test_2018 <- score_2018 %>% 
  mutate(temp = gsub("+", "", Player, fixed = TRUE),
         player_name_temp = gsub("*", "", temp, fixed = TRUE),
         player_name = tolower(player_name_temp),
         year = 2018) %>% 
  add_row(player_name = "le'veon bell", year = 2018) %>% 
  select(-Player, -temp)
full_2018 <- left_join(test_2018, draft_2018, by = c("player_name", "year"))

full_draft_data <- rbind(full_2008, full_2009, full_2010, full_2011, full_2012, full_2013, full_2014, full_2015, full_2016, full_2017, full_2018)
export(full_draft_data, "full_draft_data.csv")

#Will need to add in Le'Veon Bell to 2018 because he is missing due to not playing at all?

check <- full_draft_data %>% 
  group_by(player_pick) %>% 
  summarise(count = n())
