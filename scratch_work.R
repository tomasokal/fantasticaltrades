library(rio)
library(tidyverse)

full_data <- import("Untitled spreadsheet - full_draft_data.csv")
full_data[full_data == ""] <- NA
full_data[, 26:29][is.na(full_data[, 26:29])] <- 0

check <- full_data %>% 
  group_by(FantPos) %>% 
  summarise(count = n())

work_data <- full_data %>% 
  mutate(player_pick = as.numeric(as.character(player_pick))) %>% 
  group_by(year, player_pick, FantPos) %>% 
  select(year, player_pick, FantPos, Fantasy_FantPt, Fantasy_PPR, Fantasy_DKPt, Fantasy_FDPt, Fantasy_OvRank, Fantasy_PosRank) %>% 
  mutate(condition = ifelse((is.na(FantPos) & is.na(player_pick)), 1, 2)) %>% 
  filter(condition == 2) %>% 
  ungroup() %>% 
  mutate(waiver = ifelse((is.na(player_pick)), 1, 0)) %>% 
  group_by(year, FantPos) %>% 
  mutate(sleeper = ifelse(((Fantasy_FantPt >= quantile(Fantasy_FantPt, 0.8)) & (waiver == 1)), 1, 0),
         position_average = mean(Fantasy_FantPt, na.rm = TRUE),
         position_difference = Fantasy_FantPt-position_average,
         position_percent_difference = round((((position_difference)/position_average)*100)),
         position_average_20 = mean(Fantasy_FantPt[Fantasy_FantPt >= quantile(Fantasy_FantPt, 0.8)], na.rm = TRUE),
         position_difference_20 = Fantasy_FantPt-position_average_20,
         position_percent_difference_20 = round((((position_difference_20)/position_average_20)*100)),
         position_average_waiver = mean(Fantasy_FantPt[sleeper==1], na.rm = TRUE),
         position_difference_waiver = Fantasy_FantPt-position_average_waiver,
         position_percent_difference_waiver = round((((position_difference_waiver)/position_average_waiver)*100)))
work_data$position_percent_difference[is.na(work_data$position_percent_difference)] <- -100
work_data$position_percent_difference_20[is.na(work_data$position_percent_difference_20)] <- -100
work_data$position_percent_difference_waiver[is.na(work_data$position_percent_difference_waiver)] <- -100

work_data_1 <- work_data

work_data_2 <- work_data_1 %>% 
  mutate(player_pick = as.numeric(as.character(player_pick))) %>%
  group_by(player_pick) %>% 
  mutate(pick_average_percent = mean(position_percent_difference),
         pick_average_percent_20 = mean(position_percent_difference_20),
         pick_average_percent_waiver = mean(position_percent_difference_waiver))

ggplot(data = work_data_2, mapping = aes(x = player_pick, y = pick_average_percent)) +
  geom_point()
ggplot(data = work_data_2, mapping = aes(x = player_pick, y = pick_average_percent_20)) +
  geom_point()
ggplot(data = work_data_2, mapping = aes(x = player_pick, y = pick_average_percent_waiver)) +
  geom_point()
