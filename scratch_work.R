library(rio)
library(tidyverse)
library(ggthemes)

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
  mutate(sleeper = ifelse(((Fantasy_FantPt >= quantile(Fantasy_FantPt[waiver == 1], 0.8) & waiver == 1)), 1, 0),
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
  group_by(player_pick) %>% 
  mutate(pick_average_percent = mean(position_percent_difference),
         pick_average_percent_20 = mean(position_percent_difference_20),
         pick_average_percent_waiver = mean(position_percent_difference_waiver))

ggplot(data = work_data_2, mapping = aes(x = player_pick, y = position_percent_difference)) +
  geom_point() +
  labs(title = "Comparison used is average of each position within a year", y = "Percent better than average") +
  scale_x_continuous(name="Draft Pick", breaks = c(1, 24, 48, 72, 96, 120, 144, 168, 192, 216, 240)) +
  geom_smooth(se = F) +
  geom_hline(yintercept = 0, size = 1, color = "red") +
  theme_minimal(base_size = 18)

ggplot(data = work_data_2 %>% filter(!is.na(FantPos)), mapping = aes(x = player_pick, y = position_percent_difference)) +
  geom_point(aes(colour = FantPos)) +
  labs(title = "Comparison used is average of each position within a year", y = "Percent better than average", color = "Position") + 
  scale_x_continuous(name="Draft Pick", breaks = c(1, 24, 48, 72, 96, 120, 144, 168, 192, 216, 240)) + 
  geom_hline(yintercept = 0, size = 1, color = "red") +
  theme_minimal(base_size = 18)

ggplot(data = work_data_2 %>% filter(!is.na(FantPos)), mapping = aes(x = player_pick, y = position_percent_difference)) +
  geom_point() +
  facet_wrap(~ FantPos) +
  geom_smooth(se = F) +
  labs(title = "Comparison used is average of each position within a year", y = "Percent better than average") +
  scale_x_continuous(name="Draft Pick", breaks = c(1, 24, 48, 72, 96, 120, 144, 168, 192, 216, 240)) +
  geom_hline(yintercept = 0, size = 1, color = "red") +
  theme_minimal(base_size = 18)

ggplot(data = work_data_2 %>% filter(!is.na(FantPos)), mapping = aes(x = player_pick, y = position_percent_difference_20)) +
  geom_point() +
  facet_wrap(~ FantPos) +
  geom_smooth(se = F) +
  labs(title = "Comparison used is average of top 20% of players of that position within a year", y = "Percent better than average") +
  scale_x_continuous(name="Draft Pick", breaks = c(1, 24, 48, 72, 96, 120, 144, 168, 192, 216, 240)) + 
  geom_hline(yintercept = 0, size = 1, color = "red") +
  theme_minimal()

ggplot(data = work_data_2 %>% filter(!is.na(FantPos)), mapping = aes(x = player_pick, y = position_percent_difference_waiver)) +
  geom_point() +
  facet_wrap(~ FantPos) +
  geom_smooth(se = F) +
  labs(title = "Comparison used is average of top 20% undrafted players of that position within a year", y = "Percent better than average") +
  scale_x_continuous(name="Draft Pick", breaks = c(1, 24, 48, 72, 96, 120, 144, 168, 192, 216, 240)) + 
  geom_hline(yintercept = 0, size = 1, color = "red") +
  theme_minimal()

ggplot(data = work_data_2, mapping = aes(x = player_pick, y = pick_average_percent)) +
  geom_point() +
  labs(title = "Comparison used is average of players within a year", y = "Percent better than average") +
  scale_x_continuous(name="Draft Pick", breaks = c(1, 24, 48, 72, 96, 120, 144, 168, 192, 216, 240)) +
  geom_hline(yintercept = 0, size = 1, color = "red") +
  theme_minimal()

ggplot(data = work_data_2, mapping = aes(x = player_pick, y = pick_average_percent_20)) +
  geom_point() +
  labs(title = "Comparison used is average of top 20% of players within a year", y = "Percent better than average") +
  scale_x_continuous(name="Draft Pick", breaks = c(1, 24, 48, 72, 96, 120, 144, 168, 192, 216, 240)) +
  geom_hline(yintercept = 0, size = 1, color = "red") +
  theme_minimal()

ggplot(data = work_data_2, mapping = aes(x = player_pick, y = pick_average_percent_waiver)) +
  geom_point() +
  labs(title = "Comparison used is average of top 20% undrafted players within a year", y = "Percent better than average") +
  scale_x_continuous(name="Draft Pick", breaks = c(1, 24, 48, 72, 96, 120, 144, 168, 192, 216, 240)) +
  geom_hline(yintercept = 0, size = 1, color = "red") +
  theme_minimal()

work_data_3 <- work_data_2 %>% 
  filter(!is.na(player_pick)) %>% 
  arrange(player_pick) %>% 
  group_by(player_pick, pick_average_percent) %>% 
  nest() %>% 
  select(player_pick, pick_average_percent) %>% 
  slice(-229, -230) %>% 
  mutate(round = rep(1:(228/12), each = 12)) %>% 
  group_by(round) %>% 
  mutate(round_average_percent = mean(pick_average_percent))

ggplot(data = work_data_3, mapping = aes(x = round, y = round_average_percent)) +
  geom_point() +
  geom_hline(yintercept = 0, size = 1, color = "red") +
  labs(title = "Comparison used is average of players within a year", y = "Percent better than average") +
  scale_x_continuous(name = "Draft Round", breaks = c(1:19)) +
  geom_smooth() +
  theme_minimal()

work_data_waiver <- work_data_2 %>% 
  filter(!is.na(player_pick)) %>% 
  arrange(player_pick) %>% 
  group_by(player_pick, pick_average_percent_waiver) %>% 
  nest() %>% 
  select(player_pick, pick_average_percent_waiver) %>% 
  slice(-229, -230) %>% 
  mutate(round = rep(1:(228/12), each = 12)) %>% 
  group_by(round) %>% 
  mutate(round_average_percent = mean(pick_average_percent_waiver))

ggplot(data = work_data_waiver, mapping = aes(x = round, y = round_average_percent)) +
  geom_point() +
  geom_hline(yintercept = 0, size = 1, color = "red") +
  scale_x_continuous(name = "Draft Round", breaks = c(1:19)) +
  geom_smooth() +
  theme_minimal()
