#Packages
library(tidyverse)
library(readxl) 
library(purrr)

#Loading data
read_excel_allsheets <- function(filename, tibble = TRUE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
draft_data_full <- read_excel_allsheets("C:\\Users\\Tomas Okal\\Downloads\\2008_2018.xlsx")


#Formating data
years <- c(2008:2018)
draft_data_full[[(2008+1)-2008]]



draft_function <- function(x) {
  
  draft_data_full`x` %>% 
    mutate(year = x) %>%
    separate(name, c("lname", "rest"), sep = ", ") %>%
    separate(rest, c("fname", "team", "position"), sep = " ") %>%
    filter(position == "QB" |
             position == "RB" |
             position == "WR" |
             position == "TE" |
             position == "PK") %>%
    mutate(player_name = paste(fname, lname, sep = " "),
           player_pick = row_number()) %>%
    select(player_name, team, position, player_pick)
  
}

draft_2010 <- draft_data_full$`2010` %>% 
  mutate(year = 2010) %>% 
  separate(name, c("lname", "rest"), sep = ", ") %>% 
  separate(rest, c("fname", "team", "position"), sep = " ") %>% 
  filter(position == "QB" | 
           position == "RB" | 
           position == "WR" | 
           position == "TE" | 
           position == "PK") %>% 
  mutate(player_name = paste(fname, lname, sep = " "),
         player_pick = row_number()) %>% 
  select(player_name, team, position, player_pick, year)

draft_function("2010")

draft_2008 <- draft_data_full$`2008` %>% 
  mutate(year = 2008) %>% 
  separate(name, c("lname", "rest"), sep = ", ") %>% 
  separate(rest, c("fname", "team", "position"), sep = " ") %>% 
  filter(position == "QB" | 
           position == "RB" | 
           position == "WR" | 
           position == "TE" | 
           position == "PK") %>% 
  mutate(player_name = paste(fname, lname, sep = " "),
         player_pick = row_number()) %>% 
  select(player_name, team, position, player_pick, year)
draft_2009 <- draft_data_full$`2009` %>% 
  mutate(year = 2009) %>% 
  separate(name...2, c("lname", "rest"), sep = ", ") %>% 
  separate(rest, c("fname", "team", "position"), sep = " ") %>% 
  filter(position == "QB" | 
           position == "RB" | 
           position == "WR" | 
           position == "TE" | 
           position == "PK") %>% 
  mutate(player_name = paste(fname, lname, sep = " "),
         player_pick = row_number()) %>% 
  select(player_name, team, position, player_pick, year)
draft_2010 <- draft_data_full$`2010` %>% 
  mutate(year = 2010) %>% 
  separate(name, c("lname", "rest"), sep = ", ") %>% 
  separate(rest, c("fname", "team", "position"), sep = " ") %>% 
  filter(position == "QB" | 
           position == "RB" | 
           position == "WR" | 
           position == "TE" | 
           position == "PK") %>% 
  mutate(player_name = paste(fname, lname, sep = " "),
         player_pick = row_number()) %>% 
  select(player_name, team, position, player_pick, year)
draft_2011 <- draft_data_full$`2011` %>% 
  mutate(year = 2011) %>% 
  separate(name, c("lname", "rest"), sep = ", ") %>% 
  separate(rest, c("fname", "team", "position"), sep = " ") %>% 
  filter(position == "QB" | 
           position == "RB" | 
           position == "WR" | 
           position == "TE" | 
           position == "PK") %>% 
  mutate(player_name = paste(fname, lname, sep = " "),
         player_pick = row_number()) %>% 
  select(player_name, team, position, player_pick, year)
draft_2012 <- draft_data_full$`2012` %>% 
  mutate(year = 2012) %>% 
  separate(name, c("lname", "rest"), sep = ", ") %>% 
  separate(rest, c("fname", "team", "position"), sep = " ") %>% 
  filter(position == "QB" | 
           position == "RB" | 
           position == "WR" | 
           position == "TE" | 
           position == "PK") %>% 
  mutate(player_name = paste(fname, lname, sep = " "),
         player_pick = row_number()) %>% 
  select(player_name, team, position, player_pick, year)
draft_2013 <- draft_data_full$`2013` %>% 
  mutate(year = 2013) %>% 
  separate(name, c("lname", "rest"), sep = ", ") %>% 
  separate(rest, c("fname", "team", "position"), sep = " ") %>% 
  filter(position == "QB" | 
           position == "RB" | 
           position == "WR" | 
           position == "TE" | 
           position == "PK") %>% 
  mutate(player_name = paste(fname, lname, sep = " "),
         player_pick = row_number()) %>% 
  select(player_name, team, position, player_pick, year)
draft_2014 <- draft_data_full$`2014` %>% 
  mutate(year = 2014) %>% 
  separate(name, c("lname", "rest"), sep = ", ") %>% 
  separate(rest, c("fname", "team", "position"), sep = " ") %>% 
  filter(position == "QB" | 
           position == "RB" | 
           position == "WR" | 
           position == "TE" | 
           position == "PK") %>% 
  mutate(player_name = paste(fname, lname, sep = " "),
         player_pick = row_number()) %>% 
  select(player_name, team, position, player_pick, year)
draft_2015 <- draft_data_full$`2015` %>% 
  mutate(year = 2015) %>% 
  separate(name, c("lname", "rest"), sep = ", ") %>% 
  separate(rest, c("fname", "team", "position"), sep = " ") %>% 
  filter(position == "QB" | 
           position == "RB" | 
           position == "WR" | 
           position == "TE" | 
           position == "PK") %>% 
  mutate(player_name = paste(fname, lname, sep = " "),
         player_pick = row_number()) %>% 
  select(player_name, team, position, player_pick, year)
draft_2016 <- draft_data_full$`2016` %>% 
  mutate(year = 2016) %>% 
  separate(name, c("lname", "rest"), sep = ", ") %>% 
  separate(rest, c("fname", "team", "position"), sep = " ") %>% 
  filter(position == "QB" | 
           position == "RB" | 
           position == "WR" | 
           position == "TE" | 
           position == "PK") %>% 
  mutate(player_name = paste(fname, lname, sep = " "),
         player_pick = row_number()) %>% 
  select(player_name, team, position, player_pick, year)
draft_2017 <- draft_data_full$`2017` %>% 
  mutate(year = 2017) %>% 
  separate(name, c("lname", "rest"), sep = ", ") %>% 
  separate(rest, c("fname", "team", "position"), sep = " ") %>% 
  filter(position == "QB" | 
           position == "RB" | 
           position == "WR" | 
           position == "TE" | 
           position == "PK") %>% 
  mutate(player_name = paste(fname, lname, sep = " "),
         player_pick = row_number()) %>% 
  select(player_name, team, position, player_pick, year)
draft_2018 <- draft_data_full$`2018` %>% 
  mutate(year = 2018) %>% 
  separate(name, c("lname", "rest"), sep = ", ") %>% 
  separate(rest, c("fname", "team", "position"), sep = " ") %>% 
  filter(position == "QB" | 
           position == "RB" | 
           position == "WR" | 
           position == "TE" | 
           position == "PK") %>% 
  mutate(player_name = paste(fname, lname, sep = " "),
         player_pick = row_number()) %>% 
  select(player_name, team, position, player_pick, year)

draft_data <- rbind(draft_2008, draft_2009, draft_2010, draft_2011, draft_2012, draft_2013, draft_2014, draft_2015, draft_2016, draft_2017, draft_2018)
rio::export(draft_data, "draft_data_new.csv")

