library(nflreadr)
library(nflfastR)
library(tidyverse)

cumulative_mean_fill_missing <- function(input_vector) {
  cumulative_mean <- 0
  cumulative_count <- 0
  output_vector <- numeric(length(input_vector))
  
  for (i in 1:length(input_vector)) {
    if (!is.na(input_vector[i])) {
      cumulative_mean <- (cumulative_mean * cumulative_count + input_vector[i]) / (cumulative_count + 1)
      cumulative_count <- cumulative_count + 1
      output_vector[i] <- cumulative_mean
    } else {
      output_vector[i] <- cumulative_mean
    }
  }
  
  return(output_vector)
}

# Function to convert moneyline to decimal odds
moneyline_to_odds <- function(moneyline) {
  ifelse(moneyline > 0, 1 + (moneyline / 100), 1 - abs(100 / moneyline))
}

# Function to convert moneyline to implied probability
moneyline_to_probability <- function(moneyline) {
  ifelse(moneyline > 0, 100 / (moneyline + 100), abs(moneyline) / (abs(moneyline) + 100))
}

pbp <- load_pbp(seasons = c(2010:2023))

schedule <- load_schedules(season = c(2010:2023))

schedule$home_team[schedule$home_team == "STL"] <- "LA"
schedule$home_team[schedule$home_team == "OAK"] <- "LV"
schedule$home_team[schedule$home_team == "SD"] <- "LAC"

schedule$away_team[schedule$away_team == "STL"] <- "LA"
schedule$away_team[schedule$away_team == "OAK"] <- "LV"
schedule$away_team[schedule$away_team == "SD"] <- "LAC"

off_epa <- pbp %>% 
  group_by(season, week, posteam) %>% 
  summarize(off_epa = mean(epa, na.rm = T))

def_epa <- pbp %>% 
  group_by(season, week, defteam) %>% 
  summarize(def_epa = mean(epa, na.rm = T)) 

off_epa <- expand.grid(season = 2010:2023, week = 1:22, posteam = unique(schedule$home_team)) %>% 
  left_join(off_epa) %>% 
  group_by(season, posteam) %>% 
  arrange(week) %>% 
  mutate(off_epa = cumulative_mean_fill_missing(off_epa)) %>% 
  arrange(season, posteam) %>% 
  mutate(join_week = week - 1) 

def_epa <- expand.grid(season = 2010:2023, week = 1:22, defteam = unique(schedule$home_team)) %>% 
  left_join(def_epa) %>% 
  group_by(season, defteam) %>% 
  arrange(week) %>% 
  mutate(def_epa = cumulative_mean_fill_missing(def_epa)) %>% 
  arrange(season, defteam) %>% 
  mutate(join_week = week - 1) 

full_df <- schedule %>% 
  pivot_longer(c(away_team, home_team), names_to = "status", values_to = "team") %>%
  mutate(
    win = ifelse(status == "home_team" & sign(result) == 1, 1,
                 ifelse(status == "away_team" & sign(result) == -1, 1, 0))
  ) %>% 
  left_join(
    schedule %>% 
      pivot_longer(c(away_score, home_score), names_to = "status", values_to = "score") %>% 
      select(season, week, status, score, home_team, away_team) %>% 
      mutate(team = ifelse(status == "away_score", away_team, home_team), opponent = ifelse(status == "away_score", home_team, away_team)) %>% 
      select(-c(status, home_team, away_team)),
    by = c("season", "week", "team")
  ) %>% 
  mutate(
    moneyline = ifelse(status == "home_team", home_moneyline, away_moneyline)
  ) %>% 
  select(season, week, team, opponent, win, score, status, under_odds, over_odds, moneyline, div_game, roof, surface) %>% 
  left_join(off_epa %>% 
              ungroup() %>% 
              select(-week), by = c("season", "week" = "join_week", "team" = "posteam")) %>% 
  left_join(def_epa %>% 
              ungroup() %>% 
              select(-week), by = c("season", "week" = "join_week", "team" = "defteam" )) %>% 
  left_join(off_epa %>% 
              rename("opp_off_epa" = "off_epa") %>% 
              ungroup() %>% 
              select(-week), by = c("season", "week" = "join_week", "opponent" = "posteam")) %>% 
  left_join(def_epa %>% 
              rename("opp_def_epa" = "def_epa") %>% 
              ungroup() %>% 
              select(-week), by = c("season", "week" = "join_week", "opponent" = "defteam"))

full_df <- full_df %>% 
  mutate(ml_prob = moneyline_to_probability(moneyline),
         ml_odds = moneyline_to_odds(moneyline))

write_csv(full_df, "data/NFLGameByGameData.csv")