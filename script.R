# Load necessary packages
library(tidyverse)

# Load data
awards_data <- read.csv("/Users/oliverhu/Desktop/Thunder project/Data/awards_data.csv")
player_data <- read.csv("https://raw.githubusercontent.com/OliverHu726/Thunder-Project/main/player_stats.csv")

# Clean duplictes
awards_data = awards_data[!(duplicated(awards_data[c("nbapersonid", "season")]) | duplicated(awards_data[c("nbapersonid", "season")], fromLast = TRUE)), ]


# Filter data
relevant_seasons <- 2007:2021
relevant_awards <- c("All.NBA.First.Team", "All.NBA.Second.Team", "All.NBA.Third.Team", "all_star_game")

filtered_data <- awards_data %>%
  filter(season %in% relevant_seasons,
         (`All.NBA.First.Team` == 1 | `All.NBA.Second.Team` == 1 | `All.NBA.Third.Team` == 1 | all_star_game == TRUE)) %>%
  select(nbapersonid, season, All.NBA.First.Team, All.NBA.Second.Team, All.NBA.Third.Team, all_star_game)

# Join player stats
joined_data <- filtered_data %>%
  inner_join(player_data, by = c("nbapersonid" = "nbapersonid", "season" = "season"))

# Calculate average points per game per award category
avg_ppg_by_award <- joined_data %>%
  group_by(`All.NBA.First.Team`, `All.NBA.Second.Team`, `All.NBA.Third.Team`) %>%
  summarize(avg_ppg = sum(points, na.rm = TRUE)/sum(games, na.rm = TRUE))

avg_ppg_all_star <- joined_data %>%
  group_by(all_star_game) %>%
  summarize(avg_ppg = sum(points, na.rm = TRUE)/sum(games, na.rm = TRUE))


### Question 2  

# Filter data
relevant_years <- 2007
relevant_awards <- c("All.NBA.First.Team", "All.NBA.Second.Team", "All.NBA.Third.Team")

filtered_data <- awards_data %>%
  filter(season >= relevant_years, 
         `All.NBA.First.Team` == 1 | `All.NBA.Second.Team` == 1 | `All.NBA.Third.Team` == 1) %>%
  select(nbapersonid, season, All.NBA.First.Team, All.NBA.Second.Team, All.NBA.Third.Team)

# Join player stats
joined_data <- filtered_data %>%
  inner_join(player_data, by = c("nbapersonid" = "nbapersonid", "season" = "season"))

# Calculate years of experience
joined_data <- joined_data %>%
  arrange(nbapersonid, season) %>%
  group_by(nbapersonid) %>%
  mutate(first_award_season = min(season)) %>%
  ungroup() %>%
  mutate(years_of_experience = first_award_season - draftyear)

# Calculate average years of experience
average_experience <- joined_data %>%
  summarise(avg_years_of_experience = mean(years_of_experience, na.rm = TRUE))

average_experience


### Question 3

# Filter data for players with draft year 2010
draft_year_2010 <- player_data %>%
  filter(draftyear == 2010)

# Merge player_data and awards_data
merged_data <- draft_year_2010 %>%
  left_join(awards_data, by = c("nbapersonid" = "nbapersonid", "season" = "season"))

# Data Imputation
All.NBA.award = c('All.NBA.First.Team', 'All.NBA.Second.Team', 'All.NBA.Third.Team')
player.award = c('Most.Valuable.Player_rk', 'Defensive.Player.Of.The.Year_rk')
merged_data[All.NBA.award] <- lapply(merged_data[All.NBA.award], function(x) ifelse(is.na(x), 0, x))
merged_data[player.award] <- lapply(merged_data[player.award], function(x) ifelse(is.na(x), 999, x))
merged_data$all_star_game <- ifelse(is.na(merged_data$all_star_game), FALSE, merged_data$all_star_game)

# Detect and combine data for players played for multiple teams in a season
combined_data <- merged_data %>%
  group_by(nbapersonid, player, season) %>%
  summarise(across(matches("games|games_start|mins"), sum, na.rm = TRUE),
            all_star_game = any(all_star_game), 
            `All.NBA.First.Team` = max(`All.NBA.First.Team`),
            `All.NBA.Second.Team` = max(`All.NBA.Second.Team`),
            `All.NBA.Third.Team` = max(`All.NBA.Third.Team`),
            Most.Valuable.Player_rk = min(Most.Valuable.Player_rk),
            Defensive.Player.Of.The.Year_rk = min(Defensive.Player.Of.The.Year_rk)) %>%
  ungroup() 

# Fill in players out of league
combined_data <- combined_data %>%
  complete(nbapersonid, player, season, fill = list(
    games = 0,
    games_started = 0,
    mins = 0,
    all_star_game = FALSE,
    `All NBA First Team` = 0,
    `All NBA Second Team` = 0,
    `All NBA Third Team` = 0,
    Most.Valuable.Player_rk = 999,
    Defensive.Player.Of.The.Year_rk = 999
  ))

# Scale shortened seasons
combined_data <- combined_data %>%
  mutate(
    games_scaled = ifelse(season == 2011, games * (82/66),
                          ifelse(season %in% c(2019, 2020), games * (82/72), games)),
    games_start_scaled = ifelse(season == 2011, games_start * (82/66),
                               ifelse(season %in% c(2019, 2020), games_start * (82/72), games_start)),
    mins_scaled = ifelse(season == 2011, mins * (82/66),
                         ifelse(season %in% c(2019, 2020), mins * (82/72), mins))
  )

# Calculate career outcomes for each player
combined_data_outcomes <- combined_data %>%
  mutate(career_outcome = case_when(
    All.NBA.First.Team == 1 ~ "Elite",
    All.NBA.Second.Team == 1 ~ "Elite",
    All.NBA.Third.Team == 1 ~ "Elite",
    Most.Valuable.Player_rk == 1 ~ "Elite",
    Defensive.Player.Of.The.Year_rk == 1 ~ "Elite",
    all_star_game == TRUE ~ "All-Star",  
    games_start_scaled >= 41 | (mins_scaled >= 2000) ~ "Starter",
    mins_scaled >= 1000 ~ "Rotation",
    mins_scaled >= 1 & !(games_start_scaled >= 41 | (mins_scaled >= 2000)) ~ "Roster",
    TRUE ~ "Out of the League"
  ))

# Rank outcomes for each player
combined_data_outcomes <- combined_data_outcomes %>%
  mutate(outcome_rank = case_when(
    career_outcome == "Elite" ~ 1,
    career_outcome == "All-Star" ~ 2,  
    career_outcome == "Starter" ~ 3,
    career_outcome == "Rotation" ~ 4,
    career_outcome == "Roster" ~ 5,
    TRUE ~ 6
  ))

# Count the occurrences of each career outcome for each player in the remaining seasons
outcome_counts <- combined_data_outcomes %>%
  filter(season >=2014) %>%
  group_by(nbapersonid, player, career_outcome, outcome_rank) %>%
  summarise(occurrences = n()) %>%
  filter(occurrences >= 2) # Filter out outcomes that occurred less than 2 times

# Count players in each career outcome bucket
highest_outcome_counts <- outcome_counts %>%
  group_by(nbapersonid) %>%
  summarise(highest_outcome = min(outcome_rank))

outcome <- highest_outcome_counts %>%
  group_by(highest_outcome) %>%
  summarise(count = n())