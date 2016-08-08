library(magrittr)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(tidyr)

save_csv <- function(chosen_league = "Championship", chosen_season = 2016) {
  require(stringr)
  
  # Saves csv from football-data.co.uk website
  # Args:
  #   chosen_league: league to be downloaded
  #   chosen_season: season to be downloaded (numeric of year that season ends)
  #
  # Returns:
  #   NA
  
  dir.create(paste0("~/Github/football-data/data/", chosen_league, "/"), showWarnings = FALSE)
  
  league_info <- read.csv("~/Github/football-data/lookups/league_lookup.csv", stringsAsFactors=FALSE) %>% 
    filter(league == chosen_league)
  
  season_str <- paste0(str_sub(chosen_season-1, -2, -1), str_sub(chosen_season, -2, -1))
  
  url <- paste0("http://www.football-data.co.uk/mmz4281/", season_str, "/", league_info$football_data_url,".csv")
  
  print(url)
  
  read.csv(url) %>%
    write.csv(paste0("~/Github/football-data/data/", chosen_league, "/", chosen_season, ".csv"))
}

load_csv <- function(chosen_league = "Championship", chosen_season = 2016) {
  paste0("~/Github/football-data/data/", chosen_league, "/", chosen_season, ".csv") %>% 
    read.csv(stringsAsFactors = FALSE) %>%
    return()
}

summary_stats <- function(chosen_league = "Championship", chosen_season = 2016) {
  
  # Load requisite files
  season_data <- paste0("~/Github/football-data/data/", chosen_league, "/", chosen_season, ".csv") %>% 
    read.csv(stringsAsFactors = FALSE) %>% 
    mutate(home_points = ifelse(FTR == "H", 3,
                                ifelse(FTR == "D", 1, 0)),
           away_points = ifelse(FTR == "A", 3,
                                ifelse(FTR == "D", 1, 0)),
           freq = 1)
  
  team_lookup <- read.csv("~/Github/football-data/lookups/team_lookup.csv", stringsAsFactors=FALSE)
  
  #Process data
  #Get Primary and Secondary statistics
  season_data %>%
    group_by(HomeTeam) %>%
    summarise(goals_for = sum(FTHG, na.rm = TRUE),
              goals_against = sum(FTAG, na.rm = TRUE),
              shots_for = sum(HS, na.rm = TRUE),
              shots_against = sum(AS, na.rm = TRUE),
              sot_for = sum(HST, na.rm = TRUE),
              sot_against = sum(AST, na.rm = TRUE),
              points = sum(home_points, na.rm = TRUE),
              played = sum(freq, na.rm = TRUE)) %>%
    rename(team = HomeTeam) %>%
    rbind(season_data %>%
            group_by(AwayTeam) %>%
            summarise(goals_for = sum(FTAG, na.rm = TRUE),
                      goals_against = sum(FTHG, na.rm = TRUE),
                      shots_for = sum(AS, na.rm = TRUE),
                      shots_against = sum(HS, na.rm = TRUE),
                      sot_for = sum(AST, na.rm = TRUE),
                      sot_against = sum(HST, na.rm = TRUE),
                      points = sum(away_points, na.rm = TRUE),
                      played = sum(freq, na.rm = TRUE)) %>%
            rename(team = AwayTeam)) %>%
    group_by(team) %>%
    summarise_each(funs(sum)) %>%
    get_ratios() %>% 
    mutate(season = chosen_season) %>%
    merge(select(team_lookup, football_data, short),
          by.x = "team", by.y = "football_data",
          all.x = TRUE) %>%
    mutate(short = ifelse(is.na(short), team, short)) %>%
    return()
}

get_ratios <- function(fbl_df) {
  
  fbl_df$gf_per_game <- fbl_df$goals_for/fbl_df$played
  fbl_df$sf_per_game <- fbl_df$shots_for/fbl_df$played
  fbl_df$sotf_per_game <- fbl_df$sot_for/fbl_df$played
  fbl_df$ga_per_game <- fbl_df$goals_against/fbl_df$played
  fbl_df$sa_per_game <- fbl_df$shots_against/fbl_df$played
  fbl_df$sota_per_game <- fbl_df$sot_against/fbl_df$played
  
  fbl_df$gd <- fbl_df$goals_for-fbl_df$goals_against
  fbl_df$sd <- fbl_df$shots_for-fbl_df$shots_against
  fbl_df$sotd <- fbl_df$sot_for-fbl_df$sot_against
  
  fbl_df$gd_per_game <- fbl_df$gd/fbl_df$played
  fbl_df$Sd_per_game <- fbl_df$sd/fbl_df$played
  fbl_df$sotd_per_game <- fbl_df$sotd/fbl_df$played
  
  fbl_df$shooting_for <- fbl_df$goals_for/fbl_df$shots_for
  fbl_df$shooting_against <- fbl_df$goals_against/fbl_df$shots_against
  fbl_df$scoring_for <- fbl_df$goals_for/fbl_df$sot_for
  fbl_df$scoring_against <- fbl_df$goals_against/fbl_df$sot_against
  
  fbl_df$ppg <- fbl_df$points/fbl_df$played
  
  if (nrow(fbl_df) == 24) {
    # Championship
    rank <- c(1:24)
    relevance <- c(rep("A",2), rep("B",4),rep("C",15), rep("D",3))
    fbl_df$position[order(-fbl_df$points, -fbl_df$gd, -fbl_df$goals_for)] <- rank
    fbl_df$relevance[order(-fbl_df$points, -fbl_df$gd, -fbl_df$goals_for)] <- relevance
  } else if (nrow(fbl_df) == 20) {
    # Premier League
    rank <- c(1:20)
    relevance <- c(rep("A",1), rep("B",3),rep("C", 13), rep("D",3))
    fbl_df$position[order(-fbl_df$points, -fbl_df$gd, -fbl_df$goals_for)] <- rank
    fbl_df$relevance[order(-fbl_df$points, -fbl_df$gd, -fbl_df$goals_for)] <- relevance
  }
  
  return(fbl_df)
}

goals_stats <- function(chosen_league = "Championship", chosen_season = 2016) {
  
  # Load requisite files
  season_data <- paste0("~/Github/football-data/data/", chosen_league, "/", chosen_season, ".csv") %>% 
    read.csv(stringsAsFactors = FALSE) %>% 
    filter(HomeTeam != "", 
           AwayTeam != "") %>% 
    mutate(home_points = ifelse(FTR == "H", 3,
                                ifelse(FTR == "D", 1, 0)),
           away_points = ifelse(FTR == "A", 3,
                                ifelse(FTR == "D", 1, 0)),
           freq = 1)
  
  team_lookup <- read.csv("~/Github/football-data/lookups/team_lookup.csv", stringsAsFactors=FALSE)
  
  #Process data
  #Get Primary and Secondary statistics
  season_data %>%
    group_by(HomeTeam) %>%
    summarise(goals_for = sum(FTHG, na.rm = TRUE),
              goals_against = sum(FTAG, na.rm = TRUE),
              points = sum(home_points, na.rm = TRUE),
              played = sum(freq, na.rm = TRUE)) %>%
    rename(team = HomeTeam) %>%
    rbind(season_data %>%
            group_by(AwayTeam) %>%
            summarise(goals_for = sum(FTAG, na.rm = TRUE),
                      goals_against = sum(FTHG, na.rm = TRUE),
                      points = sum(away_points, na.rm = TRUE),
                      played = sum(freq, na.rm = TRUE)) %>%
            rename(team = AwayTeam)) %>%
    group_by(team) %>%
    summarise_each(funs(sum)) %>%
    mutate(season = chosen_season) %>%
    merge(select(team_lookup, football_data, short),
          by.x = "team", by.y = "football_data",
          all.x = TRUE) %>%
    mutate(short = ifelse(is.na(short), team, short)) %>%
    return()
}
