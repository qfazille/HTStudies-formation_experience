library(dplyr)
library(tidyr)
library(tidyr)
library(lubridate)

# Note :
#   The purpose of this script is to :
#   1. extract & load
#   2. load & reshape a bit
#   3. Merge the formation used and formation experience data.frame

#######################
## 1. Extract & load ##
#######################
{
  # Unzip the data downloaded from Hattrick
  unzip(zipfile = "./01-raw_data/ht_studies_formation_experience_data_20220907.zip", exdir = "01-raw_data")
  
  # Load the 2 data.frame
  fe <- read.csv(file = "01-raw_data/FormationExperience.csv")
  fu <- read.csv(file = "01-raw_data/FormationUsePeriod.csv")
  
}

###############################
## 2. Variables type & names ##
###############################
{
  # Rename
  fe <- fe %>% rename(formation_date = date)
  
  # Convert
  fe$formation_date <- as.Date(fe$formation_date, format = "%Y-%m-%d")
  fu$match_date <- as.Date(fu$match_date, format = "%Y-%m-%d")
  
  # Remove the duration = 0 which means that the game isn't been played
  fu <- fu %>% filter(duration > 0)
}

#########################################
## 3. Add all not-used lineup by match ##
#########################################
{
  # Get all the possible lineup
  all_lineup <- data.frame(
    formation = unique(fe$formation),
    duration = 0
  )
  
  # Couple match_id_hash/match_date all lineup
  all_matches <- fu %>% select(team_id_hash, match_id_hash, match_date) %>% distinct()
  all_lineup_possibilities <- merge(all_matches, all_lineup[,"formation",drop=F])
  
  # Get the used lineup but showing also the non-used lineup
  fu <- all_lineup_possibilities %>%
    left_join(fu, by = c("team_id_hash", "match_id_hash", "match_date", "formation")) %>%
    mutate(across(c(duration), ~replace_na(.x, 0)))
}

##################################################################
## 4. Merge between the formation used and formation experience ##
##################################################################
{
  # From this used lineup table, add the lineup level at that date and the previous level available
  x <- fu %>%
    left_join(fe, by = c("team_id_hash", "formation")) %>%
    mutate(datediff = time_length(formation_date-match_date, unit = "day")) %>%
    filter(datediff >= 1) %>% # 0 not allowed because the data extraction is very early so before the match for sure
    group_by(team_id_hash, match_id_hash, match_date, formation) %>%
    mutate(min_datediff = min(datediff)) %>%
    filter(datediff == min_datediff) %>%
    select(-min_datediff) %>%
    distinct() %>%
    as.data.frame()
  
  # Add the previous level
  x <- x %>%
    group_by(team_id_hash, formation) %>%
    mutate(previous_level = lag(level, order_by = formation_date)) %>%
    as.data.frame()
}

# Save RDS
saveRDS(x, file = "02-clean_data/fu_fe.RDS")
