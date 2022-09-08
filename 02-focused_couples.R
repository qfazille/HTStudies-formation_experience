library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

# Note :
#   The purpose of this script is to :
#   1. Define the couples we want to use for the analysis
#   2. Check data quality by removing some "strange couples"

# Load data
bck_x <- readRDS("02-clean_data/fu_fe.RDS")

######################
## 1. Focus couples ##
######################
{
  # We want to take into consideration the couples that have "started from scratch"
  # Meaning that the level started from 3
  focused_couples <- bck_x %>%
    filter(previous_level == 3 & level > 3) %>%
    group_by(team_id_hash, formation) %>%
    filter(formation_date == min(formation_date)) %>%
    select(team_id_hash, formation, min_formation_date = formation_date) %>%
    as.data.frame() %>%
    distinct()
  
  # Get the cleaned data only for the focused couples
  x <- focused_couples %>%
    left_join(bck_x, by = c("team_id_hash", "formation")) %>%
    filter(formation_date >= min_formation_date) %>%
    arrange(team_id_hash, formation, formation_date) %>%
    select(-min_formation_date)
}

#####################
## 2. Data quality ##
#####################
{
  # Unfortunately some couples have subscribe, un-subscribe and re-subscribe to the study
  
  #   1. Get number of day between each udpate
  x <- x %>%
    arrange(team_id_hash, formation, formation_date) %>%
    group_by(team_id_hash, formation) %>%
    mutate(days_since_last_update = formation_date - lag(formation_date)) %>%
    as.data.frame()
  
  # 2. Identify when there are "holes", 3 or 4 days expected
  #   I'll consider as "real strange couples" the ones when there were > 6 days between 2 extractions.
  #   Because having 5 or 6 days between 2 extraction can occur if the manager forgot to organize a friendly match.
  strange_couples <- x %>%
    filter(days_since_last_update > days(6)) %>%
    select(team_id_hash, formation) %>%
    distinct()
  
  # 3. Remove those strange couples (17/215 in total) (as of 7th Sept 2022)
  x <- x %>%
    anti_join(strange_couples, by = c("team_id_hash", "formation"))
  
  # 4. We have other strange couple where the duration = 0 but the level gets higher (and not from 2 to 3)
  other_strange_couples <- x %>%
    filter(duration == 0 & level > previous_level & level >= 4) %>%
    select(team_id_hash, formation) %>%
    distinct
  
  # 5. Remove those "other strange couples"
  x <- x %>%
    anti_join(other_strange_couples, by = c("team_id_hash", "formation"))
    
}

# Save the focused data
saveRDS(x, "03-focused_data/fu_fe_focused.RDS")
