library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(glue)
options(dplyr.summarise.inform = FALSE)

# Note :
#   The purpose of this script is :
#   Create some functions :
#     1. Function that define sublevel (=bloc) using the same "mecanism" as in this hattrick post : [post=17308544.465]
#        But having this as parameter :
#           - Number of minutes by bloc
#           - Number of bloc(s) lost at the beginning of a match
#           - Maximum number of bloc
#     2. Function that map bloc to level
#     3. Function to apply the model
#     4. Function to score a model
#   5. Tune the parameters
#   6. Find & save the best models

# Load data
bck_x <- readRDS("03-focused_data/fu_fe_focused.RDS")

#####################################
## 1. Function to create the blocs ##
#####################################
{
  create_blocs <- function(x, nb_of_blocs, bloc_duration, bloc_lost) {
    # Create the number of bloc by during (using the integer part only)
    x$bloc_duration <- x$duration %/% bloc_duration
    
    # Create the bloc_to_add knowing that at the start of each game one bloc is lost for each formation
    x$bloc_to_add <- x$bloc_duration - bloc_lost
    
    # Apply the conditionnal cumsum
    starting_value <- 1
    
    # Here my "custom" sum function applying lower & upper limits
    custom_sum <- function(a,b, lower_limit = 1, upper_limit = nb_of_blocs) {
      x <- a+b
      x <- min(x, upper_limit)
      x <- max(x, lower_limit)
      return(x)
    }
    
    # Make the conditional cumsum using purrr::accumulate
    x <- x %>%
      arrange(team_id_hash, formation, formation_date) %>%
      group_by(team_id_hash, formation) %>%
      mutate(
        calculated_bloc = accumulate(bloc_to_add, custom_sum, .init = starting_value)[-1]
      ) %>%
      as.data.frame()
    return(x)
  }
  
  
}

#################################################
## 2. Create a function that map bloc to level ##
#################################################
{
  # For each bloc we find the "real level" that occurs the most.
  map_bloc_to_level <- function(bloc, level) {
    res <- data.frame(bloc = bloc, level = level) %>%
      group_by(bloc, level) %>%
      summarise(nb_details = n()) %>%
      mutate(score = nb_details/sum(nb_details)) %>%
      arrange(desc(score)) %>%
      as.data.frame() %>%
      group_by(bloc) %>%
      filter(score == max(score)) %>%
      arrange(bloc, desc(score)) %>%
      distinct(bloc, .keep_all = T) %>%
      select(bloc, calculated_level = level) %>%
      as.data.frame()
    if (length(unique(res$calculated_level)) != 8) warning("The mapping doesn't contains all the 8 level")
    return(res)
  }
}

##################################
## 3. Function to apply a model ##
##################################
{
  apply_bloc_model <- function(...) {
    
    # Create blocs
    x <- create_blocs(...)
    
    # Find the mapping between bloc & level
    bloc_mapping <- map_bloc_to_level(bloc = x$calculated_bloc, level = x$level)
    
    # Apply the mapping
    x <- x %>%
      left_join(bloc_mapping, by = c("calculated_bloc" = "bloc"))
    
    return(x)
    
  }
}

##################################
## 4. Function to score a model ##
##################################
{
  # I use this custom accuracy function because I want each level to weigh the same
  # However in the dataset this is not even :
  #  3 => 30%
  #  4, 5, 6 < 10%
  #  9 => 20%
  # The score is calculated as follow :
  #   1. For each "real" level compute the mean absolute difference between "real" and "predicted"
  #   2. Compute the mean of the 8 level
  # The model must be close to 0 to be good. This score is good enough to compare model between them.
  model_accuracy <- function(calculated_level, level) {
    data.frame(x=calculated_level, y=level) %>%
      mutate(diff = abs(y-x)) %>%
      group_by(y) %>%
      summarise(diff = mean(diff)) %>%
      summarise(res = mean(diff)) %>%
      pull(res)
  }
}

############################
## 5. Tune the parameters ##
############################
{
  # Define the "possibles models"
  # Note : The "known model" described in the Hattrick post is the following :
  #   . nb_of_blocs = 32
  #   . bloc_duration = 10
  #   . bloc_lost = 1
  
  p1 <- data.frame(fake = 1, nb_of_blocs = seq(from = 24, to = 40, by = 2))
  p2 <- data.frame(fake = 1, bloc_duration = c(5:15))
  p3 <- data.frame(fake = 1, bloc_lost = c(1:5))
  
  grid_models <- p1 %>%
    full_join(p2, by = "fake") %>%
    full_join(p3, by = "fake") %>%
    select(-fake) %>%
    mutate(id = row_number())
  
  all_models_scores <- do.call("rbind", apply(grid_models, MARGIN = 1, function(x){
    res <- apply_bloc_model(x = bck_x, 
                            nb_of_blocs = x["nb_of_blocs"], 
                            bloc_duration = x["bloc_duration"],
                            bloc_lost = x["bloc_lost"])
    
    score <- model_accuracy(calculated_level = res$calculated_level, level = res$level)
    data.frame(
      nb_of_blocs = x["nb_of_blocs"], 
      bloc_duration = x["bloc_duration"],
      bloc_lost = x["bloc_lost"]
    ) %>% mutate(score = score)
  }))
  
  # Remove dummy rownames
  rownames(all_models_scores) <- NULL
}

#############################
## 6. Find the best models ##
#############################
{
  # Show best models
  all_models_scores %>% arrange(score) %>% head()
  
  #   nb_of_blocs bloc_duration bloc_lost     score
  #            30            10         1 0.3232739
  #            32            10         1 0.3238322
  #            32             9         1 0.3274024
  #            34             9         1 0.3284658
  #            34            10         1 0.3338304
  #            36             9         1 0.3358199
}

# Save the models results
saveRDS(all_models_scores, file = "04-models_by_bloc/models_by_bloc_scored.RDS")

# Save the functions
save(create_blocs, map_bloc_to_level, apply_bloc_model, model_accuracy, file = "99-helper_functions/model_helper_functions.RData")

