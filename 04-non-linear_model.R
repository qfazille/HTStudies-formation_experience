library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(glue)
options(dplyr.summarise.inform = FALSE)

# Note :
#   The purpose of this script is :
#   1. Function that apply a non-linear model
#   2. Tune a non-linear model knowing the parameters :
#      - Number of minutes by bloc = 1 (fixed !)
#      - Number of minutes lost at the beginning of a match
#      - Maximum number of minute a team can capitalize
#   6. Find & save the best models
#   7. Compare with the 

# Load data
bck_x <- readRDS("03-focused_data/fu_fe_focused.RDS")

# Load helper functions
load(file = "99-helper_functions/model_helper_functions.RData")

###########################################
## 1. Redfine the 'apply_model' function ##
###########################################
{
  apply_nls_model <- function(...) {
    # Create 'blocs' => this function is just used to cumsum the duration as the bloc_duration = 1
    #df <- create_blocs(x = bck_x, nb_of_blocs = 360, bloc_duration = 1, bloc_lost = 10) %>%
    df <- create_blocs(bloc_duration = 1, ...) %>%
      select(x = calculated_bloc, y = level)
    x <- df$x
    y <- df$y
    mod <- nls(y ~ a*x/(b+x), start = list(a = 10, b = 70), control = nls.control(maxiter = 500))
    
    df$a <- coef(mod)["a"]
    df$b <- coef(mod)["b"]
    df$calculated_level <- pmin(10,pmax(3, predict(mod, df))) # minium 3 and maximum 10
    df <- df %>% rename(level = y)
    return(df)
  }

}

############################
## 2. Tune the parameters ##
############################
{
  # Define the "possibles models"
  # Note : The "known model" described in the Hattrick post is the following :
  #   . nb_of_blocs = 320 to 380 (maximum number of minutes)
  #   . bloc_lost = 5:20 (minutes lost)
  p1 <- data.frame(fake = 1, bloc_lost = 5:15)
  p2 <- data.frame(fake = 1, nb_of_blocs = 320:370)
  
  grid_models <- p1 %>%
    full_join(p2, by = "fake") %>%
    select(-fake) %>%
    mutate(id = row_number())

  all_models_scores <- do.call("rbind", apply(grid_models, MARGIN = 1, function(x){
    res <- apply_nls_model(x = bck_x, 
                           nb_of_blocs = x["nb_of_blocs"], 
                           bloc_lost = x["bloc_lost"])
    
    # For one model there are 3 possibilty to use it as the predicted value contains decimals
    #  1. Round the predicted value (closest integer)
    #  2. Floor the predicted (largest integer not greater than x)
    #  3. Ceiling the predicted (smallest  integer not less than x)
    score1 <- model_accuracy(calculated_level = round(res$calculated_level), level = res$level)
    score2 <- model_accuracy(calculated_level = floor(res$calculated_level), level = res$level)
    score3 <- model_accuracy(calculated_level = ceiling(res$calculated_level), level = res$level)
    a <- unique(res$a)
    b <- unique(res$b)
    
    data.frame(
      nb_of_blocs = rep(x["nb_of_blocs"], 3),
      bloc_duration = rep(1, 3),
      bloc_lost = rep(x["bloc_lost"], 3),
      a = rep(a, 3),
      b = rep(b, 3)
    ) %>% mutate(score = c(score1, score2, score3)) %>%
      mutate(rounding_fun = c("round", "floor", "ceiling"))
  }))
}

#############################
## 6. Find the best models ##
#############################
{
  # Show best models
  all_models_scores %>% arrange(score) %>% head()
}

# Save the models results
saveRDS(all_models_scores, file = "05-non-linear_models/non-linear_models.RDS")

# Save the function
save(apply_nls_model, file = "99-helper_functions/model_nls_helper_functions.RData")
