library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(glue)
library(ggplot2)
library(scales)
options(dplyr.summarise.inform = FALSE)

# Note :
#   The purpose of this script is :
#   1. Compare the best bloc models and nls models
#   2. Visualise the results
#   3. Give an output table

# Load data
bck_x <- readRDS("03-focused_data/fu_fe_focused.RDS")

# Load helpers function
load("99-helper_functions/model_helper_functions.RData")
load("99-helper_functions/model_nls_helper_functions.RData")

# Load the models scores
scores <- readRDS("04-models_by_bloc/models_by_bloc_scored.RDS") %>%
  bind_rows(readRDS("05-non-linear_models/non-linear_models.RDS")) %>%
  mutate(type = case_when(
    is.na(a) ~ "bloc",
    T ~ "nls"
  )) %>%
  arrange(score) %>%
  mutate(rank = row_number())

##############################
## 1. Check the best models ##
##############################
{
  scores %>%
    arrange(score) %>%
    head()
  
  # nb_of_blocs bloc_duration bloc_lost     score  a  b rounding_fun type rank
  #          30            10         1 0.3232739 NA NA         <NA> bloc    1
  #          32            10         1 0.3238322 NA NA         <NA> bloc    2
  #          32             9         1 0.3274024 NA NA         <NA> bloc    3
  #          34             9         1 0.3284658 NA NA         <NA> bloc    4
  #          34            10         1 0.3338304 NA NA         <NA> bloc    5
  #          36             9         1 0.3358199 NA NA         <NA> bloc    6
  
  # We can see the best models are the ones with blocs
  
  # Check where is the best nls model
  scores %>%
    filter(type == "nls") %>% 
    head()
  
  # The first nls model is 121st
  
  table(scores$type)
  
  # This means that on the 495 bloc models tested, 120 are better than the best nls model.
}

#########################################
## 2. Visualization on the best models ##
#########################################
{
  #######################
  ## Helpers functions ##
  #######################
  {
    add_quality <- function(x) {
      x %>%
        mutate(quality = case_when(
          abs(level-calculated_level) == 0 ~ "Good",
          abs(level-calculated_level) == 1 ~ "Diff = 1",
          abs(level-calculated_level) > 1 ~ "Diff > 1"
        )) %>%
        mutate(quality = factor(quality, levels = c("Good", "Diff = 1", "Diff > 1")))
    }
    data_quality_pred <- function(x) {
      x %>%
        group_by(calculated_level, level) %>%
        summarise(volume = n()) %>%
        add_quality() %>%
        as.data.frame()
        
    }
    
    data_bloc_pred <- function(x) {
      x %>%
        group_by(calculated_level, level, calculated_bloc) %>%
        summarise(volume = n()) %>%
        add_quality() %>%
        as.data.frame()
    }
    
    data_theoric_nls <- function(x) {
      x %>%
        group_by(x, level, calculated_level) %>%
        summarise(volume = n()) %>%
        add_quality() %>%
        as.data.frame()
    }
    
    # Qualité des prédiction
    plot_quality_pred <- function(x) {
      ggplot(x, aes(x = calculated_level, y = level, size = volume, colour = quality)) +
        geom_point() +
        scale_colour_manual(name = "Prediction quality", values = c("#229954", "#f5b041", "#e74c3c"))
    }
    
    # Courbe des bloc vs level
    plot_bloc_pred <- function(x) {
      ggplot(x, aes(x = calculated_bloc, y = level)) +
        geom_point(aes(size = volume, colour = quality)) +
        geom_line(aes(x = calculated_bloc, y = calculated_level)) +
        scale_colour_manual(name = "Prediction quality", values = c("#229954", "#f5b041", "#e74c3c"))
    }
    
    # Courbe théorique du modèle nls
    plot_theoric <- function(x) {
      ggplot(x, aes(x = x, y = origin_calculated_level)) +
        geom_line() +
        geom_point(aes(x = x, y = level, colour = quality)) +
        scale_colour_manual(name = "Prediction quality", values = c("#229954", "#f5b041", "#e74c3c")) +
        labs(x = "Duration (minutes)", y = "Calculated level (points) - Calculated level with decimal (line)")
    }
  }
  
  #################################################
  ## A. Visualize results on the best bloc model ##
  #################################################
  {
    # Run model
    df <- apply_bloc_model(x = bck_x, nb_of_blocs = 30, bloc_duration = 10, bloc_lost = 1)
    
    # Prediction quality
    plot_quality_pred(x = data_quality_pred(x = df))
    
    # Bloc versus Level
    plot_bloc_pred(x = data_bloc_pred(x = df))
    
    # Table bloc vs level
    table(df$calculated_bloc, df$calculated_level)
    
  }
  
  ################################################
  ## B. Visualize results on the best nls model ##
  ################################################
  {
    # Run model
    df <- apply_nls_model(x = bck_x, nb_of_blocs = 320, bloc_lost = 9) %>%
      rename(origin_calculated_level = calculated_level) %>%
      mutate(calculated_level = floor(origin_calculated_level)) %>%
      mutate(calculated_bloc = round(origin_calculated_level, 1)) %>%
      add_quality()
      
    
    # Prediction quality
    plot_quality_pred(x = data_quality_pred(x = df))
    
    # Calculated level versus Level
    plot_bloc_pred(x = data_bloc_pred(x = df)) +
      labs(x = "Calculated level")
    
    #  Duration versus Calculated Level (points) and with Calculated level with decimals (line)
    plot_theoric(df)
    
    # Table duration versus calculated level
    df %>%
      arrange(x) %>%
      mutate(origin_calculated_level = round(origin_calculated_level, 2)) %>%
      distinct(x, origin_calculated_level) %>%
      View()
  }
  
  
}