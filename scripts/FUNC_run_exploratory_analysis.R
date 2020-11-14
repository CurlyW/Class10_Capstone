# Title     : FUNC_run_exploratory_analysis.R
# Objective : To execute a standard set of evaluations to test for assumptions
# Created by: grant
# Created on: 11/7/2020


run_EDA <- function(dataset){

  ##### Prep workspace

  # If tidyverse is installed and up to date, load, else install then load
  if('tidyverse' %in% unname(installed.packages()[,1]) &
     unname(installed.packages()[,3]['tidyverse'] >= "1.3.0")){
    library(tidyverse)
  } else {
    install.packages('tidyverse')
    library(tidyverse)
  }


  ##### Custom Functions

  not_empty <- function(vector_data){
    count <- sum(!is.na(vector_data))
    return(count)
  }

  empty <- function(vector_data){
    count <- sum(is.na(vector_data))
    return(count)
  }

  unique_count <- function(vector_data){
    count <- length(sort(unique(vector_data)))
    return(count)
  }

  mean_custom <- function(vector_data){
    if(class(vector_data) %in% c('integer', 'numeric')){
      count <- mean(vector_data, na.rm = TRUE)
    } else {
      count <- NA_integer_
    }
    return(count)
  }

  sd_custom <- function(vector_data){
    if(class(vector_data) %in% c('integer', 'numeric')){
      count <- sd(vector_data, na.rm = TRUE)
    } else {
      count <- NA_integer_
    }
    return(count)
  }

  min_custom <- function(vector_data){
    if(class(vector_data) %in% c('integer', 'numeric')){
      count <- min(vector_data, na.rm = TRUE)
    } else {
      count <- NA_integer_
    }
    return(count)
  }

  max_custom <- function(vector_data){
    if(class(vector_data) %in% c('integer', 'numeric')){
      count <- max(vector_data, na.rm = TRUE)
    } else {
      count <- NA_integer_
    }
    return(count)
  }

  med_custom <- function(vector_data){
    if(class(vector_data) %in% c('integer', 'numeric')){
      count <- median(vector_data, na.rm = TRUE)
    } else {
      count <- NA_integer_
    }
    return(count)
  }

  q25 <- function(vector_data){
    if(class(vector_data) %in% c('integer', 'numeric')){
      count <- quantile(vector_data, probs = 0.25, na.rm = TRUE)
    } else {
      count <- NA_integer_
    }
    return(count)
  }

  q75 <- function(vector_data){
    if(class(vector_data) %in% c('integer', 'numeric')){
      count <- quantile(vector_data, probs = 0.75, na.rm = TRUE)
    } else {
      count <- NA_integer_
    }
    return(count)
  }

  labels_flag_custom <- function(vector_data){
    if(class(vector_data) %in% 'character'){
      if(length(vector_data[!is.na(vector_data)]) > 3 *
         length(sort(unique(vector_data)))){
        count <- 'Y'
      } else {
        count <- 'N'
      }
    } else {
      count <- 'N'
    }
    return(count)
  }

  labels_custom <- function(vector_data){
    if(class(vector_data) %in%'character'){
      if(length(vector_data[!is.na(vector_data)]) > 3 *
         length(sort(unique(vector_data)))){
        count <- paste0(sort(unique(vector_data)), collapse = ", ")
      } else {
        count <- NA_character_
      }
    } else {
      count <- NA_character_
    }
    return(count)
  }


  # Prep initial table
  df1 <-
    as_tibble(data.frame("Columns" = names(dataset),
                         "DataTypes" = sapply(dataset, class),
                         "Rows" = nrow(dataset)[1],
                         "Unique_Rows" = nrow(distinct(dataset))[1],
                         "Duplicate_Rows" = nrow(dataset)[1] -
                           nrow(distinct(dataset))[1],
                         "Duplicate_Percent" = paste0(round((nrow(dataset)[1] -
                                                               nrow(distinct(dataset))[1])
                                                            / nrow(dataset)[1],2) *100,"%"),
                         "Values_Present" = sapply(dataset, not_empty),
                         "Values_Missing" = sapply(dataset, empty),
                         "Missing_Percent" = paste0(round(sapply(dataset, empty)
                                                          / nrow(dataset),2) *100,"%"),
                         "Values_Unique" = sapply(dataset, unique_count),
                         "Unique_Percent" = paste0(round(sapply(dataset, unique_count)
                                                         / nrow(dataset),2) *100,"%"),
                         stringsAsFactors = FALSE))

  # Prep primary key identifier table
  df2 <-
    df1 %>%
    select(Columns, Missing_Percent, Unique_Percent) %>%
    mutate(Possible_Key = ifelse(Missing_Percent == "0%" & Unique_Percent == "100%",
                                 "Y", "N")) %>%
    select(-Missing_Percent, -Unique_Percent)

  # Prep numeric analysis table
  df3 <-
    df1 %>%
    select(Columns, DataTypes) %>%
    mutate(Mean = sapply(dataset, mean_custom),
           StDev = sapply(dataset, sd_custom),
           Min = sapply(dataset, min_custom),
           Quart25 = sapply(dataset, q25),
           Median = sapply(dataset, med_custom),
           Quart75 = sapply(dataset, q75),
           Max = sapply(dataset, max_custom)) %>%
    select(-DataTypes)

  # Prep text analysis table
  df4 <-
    df1 %>%
    select(Columns, DataTypes) %>%
    mutate(Possible_Label = sapply(dataset, labels_flag_custom),
           Labels = sapply(dataset, labels_custom)) %>%
    select(-DataTypes)

  # Combine all tables
  df5 <-
    df1 %>%
    left_join(df2, by = "Columns") %>%
    left_join(df3, by = "Columns") %>%
    left_join(df4, by = "Columns")

  return(df5)

}
