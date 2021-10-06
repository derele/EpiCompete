#starting to work on it 
library(tidyverse)
library(dplyr)

## read the data from our lab data repository
CI <- read.csv("https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data_products/Challenge_infections.csv")  

## summarize by mouse and infection (challenge/primary)
as_tibble(CI) %>%
    group_by(EH_ID, infection) %>%
    summarize(max_OOC = max(OOC, na.rm=TRUE),
              max_WL = min(relative_weight, na.rm=TRUE),
              ## do this more elegantly!
              experiment = unique(experiment),
              mouse_strain= unique(mouse_strain),
              primary_infection=unique(primary_infection),
              challenge_infection=unique(challenge_infection),
              infection_history=unique(infection_history),
              infection_type=unique(infection_type)) ->
    CIMouse



### This wider format might be more usable and more intuitive:
CIMouse %>%  
    pivot_wider(names_from = infection,
                values_from = c(max_OOC, max_WL)) ->
    CIMouseW


## object genereated in this script are: 

## CI - challenge infection full dataset for all dpi

## CIMouse - challenge infection dataset with per mouse and infection
## type (challenge/primary) max oocyst and max weight loss

## CIMouseW - same as CIMouse but wide format with different columns
## for the infection types (for each weight loss and oocyst shedding)

