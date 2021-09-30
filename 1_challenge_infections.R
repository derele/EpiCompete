#starting to work on it 
library(tidyverse)
library(dplyr)

## read the data from our lab data repository
CI <- read.csv("https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data_products/Challenge_infections.csv")  

## THIS IS NOT TO BE SAVED, just to play around and explore/unerstand
## the data
CI %>%
    filter(!is.na(challenge_infection)) %>%
    group_by(experiment) %>%
    summarize(mice=n_distinct(EH_ID),
              n_obs=n(),
              n_mice_challenge=n_distinct(EH_ID[infection%in%"challenge"]))
## this looks good: 742 dpi-samples from 64 mice in E57 are not
## relevant as they were only infected ones (once?) (by Alice


## now change the dataset
CI %>% filter(!is.na(challenge_infection)) %>%
    rowwise() %>% mutate(OO4sq = rowSums(across(starts_with("oocyst_")))) %>%
    ## 0.1µl per square -> *10.000 to scale up to ml
    mutate(OOC=(OO4sq/4*10000)/dilution) %>%
    ## we have ZEROS in feces weight (also when we counted oocysts) so we better don't
    ## calculate OPG for now but just max (see below)
    ## mutate(OPG=OOC/feces_weight) %>%
    ## also re-calculate relative weight, as this seems to have errors
    ## from a spreadsheet program (wtf!)
    mutate(relative_weight= weight/weight_dpi0*100) %>%
    ## also look at this for OPG above (by uncommenting)
    ## select(feces_weight, starts_with("oocyst_"), OO4sq, OOC) %>%
    ## look at this for controlling the weight calculation
    ## select(EH_ID, dpi, infection, weight, relative_weight) %>%
    ## print(n=40)
    ## the E88 innoculum in E57 challenge infection was "not
    ## working", these mice are basically unifected controls
    mutate(challenge_infection=ifelse(!experiment%in%"E57",
                                      challenge_infection,
                               ifelse(challenge_infection%in%"E88", "UNI",
                                      challenge_infection))) %>%
    ## then correct the infection history
    mutate(infection_history=paste0(primary_infection, "_",
                                    challenge_infection)) ->
    CI

## HAVE a look at missing data. This is either missing because of the
## feces_weight zero problem (for E57) or because mice died
## (especially in primary infection in E10 and E11)
CI %>% group_by(EH_ID, infection, experiment) %>%
    summarize(n_distinct_labels=n_distinct(labels),
              n_notNA=n_distinct(labels[!is.na(OOC)&!is.na(relative_weight)])) %>%
    print(n=220)

### feces weight is only missing in one case when we have oocysts
### (other mice are dead)
table(CI[CI$feces_weight==0, "oocyst_sq1"])


## For an analysis of immune protection we want the following
## categories in one column
CI$infection_type <- NA
## primary_E88
CI$infection_type[CI$infection_history%in%"UNI_E88" &
                  CI$infection%in%"challenge"] <-  "primary_E88"
CI$infection_type[CI$primary_infection%in%"E88" &
                  CI$infection%in%"primary"] <-  "primary_E88"
## homologous_E88
CI$infection_type[CI$infection_history%in%"E88_E88" &
                  CI$infection%in%"challenge"] <-  "homologous_E88"
## heterologous_E88 
CI$infection_type[CI$infection_history%in%"E64_E88" &
                  CI$infection%in%"challenge"] <-  "heterologous_E88"
## primary_E64  ("UNI_E64" and challenge in infection) E64_* and primary in infection
CI$infection_type[CI$infection_history%in%"UNI_E64" &
                  CI$infection%in%"challenge"] <-  "primary_E64"
CI$infection_type[CI$primary_infection%in%"E64" &
                  CI$infection%in%"primary"] <-  "primary_E64"
## homologous_E64
CI$infection_type[CI$infection_history%in%"E64_E64" &
                  CI$infection%in%"challenge"] <-  "homologous_E64"
## heterologous_E64
CI$infection_type[CI$infection_history%in%"E88_E64" &
                  CI$infection%in%"challenge"] <-  "heterologous_E64"
## the remaining should be UNI?!
CI$infection_type[is.na(CI$infection_type)] <- "UNI"


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

