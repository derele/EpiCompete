## packages always on top
library(corrr)

## then the data: test whether the objects we need here exist, source
## the script(s) if one is missing
if(!exists("CIMouse") | !exists("CIMouseW")){
    source("1_challenge_infections.R")
} else{
    message ("using data alredy in the interactive session")
}


CIMouseW %>%
filter(!challenge_infection%in%"UNI" &
                    !primary_infection%in%"UNI") %>%
    select(max_OOC_challenge, max_OOC_primary,
           max_WL_challenge, max_WL_primary) %>%
    correlate()


