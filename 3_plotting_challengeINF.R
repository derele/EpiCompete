## packages always on top
library(ggplot2)

## then the data: test whether the objects needed here generated in
## the first data preparation scripts exist, source the script if one
## is missing
if(!exists("CIMouse")){
    source("1_challenge_infections.R")
} else{
    message ("using data alredy in the interactive session")
}


CIMouse%>% 
    ggplot(aes(infection_history,
               max_OOC, color=infection)) +
    geom_boxplot() +
    geom_jitter(width=0.2, alpha=0.8) +
    facet_wrap(~experiment) +
    theme_bw()


CIMouse%>% 
    ggplot(aes(infection_history,
               max_WL, color=infection)) +
    geom_boxplot() +
    geom_jitter(width=0.2, alpha=0.8) +
    facet_wrap(~experiment) +
    theme_bw()

