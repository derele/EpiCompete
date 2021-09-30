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

#plotting weight loss and oocysts 
ggplot(CIMouse, aes(x = max_OOC, y = max_WL, color = infection_type, shape = infection)) +
    geom_jitter() +
    scale_x_log10() +
    scale_y_log10()  +
    facet_wrap(~ infection_history)

#plotting weight loss for each infection_history
ggplot(CIMouse, aes(x = infection_history, y = max_WL, color = infection, size = max_OOC)) +
    geom_point() +
    scale_y_log10() 

ggplot(CIMouse, aes(x = infection_history, y = max_WL, color = infection)) +
    geom_violin() +
    scale_y_log10() 

#group by primary infections
CIMouse %>% 
    filter(infection == "primary") -> only_primary
CIMouse %>% 
    filter(infection == "challenge") -> only_challenge

# plot differences in primary infections 
#one can see here increased weight loss in E64 and E88 compared to UNI, correlating to max_OOC
plot1 <- ggplot(only_primary, aes(x = primary_infection, y = max_WL, colour = primary_infection, size = max_OOC)) +
  geom_jitter() +
    scale_y_log10() 


#now I want to plot the differences in secondary infections
plot2 <- ggplot(only_challenge, aes(x = challenge_infection, y = max_WL, colour = challenge_infection, size = max_OOC)) +
    geom_jitter() +
    facet_wrap(~ infection_type) 
 
#trying to plot for primary and challenge together, first try
ggplot(CIMouse, aes(x = challenge_infection, y = max_WL, size = max_OOC, color = infection)) +
    geom_jitter() +
    facet_wrap(~ infection_history)

#second try, seeing that it is not worth it 
install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot1, plot2)


CIMouse %>% 
    filter(infection == "challenge") -> only_challenge