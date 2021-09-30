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


pdf("figures/OOcystOverview1.pdf", width=8, height=5)
CIMouse%>% 
    ggplot(aes(infection_history,
               max_OOC, color=infection)) +
    geom_boxplot() +
    geom_jitter(width=0.2, alpha=0.8) +
    ##    facet_wrap(~experiment) +
    scale_y_log10()+
    theme_bw()
dev.off()


pdf("figures/OOcystOverview2.pdf", width=8, height=5)
CIMouse%>% 
    ggplot(aes(infection_type, max_OOC)) +
    geom_boxplot() +
    geom_jitter(width=0.2, alpha=0.8) +
    ##    facet_wrap(~experiment) +
    scale_y_log10()+
    coord_flip()+
    theme_bw()
dev.off()




pdf("figures/WLossOverview1.pdf", width=8, height=5)
CIMouse%>% 
    ggplot(aes(infection_history,
               max_WL, color=infection)) +
    geom_boxplot() +
    geom_jitter(width=0.2, alpha=0.8) +
    ## facet_wrap(~experiment) +
    theme_bw()
dev.off()


pdf("figures/WLossOverview2.pdf", width=8, height=5)
CIMouse%>% 
    ggplot(aes(infection_type, max_WL)) +
    geom_boxplot() +
    geom_jitter(width=0.2, alpha=0.8) +
    ##    facet_wrap(~experiment) +
    scale_y_log10()+
    coord_flip()+
    theme_bw()
dev.off()
