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


tapply(CIMouse$max_OOC, CIMouse$infection_type, median)
### statistics

### 
wilcox.test(CIMouse[CIMouse$infection_type%in%"homologous_E64",]$max_OOC,
            CIMouse[CIMouse$infection_type%in%"heterologous_E64",]$max_OOC)

wilcox.test(CIMouse[CIMouse$infection_type%in%"homologous_E64",]$max_OOC,
            CIMouse[CIMouse$infection_type%in%"primary_E64",]$max_OOC)

wilcox.test(CIMouse[CIMouse$infection_type%in%"heterologous_E64",]$max_OOC,
            CIMouse[CIMouse$infection_type%in%"primary_E64",]$max_OOC)


## low sample size
wilcox.test(CIMouse[CIMouse$infection_type%in%"homologous_E88",]$max_OOC,
            CIMouse[CIMouse$infection_type%in%"heterologous_E88",]$max_OOC)


CIMouse$max_OOC <- as.integer(CIMouse$max_OOC)
CIMouse$infection_type <- as.factor(CIMouse$infection_type)

library(MASS)
OOCsimple.glm <- glm.nb(max_OOC ~ infection_type, data=CIMouse)

summary(OOCsimple.glm)

library(multcomp)
summary(glht(OOCsimple.glm, mcp(infection_type="Tukey")))


CIMouse$infHetHom <- as.factor(unlist(lapply(strsplit(as.character(CIMouse$infection_type), split="_"), "[[", 1)))

CIMouse$infCurrent <- as.factor(gsub(".*_", "",
                                     as.character(CIMouse$infection_type)))

OOCsimple.glm <- glm.nb(max_OOC ~ infection_type + experiment, data=CIMouse,
                        subset=!infection_type%in%"UNI")

summary(OOCsimple.glm)

OOCHetHom.glm <- glm.nb(max_OOC ~ infHetHom+infCurrent+experiment,
                        data=CIMouse, subset=!infection_type%in%"UNI")


## sepearate effects for each mouse (reused in primary and challenge)
library(lme4)
OOCHetHom.glmm <- glmer.nb(max_OOC ~ infHetHom+infCurrent + experiment +
                               (1|EH_ID),
                           data=CIMouse, subset=!infection_type%in%"UNI")

## doesn't work we should take the warning seriously and try to remove
## it. (Maybe it's because some mice have no data for both primary and
## secondary?). 
summary(OOCHetHom.glmm)

OOCHetHomInt.glm <- glm.nb(max_OOC ~ infHetHom*infCurrent+experiment,
                           data=CIMouse, subset=!infection_type%in%"UNI")

summary(OOCHetHomInt.glm)


library(multcomp)
summary(glht(OOCsimple.glm, mcp(infection_type="Tukey")))

summary(glht(OOCHetHom.glm, mcp(infHetHom="Tukey")))

summary(glht(OOCHetHom.glm, mcp(infCurrent="Tukey")))

summary(glht(OOCHetHomInt.glm, mcp(infHetHom="Tukey")))


