## packages always on top
## library(x)

## then the data:
## test whether the objects generated in the first data preparation
## scripts exist, source the script if one is missing
if(!exists("CIMouse") | !exists("CI")){
    source("1_challenge_infections.R")
}


