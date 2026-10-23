# Preprocess the data from 11 studies from the EMOTE data base


# For each study, the preprocessing steps differ slightly depending on
# - which scales were used
# - which coding was used

# in addition descriptive statistics are computed for each study and stored in folder called "Data"


# this R-script executes the preprocessing steps for each study.


# install required packages
if(!require("dplyr")) install.packages("dplyr")
if(!require("multilevelTools")) install.packages("multilevelTools")
if(!require("psych")) install.packages("psych")



source("R/01_preprocess-data/99_preproces_aura.R")
source("R/01_preprocess-data/99_preproces_events.R")
source("R/01_preprocess-data/99_preproces_acu.R")
source("R/01_preprocess-data/99_preproces_everyday.R")
source("R/01_preprocess-data/99_preproces_effort.R")
source("R/01_preprocess-data/99_preproces_feel_1.R")
source("R/01_preprocess-data/99_preproces_goals.R")
source("R/01_preprocess-data/99_preproces_leuven_3.R")
source("R/01_preprocess-data/99_preproces_leuven_2011.R")
source("R/01_preprocess-data/99_preproces_leuven_2012.R")
source("R/01_preprocess-data/99_preproces_ress.R")


