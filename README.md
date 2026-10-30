# Men do not suppress emotions more than women in everyday life: Evidence from large scale experience-sampling data


This repository contains R-code, data and results related to the study: "Men do not suppress emotions more than women in everyday life: Evidence from large scale experience-sampling data". 

The preregistration form can be found on OSF via this link: https://osf.io/84uqe


## Raw Data

The raw data from each study included in our analysis is part of the EMOTE data base (Kalokerinos, et al., 2026). Although the raw .csv files for each individual study are not included on this github repository, they can be obtained by sending a request to the maintainers of the EMOTE database. The following link can be used to request data: <https://emotedatabase.com/requestid/LX5RQSRPFX>

To reproduce our results, store the raw data in the `Data-raw` folder within this project, and run the code step by step.



## R-code

In the folder `R` the following folders can be found: 

- `01_preprocess-data`

- `02_descriptives`

- `03_RQ1a`

- `04_RQ1b`


Each folder contains the code and some results for a specific step in the data analysis.


### 01_proprocess-data


The folder `01_preprocess-data` contains several R-scripts that were used to preprocess the data. Because each included study used different variable names, coding schemes, ... separate preprocessing files were created for each study. The study specific preprocessing R script start with `99_`. For each study, the scripts results in saving three data files (with .RDS extension): 

- a file with the beep-level-data, saved in the folder `Data/beep-level-data/`

- a file with the person-level-data, saved in the folder `Data/person-level-data/` beep-level measures are aggregated per person, by computed the average value across all beeps with a response.

- a file with descriptes and computed scale statistics (Crohnbach's alpha, ...),  saved in the folder `Data/descriptives/`


In addition to `99_` the main R scripts are: 

- `1_preprocess.R`

- `2_combine-person-level-data.R`


#### `1_preprocess.R`

The R script `1_preprocess.R` can be seen as a convenience wrapper that executes the study specific preprocessing R scripts (`99_`) sequentially.


#### `2_combine-person-level-data.R`

The R script `2_combine-person-level-data.R` combines all the person level data from the included EMOTE studies. Participants reporting a gender different from `male` or `female` are excluded form the data.

After combining the data from the different studies, the for the covariates `age`, `female` (a dummy coded indicator for gender), `negative_affect` (i.e., for each person the average of his or her momentary negative affect responses) are decomposed into a level 1 (person level) and and a level 2 (study level) component. More specifically, person deviations from the study average are computed, resulting in `age_pd`, `female_pd`, and `neg_aff_pd`, where the `_pd` suffix stands for "person deviation". In addition, for each covariate, the study deviation from the overall mean is computed, resulting in `age_sd`, `female_sd`, and `neg_aff_sd`, where the suffix `_sd` stands for "study deviation".

The resulting person level data is stored as `Data/person-level-data.RDS1`.


### 02_descriptives

The 02_descriptives folder contains a and R-script (`01_combine-descriptives.R`) in which the descriptive statitistics of each study are combined. In addition, the quarto file (`02_descriptives.qmd`) creates tables with descriptive information about the studies. The information from these tables is used in Table 1 in the manuscript. Finally, the folder contains the rendered .pdf result: `02_descriptives.pdf`. 


### 03_RQ1a

This folder contains the code and the results for the analyses related to research question RQ1a. The results are found in [this .pdf document](https://github.com/ddebeer/emotion-sepression_gender/blob/main/R/03_RQ1a/4_reported-results_RQ1a.pdf)

### 04_RQ1b

This folder contains the code and the results for the analyses related to research question RQ1b. The results are found in [this .pdf document](https://github.com/ddebeer/emotion-sepression_gender/blob/main/R/04_RQ1b/4_reported-results_RQ1b.pdf)

## References

Kalokerinos, E. K., Russo-Batterham, D., Grewal, K. K., Moeck, E. K., Wilson, E., Smith, I., Greenaway, K. H., Garrett, P., Shrestha, K. M., Kuppens, P., & Koval, P. (2026). The EMOTE Database: An open, searchable database of experience sampling data mapping everyday emotion processes. PsyArXiv. <https://osf.io/preprints/psyarxiv/h7kxd_v1/> 
