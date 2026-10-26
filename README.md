# Men do not suppress emotions more than women in everyday life: Evidence from large scale experience-sampling data


This repository contains R-code, data and results related to the study: "Men do not suppress emotions more than women in everyday life: Evidence from large scale experience-sampling data". 

The preregistration form can be found on OSF via this link: <add link>


## Raw Data

The raw data from each study included in our analysis is part of the EMOTE data base (Kalokerinos, et al., 2026). Although the raw .csv files for each individual study are not included on this github repository, they can be obtained by sending a request to the maintainers of the EMOTE database. The following link can be used to request data: <https://emotedatabase.com/requestid/LX5RQSRPFX>

To reproduce our results, store the raw data in the `Data-raw` folder within this project, and run the code step by step.



## R-code

In the folder `R` the following folders can be found: 

- `01_preprocess-data`

- `02_descriptives`

- `03_RQ1`


Each folder contains the code and some results for a specific step in the data analysis.


### 01_proprocess-data


The folder `01_preprocess-data` contains several R-scripts that were used to preprocess the data. Because each included study used different variable names, coding schemes, ... separate preprocessing files were created for each study. The study specific preprocessing R script start with `99_`. For each study, the scripts results in saving three data files (with .RDS extension): 

- a file with the beep-level-data, saved in the folder `Data/beep-level-data/`

- a file with the person-level-data, saved in the folder `Data/person-level-data/` beep-level measures are aggregated per person, by computed the average value across all beeps with a response.

- a file with descriptes and computed scale statistics (Crohnbach's alpha, ...),  saved in the folder `Data/descriptives/`


In addition to `99_` the two main R scripts are: 

- `1_preprocess.R`

- `2_combine-data.R`


#### `1_preprocess.R`

The R script `1_preprocess.R` can be seen as a convenience wrapper that executes the study specific preprocessing R scripts (`99_`) sequentially.


#### `2_combine-data.R`

The R script `2_combine-data.R` combines and then saves as one file:

- the beep-level data from all studies

- the person-level data from all studies

- descriptive information from all studies


The resulting files are saved in `Data/beep-level-data/`, `Data/person-level-data/`, and `Data/descriptives/`, respectively.


### 02_descriptives

The 02_descriptives folder contains a quarto file (`descriptives.qmd`) in which tables with descriptive information about the studies are created. The information from the tables is used in Table 1 in the manuscript. In addition, the folder contains the rendered .pdf result: `descriptives.pdf`. 




## References

Kalokerinos, E. K., Russo-Batterham, D., Grewal, K. K., Moeck, E. K., Wilson, E., Smith, I., Greenaway, K. H., Garrett, P., Shrestha, K. M., Kuppens, P., & Koval, P. (2026). The EMOTE Database: An open, searchable database of experience sampling data mapping everyday emotion processes. PsyArXiv. <https://osf.io/preprints/psyarxiv/h7kxd_v1/> 
