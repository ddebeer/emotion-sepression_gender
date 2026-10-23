library(dplyr)

# prepare data FEEL Study
data <- read.csv("Data-raw/data_RESS-EMA_study.csv")


# recode and select part 1
data <- data |>
  arrange(Date_Local, strptime(Time_Local, "%H:%M:%S")) |>
  mutate(
  age = AGE_BL,
  gender = case_when(
    GENDER_BL == 1 ~ "male",
    GENDER_BL == 2 ~ "female",
    .default = "other"),
  female = ifelse(gender == "female", 1, 0),
  neg_aff = EMOTION_INTENSITY_ES / 100,
  study = dataset
  ) |>
  rowwise() |>
  mutate(
    erq = (mean(c(ERQ_2_BL, ERQ_4_BL, ERQ_6_BL, ERQ_9_BL), na.rm = TRUE) - 1) / 6,
    er = mean(c(RESS_EMA_11_ES, RESS_EMA_12_ES), na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    er_b = (0.05 + er) / 100.1
  )



# reliabilities

## Negative Affect
library(multilevelTools)
omega_NA <- NULL



# person-level data
data_pp <- data |> group_by(UUID) |>
  summarise(age = mean(age, na.rm = TRUE),
            gender = head(gender, 1),
            female = head(female, 1),
            erq = mean(erq, na.rm = TRUE),
            ERQ_2_BL = head(ERQ_2_BL, 1),
            ERQ_4_BL = head(ERQ_4_BL, 1),
            ERQ_6_BL = head(ERQ_6_BL, 1),
            ERQ_9_BL = head(ERQ_9_BL, 1),
            er_pm = mean(er, na.rm = TRUE),
            neg_aff_pm = mean(neg_aff, na.rm = TRUE),
            study = head(study, 1)
            )

# reliabilities

## ERQ (RESS)
alpha <- psych::alpha(data |> select(ERQ_2_BL, ERQ_4_BL, ERQ_6_BL, ERQ_9_BL))
alpha_erq <- alpha$total


# other desciptives
descriptives0 <- data_pp |> summarize(
  n = n(),
  other = sum(gender == "other"),
  other_pct = other/n * 100,
  female = sum(gender == "female"),
  female_pct_no_other = female / (n - other),
  male = sum(gender == "male"),
  female_pct_no_other = female / (n - other),
  m_age = mean(age, na.rm = TRUE),
  sd_age = sd(age, na.rm = TRUE),
  min_age = min(age, na.rm = TRUE),
  max_age = max(age, na.rm = TRUE)

)

descriptives <- data_pp |>
  filter(gender != "other") |>  summarize(
    n = n(),
    female = sum(gender == "female"),
    female_pct_no_other = female / n,
    male = sum(gender == "male"),
    female_pct_no_other = female / n,
    m_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE)
  )



# transform after removing others
data_pp <- data_pp |> filter(gender != "other") |>
  mutate (age_z = scale(age)[,1],
          age_m = mean(age),
          female_c = scale(female, scale = FALSE)[,1],
          female_m = mean(female),
          ppID = paste0(study, row_number()),
          m_f = ifelse(gender == "male", .5, -.5),
          type = "cont")


# filter data
data <- data |>
  select(age, gender, erq,
         er, er_b,
         neg_aff, UUID,
         Date_Local, Time_Local, study)|>
  filter(gender != "other", !is.na(er))




data <- left_join(data, data_pp) |>
  mutate(
    neg_aff_pmc = neg_aff - neg_aff_pm
  ) |> group_by(ppID, Date_Local) |>
  mutate(
    neg_aff_pmc_lag = lag(neg_aff_pmc),
    time = strptime(Time_Local, "%H:%M:%S"),
    time_diff = as.numeric(time - lag(time), units = "secs")
  )





saveRDS(data_pp, "Data/person-level-data/ress.RDS")
saveRDS(data, "Data/beep-level-data/ress.RDS")
saveRDS(list(alpha_erq = alpha_erq,
             which_erq = "ERQ",
             omega_NA = omega_NA$Results,
             descriptives0 = descriptives0,
             descriptives = descriptives), "Data/descriptives/ress.RDS")
rm(data, data_pp, descriptives, alpha, alpha_erq, omega_NA)

