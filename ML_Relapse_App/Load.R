# Data Preparation
# Team Miami


# Packages to install if not already installed
packages <- c(
  "public.ctn0094data", "conflicted","ggthemes", "ggplot2", "table1", "styler", "glmnet", 
  "vip", "kknn", "janitor", "treemisc", "randomForest", "ranger", "usemodels",
  "gmodels", "doParallel", "tidyverse", "tidymodels"
)

# Check if packages are installed
installed_packages <- packages %in% 
  rownames(
    installed.packages()
  )

# Install packages if any are not installed
if (any(!installed_packages)) {
  packages_to_install <- packages[!installed_packages]
  remotes::install_github(packages_to_install)
}


# Loading the libraries
suppressPackageStartupMessages(
  {
    library(public.ctn0094data)
    library(ggthemes)
    library(ggplot2)
    library(table1)
    library(styler)
    library(glmnet)
    library(vip)
    library(kknn)
    library(janitor)
    library(treemisc)
    library(randomForest)
    library(ranger)
    library(usemodels)
    library(gmodels)
    library(doParallel)
    library(conflicted)
    library(tidyverse)
    library(tidymodels)
  }
)

# set preference
suppressMessages(conflict_prefer("filter", "dplyr"))
suppressMessages(conflict_prefer("spec", "yardstick"))
suppressMessages(conflicts_prefer(shinydashboard::box))
tidymodels_prefer()

# suppress "`summarise()` has grouped output by " messages
options(dplyr.summarise.inform = FALSE)


# Load the dataset

# All data objects have the same name as their sources OTHER THAN "randomized"

all_drugs <- public.ctn0094data::all_drugs
demographics <- public.ctn0094data::demographics
everybody <- public.ctn0094data::everybody
first_survey <- public.ctn0094data::first_survey
pain <- public.ctn0094data::pain
psychiatric <- public.ctn0094data::psychiatric
qol <- public.ctn0094data::qol
randomized <- public.ctn0094data::randomization # different name
rbs <- public.ctn0094data::rbs
rbs_iv <- public.ctn0094data::rbs_iv
tlfb <- public.ctn0094data::tlfb
uds_temp <- public.ctn0094data::uds_temp
withdrawal <- public.ctn0094data::withdrawal

# All Drugs data

rndr <- function(x, ...) {
  y <- render.default(x, ...)
  if (is.logical(x)) y[2] else y
}

drug_predictor <- all_drugs %>%
  dplyr::filter(what %in% c("Alcohol", "Methadone", "Buprenorphine", "Heroin", "Sedative-Hypnotic", "Antidepressant","PCP","Benzodiazepine")) %>%
  dplyr::filter(when > -29 & when < 0) %>%
  group_by(who, what) %>%
  summarise(events = n()) %>%
  pivot_wider(names_from = what, values_from = events, id_cols = who)

# Pain and Potential predictors drugs data set merged

# Pain status

pain_status_predictor <- pain %>%
  group_by(who) %>%
  select("pain")

drugs_and_pain <-
  inner_join(pain_status_predictor, drug_predictor, by = c("who" = "who"))


# Demographic data

# add trial -- project variable
analysis_demographics <-
  inner_join(
    demographics,
    everybody,
    by = "who"
  ) %>%
  mutate(
    Sex = factor(
      case_when(
        is_male == "No" ~ "Female",
        is_male == "Yes" ~ "Male",
        TRUE ~ "Refused/Missing"
      ),
      levels = c("Female", "Male", "Refused/Missing")
    )
  ) %>%
  mutate(
    Race = recode_factor(race, "Refused/missing" = "Refused/Missing")
  ) %>%
  mutate(
    Ethnicity =
      factor(
        case_when(
          is_hispanic == "No" ~ "Not Hispanic",
          is_hispanic == "Yes" ~ "Hispanic",
          TRUE ~ "Refused/Missing"
        )
      )
  ) %>%
  rename(Age = age) %>%
  rename(Education = education) %>%
  rename(Employment = job) %>%
  mutate(
    `Usual Living Arrangements` =
      factor(
        case_when(
          is_living_stable == "No" ~ "Not Stable",
          is_living_stable == "Yes" ~ "Stable",
          TRUE ~ "Refused/Missing"
        ),
        levels = c("Stable", "Not Stable", "Refused/Missing")
      )
  ) %>%
  rename(`Relationship Status` = marital) %>%
  select(
    -c(is_male, race, is_living_stable, is_hispanic)
  )

# Psych data

psych_analysis <- psychiatric %>%
  select(has_major_dep, has_anx_pan, has_bipolar, who) %>%
  rename(`Depression` = has_major_dep, `Anxiety/Panic Attacks` = has_anx_pan, `Bipolar Disorder` = has_bipolar)


#Homelessness Data

qol_analysis <- qol%>% 
  select(is_homeless, who) %>% 
  rename(`Homeless` = is_homeless)

# Withdrawl Symptoms Data
withdrawal_analysis <-withdrawal_pre_post%>% 
  select(who, withdrawal, when, what) %>%
  filter(what =="pre", as.numeric(when) <="45") %>%
  select(-c(what, when)) %>%
  rename(`Withdrawal Symptom Severity` = withdrawal)

# Risk Behavior data

rbs_data <- public.ctn0094data::rbs %>%
  group_by(who) %>%
  summarize(average_rbs = mean(days, na.rm = TRUE))

rbs_iv_data <- public.ctn0094data::rbs_iv

rbs_measures <- left_join(rbs_iv_data, rbs_data, by = c("who" = "who"))

# Outcome variable 

relapse <- read_csv("relapse_2023-06-08.csv")


# Merging variables

analysis_df <-
  analysis_demographics %>%
  select(-project) %>%
  full_join(psych_analysis, by = "who") %>%
  full_join(rbs_measures, by = "who") %>%
  full_join(drugs_and_pain, by = "who") %>%
  full_join(qol_analysis, by = "who") %>%
  full_join(withdrawal_analysis, by = "who") %>% 
  full_join(everybody, by ="who") 



# screened27 <- filter(everybody, project == 27)
# screened30 <- filter(everybody, project == 30)
# screened51 <- filter(everybody, project == 51)
# 
# who_randomized <-
#   inner_join(everybody, randomized, by = "who") %>%
#   filter(which == 1)
# 
# who_randomized_id <-
#   inner_join(everybody, randomized, by = "who") %>%
#   filter(which == 1) %>%
#   select(who)


# outcome variable


relapse_df <- read_csv("relapse_2023-06-08.csv")


# Merging with main data


analysis <- 
  analysis_df %>% 
  inner_join(relapse_df, by = "who") %>% 
  select(-c(cocaine_inject_days,heroin_inject_days,speedball_inject_days,opioid_inject_days, speed_inject_days)) %>% 
  rename(relapse =   "lee2018_rel_event") %>% 
  mutate(relapse = factor(relapse)) 
