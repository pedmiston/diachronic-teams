# ---- setup
library(tidyverse)
library(lubridate)
library(magrittr)
library(grid)
library(gridExtra)
library(lme4)
library(broom)
library(crotchet)
library(totems)

# ---- data
library(totems)
data("TeamPerformance")
data("TeamProblems")
data("TeamTrials")
data("SampledTeamTrials")
data("PlayerTrials")
data("PlayerProblems")
data("SampledPlayerTrials")

TeamTrials %<>%
  recode_strategy() %>%
  recode_groups_by_generation()

SampledTeamTrials %<>%
  recode_strategy() %>%
  recode_groups_by_generation()

SampledPlayerTrials %<>%
  recode_strategy() %>%
  recode_groups_by_generation()

PlayerTrials %<>%
  recode_strategy()

TeamPerformance %<>%
  recode_strategy()

TeamProblems %<>%
  recode_strategy()

PlayerProblems %<>%
  recode_strategy()

totems_theme <- load_totems_theme()