# ---- setup
library(magrittr)
library(broom)
library(lme4)
library(AICcmodavg)
library(gridExtra)
library(crotchet)
library(totems)
library(tidyverse) # Load tidyverse after totems to prevent dplyr::filter from being masked
t_ <- load_totems_theme()
