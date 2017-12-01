# ---- setup
library(tidyverse)
library(magrittr)
library(lme4)
library(AICcmodavg)
library(broom)
library(gridExtra)
library(crotchet)
library(totems)

t_ <- totems::load_totems_theme()

t_$scale_x_calendar_time_50 <- scale_x_continuous(
  "Calendar time (min)",
  breaks = seq(0, 3000, by = 60 * 5),
  labels = seq(0, 50, by = 5))

t_$scale_x_player_time_50 <- scale_x_continuous(
  "Learning time (min)",
  breaks = seq(0, 3000, by = 60 * 5),
  labels = seq(0, 50, by = 5))

t_$scale_x_team_time_50 <- scale_x_continuous(
  "Labor time (min)",
  breaks = seq(0, 3000, by = 60 * 5),
  labels = seq(0, 50, by = 5))