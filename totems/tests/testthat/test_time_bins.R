library(tidyverse)
library(lubridate)

context("Cutting trials into time bins")

test_that("time column can be cut into time bins", {
  # this is a f----ing joke
  times <- c(0.1, 4, 5, 9.9, 10, 10.1)
  cut_1 <- c(  0, 0, 1,   1,  2,    2)
  cut_2 <- c(  0, 0, 0,   0,  1,    1)
  cut_3 <- c(  0, 4, 5,   9, 10,   10)
  expect_identical(cut_times(times), cut_1)
  expect_identical(cut_times(times, bin_duration = 10), cut_2)
  expect_identical(cut_times(times, bin_duration = 1), cut_3)
})

test_that("cut times accepts lubridate durations", {
  expect_equal(cut_times(dseconds(c(0.1, 5))), c(0, 1))
})

context("Get closest trial to a particular time")

test_that("correct trial is selected", {
  trials <- data_frame(TrialID = c(0, 1), TeamTime = c(0.5, 1.1))
  trial <- get_closest_trial_to_time(1, trials)
  expect_equal(trial$TrialID, 0)
})

test_that("missing trial returns without blowing up", {
  trials <- data_frame(TrialID = c(0, 1), TeamTime = c(0.5, 1.1))
  trial <- get_closest_trial_to_time(0.4, trials)
  expect_equal(trial$TrialID, numeric(0))
})
