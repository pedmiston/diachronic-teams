library(dplyr)
library(evoteams)

context("Kaggle data")

test_that("submission scores are binned correctly", {
  submissions <- data_frame(CompetitionId = 1,
                            Score = c(0, 11))
  leaderboards <- data_frame(CompetitionId = 1,
                             Place = 1:10,
                             Score = 1:10)

  expected_places <- c(11, 1)
  result <- predict_place(submissions, leaderboards)
  expect_equal(result$PredictedPlace, expected_places)
})

test_that("submission scores bin by competition", {
  submissions <- data_frame(CompetitionId = c(1, 1, 2, 2),
                            Score = c(0, 11, 11, 21))
  leaderboards <- data_frame(CompetitionId = rep(c(1, 2), each = 10),
                             Place = c(1:10, 1:10),
                             Score = c(1:10, 11:20))

  expected_places <- c(11, 1, 11, 1)
  result <- predict_place(submissions, leaderboards)
  expect_equal(result$PredictedPlace, expected_places)
})
