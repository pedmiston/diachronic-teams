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
