library(evoteams)

context("Load graphviz figures")

test_that("find graphviz returns full path", {
  path <- find_graphviz("team-structure-single-project")
  expect_true(file.exists(path))
})

test_that("find graphviz fails if the file wasn't found", {
  expect_error(find_graphviz("not-a-real-file"))
})
