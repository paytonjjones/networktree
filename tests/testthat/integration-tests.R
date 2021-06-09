# for running locally:
# require(testthat)
# require(devtools)
# load_all()

context("data_setup")

set.seed(1)
d <- data.frame(trend = 1:200, foo = runif(200, -1, 1))
d <- cbind(d, rbind(
  mvtnorm::rmvnorm(100, mean = c(0, 0, 0),
                   sigma = matrix(c(1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 1), ncol = 3)),
  mvtnorm::rmvnorm(100, mean = c(0, 0, 0),
                   sigma = matrix(c(1, 0, 0.5, 0, 1, 0.5, 0.5, 0.5, 1), ncol = 3))
))
colnames(d)[3:5] <- paste0("y", 1:3)

context("mob")

test_that("default interface runs without errors", {
  expect_error(
    tree1 <- networktree(nodevars=d[,3:5], splitvars=d[,1:2]),
    NA
  )
})

test_that("dummy test that fails", {
  expect_error(
    foo(),
    NA
  )
})