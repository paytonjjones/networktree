library(qgraph)

## ---- Data Generation ----

data(workaholic)
data(tipi)
data(dass)

set.seed(1)
d <- data.frame(trend = 1:200, foo = runif(200, -1, 1))
d <- cbind(d, rbind(
  mvtnorm::rmvnorm(100, mean = c(0, 0, 0),
                   sigma = matrix(c(1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 1), ncol = 3)),
  mvtnorm::rmvnorm(100, mean = c(0, 0, 0),
                   sigma = matrix(c(1, 0, 0.5, 0, 1, 0.5, 0.5, 0.5, 1), ncol = 3))
))
colnames(d)[3:5] <- paste0("y", 1:3)

# only the mean changes, at 50
set.seed(1)
d.mean <- data.frame(trend = 1:200, foo = runif(200, -1, 1))
d.mean <- cbind(d.mean, rbind(
  rmvnorm(50, mean = c(0, 0, 0),
          sigma = matrix(c(1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 1), ncol = 3)),
  rmvnorm(150, mean = c(0, 1, 0),
          sigma = matrix(c(1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 1), ncol = 3))
))
colnames(d.mean)[3:5] <- paste0("y", 1:3)

# only the variance changes, at 150
set.seed(1)
d.var <- data.frame(trend = 1:200, foo = runif(200, -1, 1))
d.var <- cbind(d.var, rbind(
  rmvnorm(150, mean = c(0, 0, 0),
          sigma = matrix(c(1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 1), ncol = 3)),
  5*rmvnorm(50, mean = c(0, 0, 0),
            sigma = matrix(c(1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 1), ncol = 3))
))
colnames(d.var)[3:5] <- paste0("y", 1:3)

## ---- Testing ----

skip_on_cran()
test_that("default interface runs without errors, mob", {
  expect_error(
    networktree(nodevars = d[,3:5], splitvars = d[,1:2]),
    NA
  )
})

skip_on_cran()
test_that("formula interface runs without errors, mob", {
  expect_error(
    networktree(y1 + y2 + y3 ~ trend + foo, data = d),
    NA
  )
})

skip_on_cran()
test_that("ctree runs without errors", {
  expect_error(
    networktree(nodevars = d[,3:5], splitvars = d[,1:2], method = "ctree"),
    NA
  )
})

skip_on_cran()
test_that("mob runs without errors when partitioning by correlation, mean, and variance", {
  expect_error(
    networktree(nodevars=d[,3:5],splitvars=d[,1:2], model=c("correlation","mean","variance")),
    NA
  )
})

skip_on_cran()
test_that("ctree runs without errors when partitioning by correlation, mean, and variance", {
  expect_error(
    networktree(nodevars=d[,3:5],splitvars=d[,1:2], method = "ctree", model=c("correlation","mean","variance")),
    NA
  )
})

skip_on_cran()
test_that("mob runs on full workaholic data without errors", {
  expect_error(
    networktree(nodevars=workaholic[,paste("OCIR",1:18,sep="")], 
                splitvars=workaholic[,c("Workaholism_diagnosis","Gender")]),
    NA
  )
})

skip_on_cran()
test_that("ctree runs on workaholic data without errors", {
  expect_error(
    networktree(nodevars=workaholic[,paste("OCIR",1:18,sep="")], 
                splitvars=workaholic[,c("Workaholism_diagnosis")],
                method="ctree"),
    NA
  )
})

skip_on_cran()
test_that("mob runs on full dass data without errors", {
  expect_error(
    networktree(nodevars=dass[,1:42], splitvars=dass$age),
    NA
  )
})

skip_on_cran()
test_that("mob runs on full tipi data without errors", {
  expect_error(
    networktree(nodevars=tipi[,1:10], splitvars=tipi[,c("engnat", "age")]),
    NA
  )
})


skip_on_cran()
test_that("mob finds a split on generated data (d) when using trend", {
  expect_equal(
    length(networktree(nodevars = d[,3:5], splitvars = d$trend)) > 1,
    TRUE
  )
})

skip_on_cran()
test_that("mob finds NO split on generated data (d) when using foo", {
  expect_equal(
    length(networktree(nodevars = d[,3:5], splitvars = d$foo)) > 1,
    FALSE
  )
})

skip_on_cran()
test_that("mob finds a split on generated data by mean (d.mean) when using trend", {
  expect_equal(
    length(networktree(nodevars = d.mean[,3:5], splitvars = d.mean$trend, model="mean")) > 1,
    TRUE
  )
})

skip_on_cran()
test_that("mob finds NO split on generated data by mean (d.mean) when using foo", {
  expect_equal(
    length(networktree(nodevars = d.mean[,3:5], splitvars = d.mean$foo, model="mean")) > 1,
    FALSE
  )
})

skip_on_cran()
test_that("mob finds a split on generated data by variance (d.var) when using trend", {
  expect_equal(
    length(networktree(nodevars = d.var[,3:5], splitvars = d.var$trend, model="variance")) > 1,
    TRUE
  )
})

skip_on_cran()
test_that("mob finds NO split on generated data by mean (d.var) when using foo", {
  expect_equal(
    length(networktree(nodevars = d.var[,3:5], splitvars = d.var$foo, model="variance")) > 1,
    FALSE
  )
})

skip_on_cran()
test_that("ctree finds a split on generated data (d) when using trend", {
  expect_equal(
    length(networktree(nodevars = d[,3:5], splitvars = d$trend, method="ctree")) > 1,
    TRUE
  )
})

skip_on_cran()
test_that("ctree finds NO split on generated data (d) when using foo", {
  expect_equal(
    length(networktree(nodevars = d[,3:5], splitvars = d$foo, method="ctree")) > 1,
    FALSE
  )
})

skip_on_cran()
test_that("ctree finds a split on generated data by mean (d.mean) when using trend", {
  expect_equal(
    length(networktree(nodevars = d.mean[,3:5], splitvars = d.mean$trend, model="mean", method="ctree")) > 1,
    TRUE
  )
})

skip_on_cran()
test_that("ctree finds NO split on generated data by mean (d.mean) when using foo", {
  expect_equal(
    length(networktree(nodevars = d.mean[,3:5], splitvars = d.mean$foo, model="mean", method="ctree")) > 1,
    FALSE
  )
})

skip_on_cran()
test_that("ctree finds a split on generated data by variance (d.var) when using trend", {
  expect_equal(
    length(networktree(nodevars = d.var[,3:5], splitvars = d.var$trend, model="variance", method="ctree")) > 1,
    TRUE
  )
})

skip_on_cran()
test_that("ctree finds NO split on generated data by mean (d.var) when using foo", {
  expect_equal(
    length(networktree(nodevars = d.var[,3:5], splitvars = d.var$foo, model="variance", method="ctree")) > 1,
    FALSE
  )
})

# Note: ctree generates correlations without a population correction, 
# and hence returns slightly different values than mob or cor()
skip_on_cran()
cormat <- cor(d[,3:5]); diag(cormat) <- 0
test_that("getnetwork returns correlation matrix by default (with 0s in diagonal, in parity with qgraph)", {
  expect_equal(
    getnetwork(networktree(nodevars = d[,3:5], splitvars = d[,1:2]), id=1),
    cormat
  )
})

skip_on_cran()
cormat <- cor(d[,3:5])
pcormat <- getWmat(qgraph::qgraph(cormat, graph = "pcor",
                          DoNotPlot = TRUE))
rownames(pcormat) <- colnames(pcormat) <- colnames(cormat)
test_that("getnetwork dynamically transforms to pcor when specified", {
  expect_equal(
    getnetwork(networktree(nodevars = d[,3:5], splitvars = d[,1:2]), id=1, transform="pcor"),
    pcormat
  )
})

skip_on_cran()
cormat <- cor(d[,3:5])
glassomat <- suppressWarnings(getWmat(qgraph::qgraph(cormat, graph = "glasso",
                                  DoNotPlot = TRUE, sampleSize=nrow(d))))
rownames(glassomat) <- colnames(glassomat) <- colnames(cormat)
test_that("getnetwork dynamically transforms to glasso when specified", {
  expect_equal(
    getnetwork(networktree(nodevars = d[,3:5], splitvars = d[,1:2]), id=1, transform="glasso"),
    glassomat
  )
})

