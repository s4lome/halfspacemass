library(testthat)

set.seed(187471431)
# 2D standard Normal:
cluster <- data.frame(
  z1 = rnorm(50) / 2,
  z2 = rnorm(50) / 2,
  group = "cluster"
)
# polar coordinates: points with distance 3 to 5 from the origin, at 90° - 270°:
left_anomalies <- data.frame(
  angle = runif(10, pi / 2, 3 * pi / 2),
  length = runif(10, 3, 5)
)
# convert to cartesian coords
left_anomalies <- with(left_anomalies, data.frame(
  z1 = length * cos(angle),
  z2 = length * sin(angle),
  group = "anomaly"
))
# ~ N_2(\mu = (6,0), \Sigma = I_2)
right_anomalies <- data.frame(
  z1 = rnorm(20) / 5 + 6,
  z2 = rnorm(20) / 5,
  group = "anomaly"
)
data <- rbind(cluster,
                   left_anomalies,
                   right_anomalies)

# deleting halfspaces
test1 <- halfspaces(data[1:40,1:2], n_halfspace = 10, seed = 1337, rescale = FALSE)
test2 <- update.halfspaces(test1, n_halfspace = 5)

test_that("update method keeps halfspaces", {
  expect_equal(test1[[6]]$split,
               test2[[1]]$split)
  expect_equal(test1[[6]]$normal,
               test2[[1]]$normal)
})

# adding new halfspaces
test3 <- update.halfspaces(test1, n_halfspace = 15)

test_that("update method draws new halfspaces for higher n", {

  expect_equal(all(test3[[1]]$split != test3[[11]]$split),
               TRUE)
  expect_equal(all(test3[[1]]$normal != test3[[11]]$normal),
               TRUE)
})

# add traning data
test4 <- update.halfspaces(test1, data = data[60:80,1:2], add = TRUE)
test_that("add training data works correctly",
  expect_equal(all(attr(test4, "train_data") ==
                   rbind(data[1:40,1:2], data[60:80,1:2])),
               TRUE)
)

# replace training data
test4 <- update.halfspaces(test1, data = data[60:80,1:2], add = FALSE)
test_that("add training data works correctly",
          expect_equal(all(attr(test4, "train_data") == data[60:80,1:2]),
                       TRUE)
)





