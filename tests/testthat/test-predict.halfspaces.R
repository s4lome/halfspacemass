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

hs <- halfspaces(data[,1:2], seed = 1337, rescale = FALSE)

# compare method output to calculation function output


test_that("predict method calculates metric accuratley",
          expect_equal(all(evaluate_depth(hs, data = data[,1:2]) ==
                             predict(hs, data = data[,1:2])),
                       TRUE)
)
