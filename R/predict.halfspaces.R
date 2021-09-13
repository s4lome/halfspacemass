#' Predict method for 'halfspace' objects
#'
#' Calculates halfspacemass or depth for given halfspaces and data. For more
#' details see [halfspaces()].
#'
#' @param object an object of class 'halfspace'
#' @param metric "mass" or "depth". Determines which metric should be calculated.
#' @param data matrix or dataframe in the same dimensionality like the given
#' 'halfspaces' object, for which halfspacemass shall be calculated
#' @param ... further arguments
#' @describeIn halfspaces predict halfspaces: Calculates halfspacemass or depth
#' for given halfspaces and data. For more
#' details see [halfspaces()].
#' @return A nrows((halfspaces$training_data)) x 1 matrix containing the halfspace
#' mass or depth for each data point in the training data of the given halfspace
#' object.
#'
#' @examples \dontrun{
#' set.seed(187471431)
#' # 2D standard Normal:
#' cluster <- data.frame(z1 = rnorm(50) / 2,
#'                       z2 = rnorm(50) / 2,
#'                       group = "cluster")
#' # polar coordinates: points with distance 3 to 5 from the origin, at 90° - 270°:
#'  left_anomalies <- data.frame(angle = runif(10, pi / 2, 3 * pi / 2),
#'                               length = runif(10, 3, 5))
#' # convert to cartesian coords
#' left_anomalies <- with(left_anomalies, data.frame(
#'                        z1 = length * cos(angle),
#'                        z2 = length * sin(angle),
#'                        group = "anomaly"))
#' # ~ N_2(\mu = (6,0), \Sigma = I_2)
#' right_anomalies <- data.frame(z1 = rnorm(20) / 5 + 6,
#'                               z2 = rnorm(20) / 5,
#'                               group = "anomaly")
#' data <- rbind(cluster,
#'                   left_anomalies,
#'                   right_anomalies)
#'
#' hs <- halfspaces(data[,1:2])
#' predict(hs, data[1:2])
#'
#' #############################################################################
#' set.seed(187471431)
#' n <- 500
#' cov_3d <- matrix(.9, 3, 3)
#' diag(cov_3d) <- rep(1, 3)
#' cluster_3d <- as.data.frame(mvtnorm::rmvnorm(n = n, sigma = cov_3d))
#' colnames(cluster_3d) <- paste0("z", 1:3)
#' anomalies_3d <- data.frame(
#' angle1 = runif(n / 10, 0, 2 * pi),
#' angle2 = runif(n / 10, 0, 2 * pi), length = runif(n / 10, 5, 7))
#' anomalies_3d <- with(
#' anomalies_3d,
#' data.frame(
#' z1 = length * cos(angle1) * sin(angle2),
#' z2 = length * sin(angle1) * sin(angle2), z3 = length * cos(angle2)))
#' data_3d <- rbind(cluster_3d, anomalies_3d)
#'
#' hs_3d <- halfspaces(data_3d, n_halfspace = 3e3, subsample = .5, seed = 34)
#'
#' data_3d$`Halfspace Mass` <- predict(data = data_3d[, 1:3], object = hs_3d)
#'
#'
#' data_3d$`Neg. Mahalanobis` <- -1 * mahalanobis(data_3d[, 1:3],
#'                                              center = TRUE,
#'                                              cov = cov(data_3d[, 1:3]))
#'
#' data_3d$Group <- c(rep("cluster", n), rep("anomaly", n / 10))
#' # Data:
#' pairs(data_3d[, 1:3],
#'      col = c(rgb(0, 0, 0, .5),
#'      rgb(1, 0, 0, .2))[(data_3d$Group == "cluster") + 1])
#' }
#'
#' @import checkmate
#' @md
#' @export


predict.halfspaces <- function(object, data, metric = c("mass", "depth"), ...) {
  # input checking
  if (inherits(data, "data.frame")) data <- as.matrix(data)
  checkmate::assert_matrix(data,
                           mode = "numeric", any.missing = FALSE, min.cols = 2,
                           min.rows = 1)
  check_halfspaces(halfspaces = object, data = data)
  if (attr(object, "rescale")) data <- scale(data)
  #calculate metric
  evaluate_depth(data = data,
                 halfspaces = object,
                 metric = metric,
                 rescale = attr(object, "rescale"))
}
