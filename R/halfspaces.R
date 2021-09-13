#' Function for creating an object of class 'halfspaces' and methods for updating,
#' plotting, predicting
#'
#' Create halfspace object, which is characterized by the training data, the
#' the number of estimated halfspaces, subsample-number, scope, if the data
#' has been rescaled, and the given seed. Follows the reasoning of Chen et. al
#' described in this
#' \href{https://link.springer.com/article/10.1007/s10994-015-5524-x}{article}
#'
#' @param data data on which halfspaces shall be trained
#'             rows are vectors / points to project, dataframe or matrix
#' @param n_halfspace number of halfspaces that shall be drawn, numeric
#' @param subsample what proportion of data to use for each halfspace
#'                  computation, numeric
#' @param scope controls size of region of convexity for halfspace mass
#'              i.e., how far outside of sampled data range the sampled
#'              hyperplanes can lie, numeric >= 1
#' @param rescale determines if data should be rescaled, logical
#' @param seed optional RNG seed
#'
#' @return An object of class 'halfspaces' containing a  list of n_halfspace
#'         halfspaces, defined by their normal vector and offset from origin,
#'         with estimated data frequencies above/below halfspace boundary, and
#'         attributes according to the inputs.
#' @examples \dontrun{
#' # 1000 2-dimensional halfspaces with default values
#' data <- matrix(ncol = 2, data = rnorm(200))
#' halfspaces(data)
#'
#' # 1000 non rescaled, reproducable  5-dimensional halfspaces with scope = 2
#' data <- matrix(ncol = 5, data = rnorm(1000, 5, 10))
#' halfspaces(data, scope = 2, rescale = FALSE, seed = 1312)
#'
#' # 2000 3-dimensional halfspaces with lowered subsample
#' data <- matrix(ncol = 3, data = rnorm(300))
#' halfspaces(data, subsample = 0.5)
#'
#' # 3000 3-dimensional Gaussian-distributed with strong correlation and
#' 10% outliers
#'
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
#' hs_3d <- halfspaces(data_3d, n_halfspace = 3e3, subsample = .5, seed = 34)
#'
#' data_3d$Group <- c(rep("cluster", n), rep("anomaly", n / 10))
#' # Data:
#' pairs(data_3d[, 1:3],
#'      col = c(rgb(0, 0, 0, .5),
#'      rgb(1, 0, 0, .2))[(data_3d$Group == "cluster") + 1])
#' }
#' @import checkmate
#' @md
#' @export

halfspaces <- function(data, n_halfspace = 1e3, subsample = 1, scope = 1,
                       rescale = TRUE, seed = NULL) {
  validate_halfspaces(
    new_halfspaces(data, n_halfspace, subsample, scope, rescale, seed))
}

# Constructor ##################################################################
#' Constructor function for creating an object of class 'halfspaces'
#'
#' Construct an object of class 'halfspaces' and assign given attributes.
#'
#' @inheritParams halfspaces
#' @return an object of class 'halfspaces' containing a  list of n_halfspace
#'         halfspaces, defined by their normal vector and offset from origin,
#'         with estimated data frequencies above/below halfspace boundary, and
#'         attributes according to the inputs.
#' @import checkmate
#' @md

new_halfspaces <- function(data, n_halfspace, subsample, scope, rescale, seed) {
  # type checking
  checkmate::assert(checkmate::check_matrix(data, any.missing = FALSE),
                    checkmate::check_data_frame(data, any.missing = FALSE))
  checkmate::assert_integerish(n_halfspace)
  checkmate::assert_number(subsample)
  checkmate::assert_number(scope, na.ok = FALSE, finite = TRUE)
  checkmate::assert_integerish(seed, null.ok = TRUE)
  checkmate::assert_logical(rescale, len = 1)

  # create structure according to given attributes and assign class
  structure(train_depth(data = data,
                        n_halfspace = n_halfspace,
                        subsample = subsample,
                        scope = scope,
                        rescale = rescale,
                        seed = seed),
            class = "halfspaces")
}

# Validation ###################################################################
#' Validator function for creating an object of class 'halfspaces'
#'
#' Validates if the given inputs are sufficient to create an object of class
#' 'halfspaces'
#'
#' @param halfspaces a object of class halfspace to be validated
#' @return an object of class 'halfspaces' containing a  list of n_halfspace
#'         halfspaces, defined by their normal vector and offset from origin,
#'         with estimated data frequencies above/below halfspace boundary, and
#'         attributes according to the inputs.
#' @import checkmate
#' @md

validate_halfspaces <- function(halfspaces) {
  # validation checks
  checkmate::assert_class(halfspaces, "halfspaces")
  checkmate::assert_number(attr(halfspaces, "n_halfspace"), lower = 1)
  checkmate::assert_number(attr(halfspaces, "seed"), null.ok = TRUE, lower = 1L)
  checkmate::assert_number(attr(halfspaces, "scope"), lower = 1)
  checkmate::assert_matrix(attr(halfspaces,"train_data"),
                           mode = "numeric", min.cols = 2, min.rows = 1)
  checkmate::assert_number(attr(halfspaces, "subsample"), lower = 1e-3, upper = 1)
  check_halfspaces(halfspaces = halfspaces, attr(halfspaces,"train_data"))

  # return object
  halfspaces
}
