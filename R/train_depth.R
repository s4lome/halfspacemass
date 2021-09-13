#' Helper function for halfspaces()
#'
#' implements Algorithm 1 of Chen et al, according to [halfspaces()]
#'
#' @inheritParams halfspaces
#'
#' @return A list of n_halfspaces halfspaces, with attributes set according
#'         to inputs. Compare [halfspaces()].
#'
#' @import checkmate
#' @md

train_depth <-
  function(data, n_halfspace = 1e3, subsample = 1, scope = 1, seed = NULL,
           rescale = TRUE) {
    # rescale data per default
    if (rescale) data <- scale(data)
    # input homogenizatuon
    if (inherits(data, "data.frame")) data <- as.matrix(data)
    # set seed if given
    if (!is.null(seed)) {set.seed(as.integer(seed))}

    dims <- ncol(data)
    normals <- get_directions(n_halfspace, dims = dims)

    # create halfspaces
    halfspaces <- apply(normals, 2, get_halfspace,
                        data = data, subsample = subsample, scope = scope
    )
    # set attributes
    structure(halfspaces,
              n_halfspace = n_halfspace,
              seed = seed,
              scope = scope,
              rescale = rescale,
              train_data = data,
              subsample = subsample)
  }

################################################################################
#' Helper function train_depth()
#'
#' returns normal direction & location of halfspace boundary and relative
#' frequencies of data above/below it
#'
#' @param normal output of get_directions() helper function
#' @inheritParams train_depth
#'
#' @return a list with entries
#'   normal: (as given),
#'   split: the split point along normal defining the halfspace,
#'   mass: a named vector giving the relative frequencies of subsampled data that
#'     l   ie above and below the halfspace boundary.
#' @md

get_halfspace <- function(normal, data, subsample, scope) {
  # subsample data
  sampledata <- subsample(data, subsample)

   # get porjections
  projections <- project_scalar(sampledata, normal)

  # get splits
  split <- sample_split(projections, scope)

  list(
    normal = normal,
    split = split,
    mass_above = compute_mass(projections, split)
  )
}

################################################################################
#' Helper function for train_depth()
#'
#' uniformly sample n d-dimensional direction vectors (i.e., points on the unit
#'  sphere in d dimensions, see
#'  {http://mathworld.wolfram.com/SpherePointPicking.html.}, eq. 16)
#'  used as normal vectors of the (hyper-)planes defining the halfspaces
#'
#' @inheritParams train_depth
#' @param dims required dimension
#'
#' @return a <dims> x <n> matrix of <n> <dims>-dimensional directions
#' @import checkmate stats
#' @md

get_directions <- function(n_halfspace, dims = 2) {
  checkmate::assert_integerish(dims, lower = 2)
  # no need to scale to length 1 here since later steps adjust for length anyway
  matrix(rnorm(dims * n_halfspace), nrow = dims)
}


################################################################################
#' Helper function for get_halfspace()
#'
#' compute projection(s) of point(s) on a vector
#' Equations: {https://en.wikipedia.org/wiki/Scalar_projection}
#' used to project data points onto the normal vector defining the orientation
#' of a halfspace
#'
#' @inheritParams train_depth
#' @param direction Output of get_directions()
#'
#' @return numeric nrow(data) vector: projections of <data> on <direction> in
#'         units of "length of <direction>"
#' @import checkmate
#' @md

project_scalar <- function(data, direction) {
  checkmate::assert_numeric(direction, any.missing = FALSE, min.len = 2)
  checkmate::assert_matrix(data,
                           any.missing = FALSE, ncols = length(direction),
                           mode = "numeric"
  )
  # would be crossprod(x, d)/sqrt(crossprod(d)) for column vectors but
  # no need to scale with sqrt(crossprod(d)) since split is drawn from
  # observed projection values
  data %*% direction
}

################################################################################
#' Helper function for get_halfspace()
#'
#' sample a number in (midrange - scope/2 (max-min), midrange + scope/2 (max-min))
#' i.e. from (min, max) with scope = 1, from (mid, mid) with  scope = 0
#'
#' @inheritParams train_depth
#' @param projections Output of project_scalar()
#'
#' @return "offset" of the halfspace boundary along its normal vector
#' @import checkmate stats
#' @md

sample_split <- function(projections, scope) {
  checkmate::assert_numeric(projections, any.missing = FALSE, finite = TRUE)
  minmax <- range(projections)
  span <- minmax[2] - minmax[1]
  mid <- mean(minmax)
  runif(1, min = mid - scope / 2 * span, max = mid + scope / 2 * span)
}

################################################################################
#' Helper function for get_halfspace()
#'
#' get proportion of observations above boundary separating 2 halfspaces
#'
#' @param split output of sample_split()
#' @param projections output of project_scalar()
#'
#' @return a vector with relative data frequency "at or above" the boundary
#'         defined by split
#' @import checkmate
#' @md

compute_mass <- function(projections, split) {
  checkmate::assert_numeric(projections, any.missing = FALSE, finite = TRUE)
  checkmate::assert_number(split, finite = TRUE)
  mean(projections >= split)
}

################################################################################
#' Helper function for get_halfspace()
#'
#' subsample a proportion <subsample> from <data>
#'
#' @inheritParams train_depth
#'
#' @return a subsample of data, size depending on given subsample value
#' @md


# subsample a proportion <subsample> from <data>
subsample <- function(data, subsample = 1) {
  nrows <- NROW(data)
  subsample_size <- round(subsample * seq_len(nrows))
  subsample_size <- min(max(1, subsample_size), nrows)
  if (subsample_size == nrows) return(data)
  use <- sample(seq_len(nrows), subsample_size)
  data[use, ]
}
