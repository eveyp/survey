#' Calculate a mean and variance from a two-stage sample
#'
#' The mean and its variance are calculated following the equations in [Model Assisted Survey Sampling by Sarndal, et al. (1992), pp. 314-5](http://www.ru.ac.bd/wp-content/uploads/sites/25/2019/03/306_06_Sarndal_Model-Assisted-Survey-Sampling-1992.pdf)
#'
#' @param x A numeric vector with the variable-of-interest's values.
#' @param psu_index A vector with the PSU membership for each element in the `x`
#'   vector.
#' @param psu_probability A vector with the PSU selection probabilities. It
#'   should be the same length as the number of unique PSUs.
#' @param psu_size A vector with the size of each PSU. It should be the same
#'   length as `psu_probability`.
#' @param ssu_probability A vector with the SSU selection probabilities (ie. the
#'   individual's selection probability given the selection of its PSU).
#' @param joint_probability A square matrix of the PSU joint selection
#'   probabilities. It's dimensions should be equal to the number of unique
#'   PSUs. The PSU selection probabilities should be on the diagonal.
#'
#' @return A named vector with the mean and its variance.
#' @export
#'
#' @seealso [svymean()] and internal functions in the
#'   `survey` package: `survey:::ppsvar()`
#'   `survey:::htvar.matrix()`, `survey:::pi2Dcheck()`.
mean_with_variance <- function(x,
                               psu_index,
                               psu_probability,
                               psu_size,
                               ssu_probability,
                               joint_probability) {
  overall_mean <- mean_estimator(
    x,
    psu_index,
    psu_probability,
    psu_size,
    ssu_probability
  )

  variance <- mean_variance(
    x,
    overall_mean,
    psu_index,
    psu_probability,
    psu_size,
    ssu_probability,
    joint_probability
  )

  c(mean = overall_mean, variance = variance)
}

#' @describeIn mean_with_variance Only calculate the mean.
#' @export
mean_estimator <- function(x,
                           psu_index,
                           psu_probability,
                           psu_size,
                           ssu_probability) {
  x_total <- sum(psu_totals(x, psu_index, ssu_probability) / psu_probability)
  n_total <- sum(psu_size / psu_probability)

  x_total / n_total
}

#' @describeIn mean_with_variance Only calculate the variance (given a
#'   pre-calculated mean).
#'
#' @param mean The survey mean of the variable.
#'
#' @export
mean_variance <- function(x,
                          mean,
                          psu_index,
                          psu_probability,
                          psu_size,
                          ssu_probability,
                          joint_probability) {
  residuals <- psu_totals(x, psu_index, ssu_probability) - psu_size * mean

  numerator <- sum(
    variance_term_1(residuals, psu_probability, joint_probability),
    variance_term_2(x, psu_index, psu_size, psu_probability)
  )

  denominator <- sum(psu_size / psu_probability)^2

  numerator / denominator
}

#' Calculate a total and variance from a two-stage sample
#'
#' The total and its variance are calculated following the equations in [Model Assisted Survey Sampling by Sarndal, et al. (1992), pp. 136-7](http://www.ru.ac.bd/wp-content/uploads/sites/25/2019/03/306_06_Sarndal_Model-Assisted-Survey-Sampling-1992.pdf)
#'
#' @param x A numeric vector with the variable-of-interest's values.
#' @param psu_index A vector with the PSU membership for each element in the `x`
#'   vector.
#' @param psu_probability A vector with the PSU selection probabilities. It
#'   should be the same length as the number of unique PSUs.
#' @param psu_size A vector with the size of each PSU. It should be the same
#'   length as `psu_probability`.
#' @param ssu_probability A vector with the SSU selection probabilities (ie. the
#'   individual's selection probability given the selection of its PSU).
#' @param joint_probability A square matrix of the PSU joint selection
#'   probabilities. It's dimensions should be equal to the number of unique
#'   PSUs. The PSU selection probabilities should be on the diagonal.
#'
#' @return A named vector with the total and its variance.
#' @export
#'
#' @seealso [svytotal()] and internal functions in the
#'   `survey` package: `survey:::ppsvar()`
#'   `survey:::htvar.matrix()`, `survey:::pi2Dcheck()`.
total_with_variance <- function(x,
                                psu_index,
                                psu_probability,
                                psu_size,
                                ssu_probability,
                                joint_probability) {
  psu_total <- psu_totals(x, psu_index, ssu_probability)
  overall_total <- sum(psu_total / psu_probability)

  variance <- total_variance(
    x,
    psu_index,
    psu_probability,
    psu_size,
    ssu_probability,
    joint_probability
  )

  c(total = overall_total, variance = variance)
}

#' @describeIn total_with_variance Only calculate the variance.
#' @export
total_variance <- function(x, psu_index, psu_probability, psu_size, ssu_probability, joint_probability) {
  sum(
    variance_term_1(
      psu_totals(x, psu_index, ssu_probability),
      psu_probability,
      joint_probability
    ),
    variance_term_2(x, psu_index, psu_size, psu_probability)
  )
}

psu_totals <- function(x, psu_index, ssu_probability) {
  if (!is.ordered(psu_index)) {
    psu_index <- as_ordered_factor(psu_index)
  }

  purrr::map2_dbl(
    split(x, psu_index), split(ssu_probability, psu_index),
    ~ sum(.x / .y)
  )
}

variance_term_1 <- function(x, psu_probability, joint_probability) {
  # weight the psu x's by dividing by the psu selection probability
  x_check <- x / psu_probability

  # setup the joint probability matrix with the survey package
  if (is.null(attr(joint_probability, "dcheck"))) {
    d_check <- pi2Dcheck(joint_probability)
  } else {
    d_check <- joint_probability
  }

  # finally multiply all the vectors/matrices to get the variance
  Matrix::crossprod(x_check, Matrix::crossprod(d_check, x_check))[1, 1]
}

variance_term_2 <- function(x, psu_index, psu_size, psu_probability) {
  # if psu_index isn't an ordered factor already, make it so
  if (!is.ordered(psu_index)) {
    psu_index <- as_ordered_factor(psu_index)
  }

  # calculate the variance within each psu
  intra_psu_variance <- purrr::map2_dbl(
    split(x, psu_index), psu_size,
    intra_psu_variance
  )

  # weight the within psu variance by dividing by the psu selection probability
  # then sum them to get the total within psu variance
  sum(intra_psu_variance / psu_probability)
}

intra_psu_variance <- function(x, psu_size) {
  # get the sample size from the length of the x vector
  n <- length(x)

  # an individual's selection probability is just the sample size / psu size
  ind_prob <- n / psu_size

  # weight the x's by dividing by the individual's selection probability
  x_check <- x / ind_prob

  # the joint selection probabilities are all n(n-1)/N(N-1)
  joint_prob <- (n * (n - 1)) / (psu_size * (psu_size - 1))

  # make the joint probabilities an n by n matrix to make the final calc. easier
  joint_prob_matrix <- matrix(rep(joint_prob, n^2), nrow = n)
  # put the individual selection probabilities on the diagonal
  diag(joint_prob_matrix) <- ind_prob

  # setup the joint probability matrix with the survey package
  d_check <- pi2Dcheck(joint_prob_matrix)

  # finally multiply all the vectors/matricies to get the variance
  Matrix::crossprod(x_check, Matrix::crossprod(d_check, x_check))[1, 1]
}

# argument checks
# check_numeric(x)
# check_numeric(psu_probability)
# check_integerish(psu_size)
# check_numeric(ssu_probability)
# check_square_matrix(joint_probability)
# check_same_length(x, psu_index, ssu_probability)
# check_same_length(psu_probability, psu_size)
# check_psu_consistency(psu_index, psu_probability)
# check_psu_consistency(psu_index, psu_size)

as_ordered_factor <- function(x) {
  factor(x, levels = unique(x), ordered = TRUE)
}

convert_survey_design <- function(design) {
  psu_index <- as_ordered_factor(design$cluster[, 1])
  ssu_probability <- design$allprob[, 2]

  psu_probability <- purrr::map_dbl(split(design$allprob[, 1], psu_index), max)

  psu_sample_sizes <- purrr::map_int(split(psu_index, psu_index), length)
  psu_size <- psu_sample_sizes / purrr::map_dbl(split(ssu_probability, psu_index), max)

  joint_probability <- design$dcheck[[1]]$dcheck
  attr(joint_probability, "dcheck") <- "dcheck"

  list(
    psu_index = psu_index,
    psu_probability = psu_probability,
    psu_size = psu_size,
    ssu_probability = ssu_probability,
    joint_probability = joint_probability
  )
}
