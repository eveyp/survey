x <- c(8, 10, 13, 12, 11, 8, 2, 7, 16, 10)

psu_index <- c("11", "11", "5", "5", "20", "20", "48", "48", "14", "14")

psu_probability <- rep(0.1, 5)

psu_size <- c(6, 5, 5, 5, 7)

ssu_probability <- unlist(purrr::map(psu_size, ~ rep(2 / .x, 2)))

joint_prob <- (5 * (5 - 1)) / (50 * (50 - 1))
joint_probability <- matrix(rep(joint_prob, 25), ncol = 5)
diag(joint_probability) <- psu_probability

test_that("totals work", {
  total <- total_with_variance(
    x,
    psu_index,
    psu_probability,
    psu_size,
    ssu_probability,
    joint_probability
  )

  expect_equal(total["total"], 2775, ignore_attr = "names")
  expect_equal(total["variance"], 281958.7, ignore_attr = "names", tolerance = 0.1)
})

test_that("means work", {
  mean <- mean_with_variance(
    x,
    psu_index,
    psu_probability,
    psu_size,
    ssu_probability,
    joint_probability
  )

  expect_equal(mean["mean"], 9.9107, ignore_attr = "names", tolerance = 0.00001)
  expect_equal(mean["variance"], 2.07, ignore_attr = "names", tolerance = 0.01)
})

library(survey)
data("election")

dpps_ht <- svydesign(id = ~1, fpc = ~p, data = election_pps, pps = ppsmat(election_jointprob))

test_that("mean matches svymean", {
  svy_mean <- svymean(~Kerry, dpps_ht)
  mwv <- mean_with_variance(
    election_pps$Kerry,
    election_pps$County,
    election_pps$p,
    rep(1, nrow(election_pps)),
    rep(1, nrow(election_pps)),
    election_jointprob
  )

  expect_equal(mwv["mean"], svy_mean[1], ignore_attr = "names")
  expect_equal(
    mwv["variance"],
    attr(svy_mean, "var")[1, 1],
    ignore_attr = "names"
  )
})
