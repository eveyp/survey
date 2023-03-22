data("sarndal")

test_that("totals work", {
  total <- total_with_variance(
    sarndal$data$x,
    sarndal$data$psu_index,
    sarndal$data$psu_probability[1:5],
    sarndal$data$psu_size[seq(1, 9, 2)],
    sarndal$data$ssu_probability,
    sarndal$joint_probability
  )

  expect_equal(total["total"], 2775, ignore_attr = "names")
  expect_equal(total["variance"], 281958.7, ignore_attr = "names", tolerance = 0.1)
})

test_that("means work", {
  mean <- mean_with_variance(
    sarndal$data$x,
    sarndal$data$psu_index,
    sarndal$data$psu_probability[1:5],
    sarndal$data$psu_size[seq(1, 9, 2)],
    sarndal$data$ssu_probability,
    sarndal$joint_probability
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
