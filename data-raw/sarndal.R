joint_prob <- (5 * (5 - 1)) / (50 * (50 - 1))
joint_probability <- matrix(rep(joint_prob, 25), ncol = 5)
diag(joint_probability) <- rep(0.1, 5)

sarndal <- list(
  data = data.frame(
    x = c(8, 10, 13, 12, 11, 8, 2, 7, 16, 10),
    psu_index = c("11", "11", "5", "5", "20", "20", "48", "48", "14", "14"),
    psu_probability = 0.1,
    psu_size = c(6, 6, 5, 5, 5, 5, 5, 5, 7, 7),
    ssu_probability = unlist(purrr::map(c(6, 5, 5, 5, 7), ~ rep(2 / .x, 2)))
  ),
  joint_probability = joint_probability
)

usethis::use_data(sarndal, overwrite = TRUE)
