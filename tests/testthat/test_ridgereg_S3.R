test_that("ridgereg coefficients are similar to lm.ridge", {
  data(mtcars)
  lambda <- 0.1

  my_model <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = lambda)
  mass_model <- lm.ridge(mpg ~ wt + hp, data = mtcars, lambda = lambda)

  my_coefs <- coef(my_model)
  mass_coefs <- coef(mass_model)

  expect_equal(unname(my_coefs), unname(mass_coefs), tolerance = 0.05)
})

