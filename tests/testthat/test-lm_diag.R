test_that("lm_diag test", {
  #create a lm model
  lm_life_gdp <- lm(lifeExp ~ gdpPercap, data = gapminder::gapminder)
  #expect error if input length = 0
  expect_error(lm_diag(lm_life_gdp, alpha = numeric(0)))
  #expect output type is list when output.p set as TRUE
  expect_type(lm_diag(lm_life_gdp, output.p = TRUE), "list")
  #expect error when alpha is out of range
  expect_error(lm_diag(lm_life_gdp, alpha = 2))
})
