context('utils')

test_that("%||% works", {
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 1, 1)
  expect_equal(1 %||% NULL, 1)
})
