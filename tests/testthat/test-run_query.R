context('run_query')

test_that('it can run a simple query', {
  expect_equal(run_query('select 1 + 1', dbconn())[[1]], 2)
})
