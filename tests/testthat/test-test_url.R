
test_that("test_url", {
  output = tempfile()
  expect_message(test_url(link = "http://httpbin.org/status/404",
                          output = output))
})
