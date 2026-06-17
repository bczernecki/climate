test_that("check_locale flags non-Polish locales", {

  old_locale = Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old_locale)), add = TRUE)

  if (!is.na(suppressWarnings(Sys.setlocale("LC_CTYPE", "en_GB.UTF-8"))) &&
      Sys.getlocale("LC_CTYPE") == "en_GB.UTF-8") {
    expect_equal(suppressMessages(check_locale()), 1)
  } else {
    skip("en_GB.UTF-8 locale not available")
  }
})
