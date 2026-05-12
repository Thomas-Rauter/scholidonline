testthat::test_that("Europe PMC rate limiter waits between requests", {
  old_rate_limit <- getOption("scholidonline.rate_limit")
  old_interval <- getOption("scholidonline.epmc.min_interval")
  
  on.exit(
    {
      options(
        scholidonline.rate_limit = old_rate_limit,
        scholidonline.epmc.min_interval = old_interval
      )
    },
    add = TRUE
  )
  
  options(
    scholidonline.rate_limit = TRUE,
    scholidonline.epmc.min_interval = 0.1
  )
  
  .epmc_rate_limit_reset()
  
  .epmc_rate_limit(quiet = TRUE)
  
  start <- Sys.time()
  .epmc_rate_limit(quiet = TRUE)
  elapsed <- as.numeric(
    difftime(
      time1 = Sys.time(),
      time2 = start,
      units = "secs"
    )
  )
  
  testthat::expect_gte(
    elapsed,
    0.08
  )
})

testthat::test_that("Europe PMC rate limiter can be disabled", {
  old_rate_limit <- getOption("scholidonline.rate_limit")
  old_interval <- getOption("scholidonline.epmc.min_interval")
  
  on.exit(
    {
      options(
        scholidonline.rate_limit = old_rate_limit,
        scholidonline.epmc.min_interval = old_interval
      )
    },
    add = TRUE
  )
  
  options(
    scholidonline.rate_limit = FALSE,
    scholidonline.epmc.min_interval = 1
  )
  
  .epmc_rate_limit_reset()
  
  .epmc_rate_limit(quiet = TRUE)
  
  start <- Sys.time()
  .epmc_rate_limit(quiet = TRUE)
  elapsed <- as.numeric(
    difftime(
      time1 = Sys.time(),
      time2 = start,
      units = "secs"
    )
  )
  
  testthat::expect_lt(
    elapsed,
    0.5
  )
})

testthat::test_that("Europe PMC rate limiter accepts zero interval", {
  old_rate_limit <- getOption("scholidonline.rate_limit")
  old_interval <- getOption("scholidonline.epmc.min_interval")
  
  on.exit(
    {
      options(
        scholidonline.rate_limit = old_rate_limit,
        scholidonline.epmc.min_interval = old_interval
      )
    },
    add = TRUE
  )
  
  options(
    scholidonline.rate_limit = TRUE,
    scholidonline.epmc.min_interval = 0
  )
  
  .epmc_rate_limit_reset()
  
  .epmc_rate_limit(quiet = TRUE)
  
  start <- Sys.time()
  .epmc_rate_limit(quiet = TRUE)
  elapsed <- as.numeric(
    difftime(
      time1 = Sys.time(),
      time2 = start,
      units = "secs"
    )
  )
  
  testthat::expect_lt(
    elapsed,
    0.5
  )
})