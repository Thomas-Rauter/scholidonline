testthat::test_that("NCBI rate limiter waits between requests", {
  old_rate_limit <- getOption("scholidonline.rate_limit")
  old_interval <- getOption("scholidonline.ncbi.min_interval")
  
  on.exit(
    {
      options(
        scholidonline.rate_limit = old_rate_limit,
        scholidonline.ncbi.min_interval = old_interval
      )
    },
    add = TRUE
  )
  
  options(
    scholidonline.rate_limit = TRUE,
    scholidonline.ncbi.min_interval = 0.1
  )
  
  .ncbi_rate_limit_reset()
  
  .ncbi_rate_limit(quiet = TRUE)
  
  start <- Sys.time()
  .ncbi_rate_limit(quiet = TRUE)
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

testthat::test_that("NCBI rate limiter can be disabled", {
  old_rate_limit <- getOption("scholidonline.rate_limit")
  old_interval <- getOption("scholidonline.ncbi.min_interval")
  
  on.exit(
    {
      options(
        scholidonline.rate_limit = old_rate_limit,
        scholidonline.ncbi.min_interval = old_interval
      )
    },
    add = TRUE
  )
  
  options(
    scholidonline.rate_limit = FALSE,
    scholidonline.ncbi.min_interval = 1
  )
  
  .ncbi_rate_limit_reset()
  
  .ncbi_rate_limit(quiet = TRUE)
  
  start <- Sys.time()
  .ncbi_rate_limit(quiet = TRUE)
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

testthat::test_that(
  "arXiv rate-limit interval normalization validates numeric input",
  {
    testthat::expect_error(
      .scholidonline_rate_limit_normalize_interval(
        min_interval = c(0.1, 0.2),
        service = "arxiv",
        default = 0.3
      ),
      "`min_interval` must be a single numeric value.",
      fixed = TRUE
    )
    testthat::expect_null(
      .scholidonline_rate_limit_normalize_interval(
        min_interval = NA_real_,
        service = "arxiv",
        default = 0.3
      )
    )
    testthat::expect_identical(
      .scholidonline_rate_limit_normalize_interval(
        min_interval = 0.2,
        service = "arxiv",
        default = 0.3
      ),
      0.2
    )
  }
)