testthat::test_that("arXiv batch existence preserves input order", {
  xml <- paste0(
    '<feed xmlns="http://www.w3.org/2005/Atom">',
    '<entry>',
    '<id>http://arxiv.org/abs/0706.0001v2</id>',
    '</entry>',
    '<entry>',
    '<id>http://arxiv.org/abs/hep-ex/0307015v1</id>',
    '</entry>',
    '</feed>'
  )
  
  testthat::local_mocked_bindings(
    .arxiv_query_id_list = function(x, ..., quiet = FALSE) {
      xml
    }
  )
  
  testthat::expect_identical(
    .exists_arxiv_arxiv_batch(
      c("hep-ex/0307015", "1234.12345", "0706.0001"),
      quiet = TRUE
    ),
    c(TRUE, FALSE, TRUE)
  )
})

testthat::test_that("arXiv batch existence handles missing inputs", {
  xml <- paste0(
    '<feed xmlns="http://www.w3.org/2005/Atom">',
    '<entry>',
    '<id>http://arxiv.org/abs/0706.0001v2</id>',
    '</entry>',
    '</feed>'
  )
  
  testthat::local_mocked_bindings(
    .arxiv_query_id_list = function(x, ..., quiet = FALSE) {
      xml
    }
  )
  
  testthat::expect_identical(
    .exists_arxiv_arxiv_batch(
      c("0706.0001", NA_character_, ""),
      quiet = TRUE
    ),
    c(TRUE, NA, NA)
  )
})

testthat::test_that("arXiv XML ID extraction ignores feed IDs", {
  xml <- paste0(
    '<feed xmlns="http://www.w3.org/2005/Atom">',
    '<id>http://arxiv.org/api/query?search_query=all</id>',
    '<entry>',
    '<id>http://arxiv.org/abs/0706.0001v2</id>',
    '</entry>',
    '<entry>',
    '<id>http://arxiv.org/api/errors#incorrect_id</id>',
    '</entry>',
    '</feed>'
  )
  
  testthat::expect_identical(
    .arxiv_extract_entry_ids(xml),
    "0706.0001v2"
  )
})

testthat::test_that("arXiv version stripping removes trailing versions", {
  testthat::expect_identical(
    .arxiv_strip_version(
      c("0706.0001v2", "hep-ex/0307015v1", "1234.12345")
    ),
    c("0706.0001", "hep-ex/0307015", "1234.12345")
  )
})

testthat::test_that("arXiv rate limiter can be disabled", {
  old_rate_limit <- getOption("scholidonline.rate_limit")
  old_interval <- getOption("scholidonline.arxiv.min_interval")
  
  on.exit(
    {
      options(
        scholidonline.rate_limit = old_rate_limit,
        scholidonline.arxiv.min_interval = old_interval
      )
    },
    add = TRUE
  )
  
  options(
    scholidonline.rate_limit = FALSE,
    scholidonline.arxiv.min_interval = 10
  )
  
  .arxiv_rate_limit_reset()
  .scholidonline_arxiv_state$last_request_time <- Sys.time()
  
  start <- Sys.time()
  .arxiv_rate_limit(quiet = TRUE)
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  
  testthat::expect_lt(elapsed, 0.1)
})

testthat::test_that("arXiv rate limiter waits between requests", {
  old_rate_limit <- getOption("scholidonline.rate_limit")
  old_interval <- getOption("scholidonline.arxiv.min_interval")
  
  on.exit(
    {
      options(
        scholidonline.rate_limit = old_rate_limit,
        scholidonline.arxiv.min_interval = old_interval
      )
    },
    add = TRUE
  )
  
  options(
    scholidonline.rate_limit = TRUE,
    scholidonline.arxiv.min_interval = 0.1
  )
  
  .arxiv_rate_limit_reset()
  
  .arxiv_rate_limit(quiet = TRUE)
  
  start <- Sys.time()
  .arxiv_rate_limit(quiet = TRUE)
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  
  testthat::expect_gte(elapsed, 0.08)
})