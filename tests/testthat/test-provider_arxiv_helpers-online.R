testthat::test_that("arXiv batch helper works against public API", {
  skip_if_no_internet_for_live_tests()
  
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
    scholidonline.arxiv.min_interval = 3
  )
  
  .arxiv_rate_limit_reset()
  
  x <- c(
    "hep-ex/0307015",
    "0706.0001",
    "1234.12345",
    NA_character_
  )
  
  txt <- .arxiv_query_id_list(
    x,
    quiet = TRUE
  )
  
  skip_if_arxiv_live_unavailable(txt)
  
  testthat::local_mocked_bindings(
    .arxiv_query_id_list = function(x, ..., quiet = FALSE) {
      txt
    }
  )
  
  out <- .exists_arxiv_arxiv_batch(
    x,
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out,
    c(TRUE, TRUE, FALSE, NA)
  )
})

testthat::test_that("arXiv batch query returns article entry IDs", {
  skip_if_no_internet_for_live_tests()
  
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
    scholidonline.arxiv.min_interval = 3
  )
  
  .arxiv_rate_limit_reset()
  
  txt <- .arxiv_query_id_list(
    c("hep-ex/0307015", "0706.0001", "1234.12345"),
    quiet = TRUE
  )
  
  skip_if_arxiv_live_unavailable(txt)
  
  ids <- .arxiv_extract_entry_ids(txt)
  
  testthat::expect_true(
    "hep-ex/0307015v1" %in% ids
  )
  
  testthat::expect_true(
    any(.arxiv_strip_version(ids) == "0706.0001")
  )
  
  testthat::expect_false(
    "1234.12345" %in% .arxiv_strip_version(ids)
  )
})