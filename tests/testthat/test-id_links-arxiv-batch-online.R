testthat::test_that("id_links checks arXiv vectors against public API", {
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
    "1503.07589",
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
  
  out <- id_links(
    x,
    type = "arxiv",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out$query,
    c("hep-ex/0307015", "1503.07589")
  )
  
  testthat::expect_identical(
    out$query_type,
    c("arxiv", "arxiv")
  )
  
  testthat::expect_identical(
    out$linked_type,
    c("doi", "doi")
  )
  
  testthat::expect_identical(
    out$linked_id,
    c(
      "10.1140/epjc/s2003-01326-x",
      "10.1103/PhysRevLett.114.191803"
    )
  )
  
  testthat::expect_identical(
    out$provider,
    c("arxiv", "arxiv")
  )
})