testthat::test_that(
  "id_metadata checks arXiv vectors against public API",
  {
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
    
    out <- id_metadata(
      x,
      type = "arxiv",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out$provider,
      c("arxiv", "arxiv", NA_character_, NA_character_)
    )
    
    testthat::expect_identical(
      out$year,
      c(2003L, 2007L, NA_integer_, NA_integer_)
    )
    
    testthat::expect_identical(
      out$container,
      c("arXiv", "arXiv", NA_character_, NA_character_)
    )
    
    testthat::expect_true(
      grepl("Multi-Electron Production", out$title[[1]])
    )
    
    testthat::expect_true(
      grepl("Elastic theory", out$title[[2]])
    )
    
    testthat::expect_identical(
      out$doi[[1]],
      "10.1140/epjc/s2003-01326-x"
    )
    
    testthat::expect_true(
      is.na(out$doi[[2]])
    )
  }
)