testthat::test_that("id_metadata uses arXiv batch helper for vectors", {
  testthat::local_mocked_bindings(
    .meta_arxiv_arxiv_batch = function(x, ..., quiet = FALSE) {
      testthat::expect_identical(
        x,
        c("hep-ex/0307015", "0706.0001", "1234.12345")
      )
      
      data.frame(
        arxiv_id = c("hep-ex/0307015v1", "0706.0001v2"),
        title = c(
          "Multi-Electron Production at HERA",
          "Elastic theory of low-dimensional continua"
        ),
        year = c(2003L, 2007L),
        container = c("arXiv", "arXiv"),
        doi = c("10.1140/epjc/s2003-01326-x", NA_character_),
        pmid = c(NA_character_, NA_character_),
        pmcid = c(NA_character_, NA_character_),
        url = c(
          "http://arxiv.org/abs/hep-ex/0307015v1",
          "http://arxiv.org/abs/0706.0001v2"
        ),
        provider = c("arxiv", "arxiv"),
        stringsAsFactors = FALSE
      )
    }
  )
  
  out <- id_metadata(
    c("hep-ex/0307015", "0706.0001", "1234.12345", NA_character_),
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
    out$doi,
    c(
      "10.1140/epjc/s2003-01326-x",
      NA_character_,
      NA_character_,
      NA_character_
    )
  )
})