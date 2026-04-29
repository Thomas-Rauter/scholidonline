testthat::test_that("id_links uses arXiv batch helper for vectors", {
  testthat::local_mocked_bindings(
    .links_arxiv_arxiv_batch = function(x, ..., quiet = FALSE) {
      testthat::expect_identical(
        x,
        c("hep-ex/0307015", "0706.0001", "1503.07589", "1234.12345")
      )
      
      data.frame(
        arxiv_id = c("hep-ex/0307015v1", "1503.07589v1"),
        linked_type = c("doi", "doi"),
        linked_value = c(
          "10.1140/epjc/s2003-01326-x",
          "10.1103/PhysRevLett.114.191803"
        ),
        provider = c("arxiv", "arxiv"),
        stringsAsFactors = FALSE
      )
    }
  )
  
  out <- id_links(
    c(
      "hep-ex/0307015",
      "0706.0001",
      "1503.07589",
      "1234.12345",
      NA_character_
    ),
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