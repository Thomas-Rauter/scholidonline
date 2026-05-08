testthat::test_that("id_exists checks PMID vectors with NCBI", {
  skip_if_no_internet_for_live_tests()
  
  x <- c(
    "31452104",
    "999999999",
    NA_character_
  )
  
  probe <- .exists_pmid_ncbi_batch(
    x,
    quiet = TRUE
  )
  
  skip_if_ncbi_live_unavailable(probe)
  
  testthat::local_mocked_bindings(
    .exists_pmid_ncbi_batch = function(x, ..., quiet = FALSE) {
      probe[seq_along(x)]
    }
  )
  
  out <- id_exists(
    x,
    type = "pmid",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out,
    c(TRUE, FALSE, NA)
  )
})

testthat::test_that("id_exists checks PMCID vectors with NCBI", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_exists(
    c("PMC6784763", "PMC999999999", NA_character_),
    type = "pmcid",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out,
    c(TRUE, FALSE, NA)
  )
})