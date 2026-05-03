testthat::test_that("id_exists checks PMID vectors with NCBI", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_exists(
    c("31452104", "999999999", NA_character_),
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