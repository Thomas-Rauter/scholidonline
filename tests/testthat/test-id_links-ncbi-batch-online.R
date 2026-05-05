testthat::test_that("id_links checks PMID vectors with NCBI", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_links(
    c("31469695", "999999999", NA_character_),
    type = "pmid",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out$query,
    c("31469695", "31469695")
  )
  
  testthat::expect_identical(
    out$query_type,
    c("pmid", "pmid")
  )
  
  testthat::expect_identical(
    out$linked_type,
    c("pmcid", "doi")
  )
  
  testthat::expect_identical(
    out$linked_id,
    c("PMC6784763", "10.1097/EDE.0000000000001091")
  )
  
  testthat::expect_identical(
    out$provider,
    c("ncbi", "ncbi")
  )
})

testthat::test_that("id_links checks PMCID vectors with NCBI", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_links(
    c("PMC6784763", "PMC999999999", NA_character_),
    type = "pmcid",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out$query,
    c("PMC6784763", "PMC6784763")
  )
  
  testthat::expect_identical(
    out$query_type,
    c("pmcid", "pmcid")
  )
  
  testthat::expect_identical(
    out$linked_type,
    c("pmid", "doi")
  )
  
  testthat::expect_identical(
    out$linked_id,
    c("31469695", "10.1097/EDE.0000000000001091")
  )
  
  testthat::expect_identical(
    out$provider,
    c("ncbi", "ncbi")
  )
})