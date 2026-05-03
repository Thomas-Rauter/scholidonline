testthat::test_that("id_exists uses NCBI PMID batch helper", {
  testthat::local_mocked_bindings(
    .exists_pmid_ncbi_batch = function(x, ..., quiet = FALSE) {
      testthat::expect_identical(
        x,
        c("31452104", "999999999")
      )
      
      c(TRUE, FALSE)
    }
  )
  
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

testthat::test_that("id_exists uses NCBI PMCID batch helper", {
  testthat::local_mocked_bindings(
    .exists_pmcid_ncbi_batch = function(x, ..., quiet = FALSE) {
      testthat::expect_identical(
        x,
        c("PMC6784763", "PMC999999999")
      )
      
      c(TRUE, FALSE)
    }
  )
  
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