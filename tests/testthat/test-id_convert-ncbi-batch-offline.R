# tests/testthat/test-id_convert-ncbi-batch.R

testthat::test_that("id_convert batches PMID to PMCID through NCBI", {
  testthat::local_mocked_bindings(
    .convert_pmid_to_pmcid_ncbi_batch = function(x, ..., quiet = FALSE) {
      testthat::expect_identical(
        x,
        c("31469695", "999999999")
      )
      
      c("PMC6784763", NA_character_)
    }
  )
  
  out <- id_convert(
    c("31469695", "999999999", NA_character_),
    from = "pmid",
    to = "pmcid",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out,
    c("PMC6784763", NA_character_, NA_character_)
  )
})

testthat::test_that("id_convert batches PMCID to PMID through NCBI", {
  testthat::local_mocked_bindings(
    .convert_pmcid_to_pmid_ncbi_batch = function(x, ..., quiet = FALSE) {
      testthat::expect_identical(
        x,
        c("PMC6784763", "PMC999999999")
      )
      
      c("31469695", NA_character_)
    }
  )
  
  out <- id_convert(
    c("PMC6784763", "PMC999999999", NA_character_),
    from = "pmcid",
    to = "pmid",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out,
    c("31469695", NA_character_, NA_character_)
  )
})

testthat::test_that("id_convert batches PMCID to DOI through NCBI", {
  testthat::local_mocked_bindings(
    .convert_pmcid_to_doi_ncbi_batch = function(x, ..., quiet = FALSE) {
      testthat::expect_identical(
        x,
        c("PMC6784763", "PMC999999999")
      )
      
      c("10.1097/EDE.0000000000001091", NA_character_)
    }
  )
  
  out <- id_convert(
    c("PMC6784763", "PMC999999999", NA_character_),
    from = "pmcid",
    to = "doi",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out,
    c(
      "10.1097/EDE.0000000000001091",
      NA_character_,
      NA_character_
    )
  )
})

testthat::test_that("id_convert batches DOI to PMCID through NCBI", {
  testthat::local_mocked_bindings(
    .convert_doi_to_pmcid_ncbi_batch = function(x, ..., quiet = FALSE) {
      testthat::expect_identical(
        x,
        c("10.1097/EDE.0000000000001091", "10.0000/not-real")
      )
      
      c("PMC6784763", NA_character_)
    }
  )
  
  out <- id_convert(
    c(
      "10.1097/EDE.0000000000001091",
      "10.0000/not-real",
      NA_character_
    ),
    from = "doi",
    to = "pmcid",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out,
    c("PMC6784763", NA_character_, NA_character_)
  )
})


testthat::test_that("id_convert batches PMID to DOI through NCBI", {
  testthat::local_mocked_bindings(
    .convert_pmid_to_doi_ncbi_batch = function(x, ..., quiet = FALSE) {
      testthat::expect_identical(
        x,
        c("31469695", "999999999")
      )
      
      c("10.1097/EDE.0000000000001091", NA_character_)
    }
  )
  
  out <- id_convert(
    c("31469695", "999999999", NA_character_),
    from = "pmid",
    to = "doi",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out,
    c(
      "10.1097/EDE.0000000000001091",
      NA_character_,
      NA_character_
    )
  )
})


testthat::test_that("id_convert batches DOI to PMID through NCBI", {
  testthat::local_mocked_bindings(
    .convert_doi_to_pmid_ncbi_batch = function(x, ..., quiet = FALSE) {
      testthat::expect_identical(
        x,
        c("10.1097/EDE.0000000000001091", "10.0000/not-real")
      )
      
      c("31469695", NA_character_)
    }
  )
  
  out <- id_convert(
    c(
      "10.1097/EDE.0000000000001091",
      "10.0000/not-real",
      NA_character_
    ),
    from = "doi",
    to = "pmid",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out,
    c("31469695", NA_character_, NA_character_)
  )
})