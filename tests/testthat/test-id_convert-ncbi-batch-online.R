# tests/testthat/test-id_convert-ncbi-batch-online.R

testthat::test_that("id_convert checks PMID to PMCID vectors with NCBI", {
  skip_if_no_internet_for_live_tests()
  
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

testthat::test_that("id_convert checks PMCID to PMID vectors with NCBI", {
  skip_if_no_internet_for_live_tests()
  
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

testthat::test_that("id_convert checks PMCID to DOI vectors with NCBI", {
  skip_if_no_internet_for_live_tests()
  
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

testthat::test_that("id_convert checks DOI to PMCID vectors with NCBI", {
  skip_if_no_internet_for_live_tests()
  
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

testthat::test_that("id_convert batches NCBI conversions with auto provider", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_convert(
    c("31469695", "999999999", NA_character_),
    from = "pmid",
    to = "pmcid",
    provider = "auto",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out,
    c("PMC6784763", NA_character_, NA_character_)
  )
})


testthat::test_that("id_convert checks PMID to DOI vectors with NCBI", {
  skip_if_no_internet_for_live_tests()
  
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


testthat::test_that("id_convert checks DOI to PMID vectors with NCBI", {
  skip_if_no_internet_for_live_tests()
  
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