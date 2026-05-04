testthat::test_that("id_metadata checks PMID vectors with NCBI", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_metadata(
    c("31452104", "999999999", NA_character_),
    type = "pmid",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out$provider,
    c("ncbi", NA_character_, NA_character_)
  )
  
  testthat::expect_true(
    grepl("Molegro Virtual Docker", out$title[[1]])
  )
  
  testthat::expect_identical(
    out$year,
    c(2019L, NA_integer_, NA_integer_)
  )
  
  testthat::expect_identical(
    out$pmid,
    c("31452104", NA_character_, NA_character_)
  )
  
  testthat::expect_identical(
    out$url[[1]],
    "https://pubmed.ncbi.nlm.nih.gov/31452104/"
  )
})