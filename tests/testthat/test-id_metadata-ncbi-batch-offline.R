testthat::test_that("id_metadata uses NCBI PMID batch helper", {
  testthat::local_mocked_bindings(
    .meta_pmid_ncbi_batch = function(x, ..., quiet = FALSE) {
      testthat::expect_identical(
        x,
        c("31452104", "999999999")
      )
      
      data.frame(
        pmid_key = "31452104",
        title = "Molegro Virtual Docker for Docking.",
        year = 2019L,
        container = "Methods Mol Biol",
        doi = NA_character_,
        pmid = "31452104",
        pmcid = NA_character_,
        url = "https://pubmed.ncbi.nlm.nih.gov/31452104/",
        provider = "ncbi",
        stringsAsFactors = FALSE
      )
    }
  )
  
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
  
  testthat::expect_identical(
    out$title,
    c(
      "Molegro Virtual Docker for Docking.",
      NA_character_,
      NA_character_
    )
  )
  
  testthat::expect_identical(
    out$year,
    c(2019L, NA_integer_, NA_integer_)
  )
  
  testthat::expect_identical(
    out$pmid,
    c("31452104", NA_character_, NA_character_)
  )
})


testthat::test_that("id_metadata uses NCBI PMCID batch helper", {
  testthat::local_mocked_bindings(
    .meta_pmcid_ncbi_batch = function(x, ..., quiet = FALSE) {
      testthat::expect_identical(
        x,
        c("PMC6784763", "PMC999999999")
      )
      
      data.frame(
        pmcid_key = "PMC6784763",
        title = paste(
          "Talc, Asbestos, and Epidemiology:",
          "Corporate Influence and Scientific Incognizance."
        ),
        year = 2019L,
        container = "Epidemiology",
        doi = NA_character_,
        pmid = NA_character_,
        pmcid = "PMC6784763",
        url = paste0(
          "https://www.ncbi.nlm.nih.gov/pmc/articles/",
          "PMC6784763/"
        ),
        provider = "ncbi",
        stringsAsFactors = FALSE
      )
    }
  )
  
  out <- id_metadata(
    c("PMC6784763", "PMC999999999", NA_character_),
    type = "pmcid",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out$provider,
    c("ncbi", NA_character_, NA_character_)
  )
  
  testthat::expect_true(
    grepl("Talc, Asbestos", out$title[[1]])
  )
  
  testthat::expect_identical(
    out$year,
    c(2019L, NA_integer_, NA_integer_)
  )
  
  testthat::expect_identical(
    out$pmcid,
    c("PMC6784763", NA_character_, NA_character_)
  )
})