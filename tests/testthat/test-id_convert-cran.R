testthat::test_that(
  "id_convert (CRAN/offline): converts PMID -> DOI with declared types",
  {
    testthat::local_mocked_bindings(
      .convert_pmid_to_doi = function(
    x,
    provider = "auto",
    ...,
    quiet = FALSE
      ) {
        paste0("10.1000/", x)
      },
    .scholidonline_conversion_providers = function(from, to) {
      "mock"
    },
    .scholidonline_match_provider = function(provider, choices) {
      "mock"
    },
    .package = "scholidonline"
    )
    
    out <- scholidonline::id_convert(
      x    = "12345678",
      from = "pmid",
      to   = "doi"
    )
    
    testthat::expect_type(out, "character")
    testthat::expect_length(out, 1L)
    testthat::expect_identical(out, "10.1000/12345678")
  }
)

testthat::test_that(
  "id_convert (CRAN/offline): from = NULL infers type per element",
  {
    testthat::local_mocked_bindings(
      .convert_pmid_to_doi = function(
    x,
    provider = "auto",
    ...,
    quiet = FALSE
      ) {
        paste0("10.1000/", x)
      },
    .scholidonline_conversion_providers = function(from, to) {
      "mock"
    },
    .scholidonline_match_provider = function(provider, choices) {
      "mock"
    },
    .package = "scholidonline"
    )
    
    x <- c(
      "12345678",
      "not an id",
      NA
    )
    
    out <- scholidonline::id_convert(
      x    = x,
      from = NULL,
      to   = "doi"
    )
    
    testthat::expect_type(out, "character")
    testthat::expect_length(out, length(x))
    testthat::expect_identical(out[1], "10.1000/12345678")
    testthat::expect_true(is.na(out[2]))
    testthat::expect_true(is.na(out[3]))
  }
)

testthat::test_that(
  "id_convert (CRAN/offline): errors on invalid `to` type",
  {
    testthat::expect_error(
      scholidonline::id_convert(
        x    = "12345678",
        from = "pmid",
        to   = "notatype"
      ),
      "'arg' should be one of",
      fixed = FALSE
    )
  }
)

testthat::test_that(
  "id_convert (CRAN/offline): errors on unsupported conversion",
  {
    testthat::expect_error(
      scholidonline::id_convert(
        x        = "31452104",
        from     = "pmid",
        to       = "orcid",
        provider = "mock"
      ),
      "Unsupported conversion: pmid -> orcid.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  "id_convert (CRAN/offline): converts DOI -> PMCID with declared types",
  {
    testthat::local_mocked_bindings(
      .convert_doi_to_pmcid = function(
    x,
    provider = "auto",
    ...,
    quiet = FALSE
      ) {
        "PMC1234567"
      },
    .scholidonline_conversion_providers = function(from, to) {
      "mock"
    },
    .scholidonline_match_provider = function(provider, choices) {
      "mock"
    },
    .package = "scholidonline"
    )
    
    out <- scholidonline::id_convert(
      x    = "10.1000/12345678",
      from = "doi",
      to   = "pmcid"
    )
    
    testthat::expect_type(out, "character")
    testthat::expect_length(out, 1L)
    testthat::expect_identical(out, "PMC1234567")
  }
)