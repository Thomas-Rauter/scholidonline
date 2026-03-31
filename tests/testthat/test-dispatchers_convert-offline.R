mock_scalar_chr_check <- function() {
  testthat::local_mocked_bindings(
    .scholidonline_check_scalar_chr = function(x) invisible(x)
  )
}

testthat::test_that(
  "PMID -> DOI auto uses NCBI first and falls back to Europe PMC",
  {
    mock_scalar_chr_check()
    
    testthat::local_mocked_bindings(
      .convert_pmid_to_doi_ncbi = function(x, ..., quiet = FALSE) {
        "10.1000/ncbi"
      },
      .convert_pmid_to_doi_epmc = function(x, ..., quiet = FALSE) {
        "10.1000/epmc"
      }
    )
    
    testthat::expect_equal(
      .convert_pmid_to_doi(
        x = "123",
        from = "pmid",
        to = "doi",
        provider = "auto"
      ),
      "10.1000/ncbi"
    )
    
    testthat::local_mocked_bindings(
      .convert_pmid_to_doi_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_pmid_to_doi_epmc = function(x, ..., quiet = FALSE) {
        "10.1000/epmc"
      }
    )
    
    testthat::expect_equal(
      .convert_pmid_to_doi(
        x = "123",
        from = "pmid",
        to = "doi",
        provider = "auto"
      ),
      "10.1000/epmc"
    )
  }
)


testthat::test_that(
  "PMID -> DOI auto warns and returns NA if no provider succeeds",
  {
    mock_scalar_chr_check()
    
    testthat::local_mocked_bindings(
      .convert_pmid_to_doi_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_pmid_to_doi_epmc = function(x, ..., quiet = FALSE) {
        NA_character_
      }
    )
    
    testthat::expect_warning(
      out <- .convert_pmid_to_doi(
        x = "123",
        from = "pmid",
        to = "doi",
        provider = "auto"
      ),
      "could not be determined via NCBI or Europe PMC"
    )
    
    testthat::expect_true(is.na(out))
    
    testthat::expect_no_warning(
      out_quiet <- .convert_pmid_to_doi(
        x = "123",
        from = "pmid",
        to = "doi",
        provider = "auto",
        quiet = TRUE
      )
    )
    
    testthat::expect_true(is.na(out_quiet))
  }
)


testthat::test_that(
  "explicit provider dispatch works for DOI -> PMID",
  {
    mock_scalar_chr_check()
    
    testthat::local_mocked_bindings(
      .convert_doi_to_pmid_ncbi = function(x, ..., quiet = FALSE) {
        "111"
      },
      .convert_doi_to_pmid_epmc = function(x, ..., quiet = FALSE) {
        "222"
      }
    )
    
    testthat::expect_equal(
      .convert_doi_to_pmid(
        x = "10.1000/xyz",
        from = "doi",
        to = "pmid",
        provider = "ncbi"
      ),
      "111"
    )
    
    testthat::expect_equal(
      .convert_doi_to_pmid(
        x = "10.1000/xyz",
        from = "doi",
        to = "pmid",
        provider = "epmc"
      ),
      "222"
    )
  }
)


testthat::test_that(
  "unknown provider aborts for PMCID -> PMID and DOI -> PMCID",
  {
    mock_scalar_chr_check()
    
    testthat::expect_error(
      .convert_pmcid_to_pmid(
        x = "PMC123",
        from = "pmcid",
        to = "pmid",
        provider = "bogus"
      ),
      "Unknown provider: bogus"
    )
    
    testthat::expect_error(
      .convert_doi_to_pmcid(
        x = "10.1000/xyz",
        from = "doi",
        to = "pmcid",
        provider = "bogus"
      ),
      "Unknown provider: bogus"
    )
  }
)


testthat::test_that(
  "auto fallback logic works for PMCID -> DOI and PMID -> PMCID",
  {
    mock_scalar_chr_check()
    
    testthat::local_mocked_bindings(
      .convert_pmcid_to_doi_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_pmcid_to_doi_epmc = function(x, ..., quiet = FALSE) {
        "10.2000/epmc"
      },
      .convert_pmid_to_pmcid_ncbi = function(x, ..., quiet = FALSE) {
        "PMC999"
      },
      .convert_pmid_to_pmcid_epmc = function(x, ..., quiet = FALSE) {
        "PMC888"
      }
    )
    
    testthat::expect_equal(
      .convert_pmcid_to_doi(
        x = "PMC123",
        from = "pmcid",
        to = "doi",
        provider = "auto"
      ),
      "10.2000/epmc"
    )
    
    testthat::expect_equal(
      .convert_pmid_to_pmcid(
        x = "123",
        from = "pmid",
        to = "pmcid",
        provider = "auto"
      ),
      "PMC999"
    )
  }
)


testthat::test_that(
  "auto fallback logic works for PMCID -> PMID and DOI -> PMCID",
  {
    mock_scalar_chr_check()
    
    testthat::local_mocked_bindings(
      .convert_pmcid_to_pmid_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_pmcid_to_pmid_epmc = function(x, ..., quiet = FALSE) {
        "321"
      },
      .convert_doi_to_pmcid_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_doi_to_pmcid_epmc = function(x, ..., quiet = FALSE) {
        "PMC321"
      }
    )
    
    testthat::expect_equal(
      .convert_pmcid_to_pmid(
        x = "PMC123",
        from = "pmcid",
        to = "pmid",
        provider = "auto"
      ),
      "321"
    )
    
    testthat::expect_equal(
      .convert_doi_to_pmcid(
        x = "10.1000/xyz",
        from = "doi",
        to = "pmcid",
        provider = "auto"
      ),
      "PMC321"
    )
  }
)


mock_scalar_chr_check <- function() {
  testthat::local_mocked_bindings(
    .scholidonline_check_scalar_chr = function(x) invisible(x)
  )
}


testthat::test_that(
  ".convert_doi_to_pmid auto falls back to epmc and can warn",
  {
    mock_scalar_chr_check()
    
    testthat::local_mocked_bindings(
      .convert_doi_to_pmid_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_doi_to_pmid_epmc = function(x, ..., quiet = FALSE) {
        "12345"
      }
    )
    
    testthat::expect_equal(
      .convert_doi_to_pmid(
        x = "10.1000/xyz",
        from = "doi",
        to = "pmid",
        provider = "auto"
      ),
      "12345"
    )
    
    testthat::local_mocked_bindings(
      .convert_doi_to_pmid_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_doi_to_pmid_epmc = function(x, ..., quiet = FALSE) {
        NA_character_
      }
    )
    
    testthat::expect_warning(
      out <- .convert_doi_to_pmid(
        x = "10.1000/xyz",
        from = "doi",
        to = "pmid",
        provider = "auto"
      ),
      "PMID for this DOI could not be determined"
    )
    
    testthat::expect_true(is.na(out))
    
    testthat::expect_no_warning(
      out_quiet <- .convert_doi_to_pmid(
        x = "10.1000/xyz",
        from = "doi",
        to = "pmid",
        provider = "auto",
        quiet = TRUE
      )
    )
    
    testthat::expect_true(is.na(out_quiet))
  }
)


testthat::test_that(
  ".convert_pmcid_to_pmid explicit providers and quiet NA branch work",
  {
    mock_scalar_chr_check()
    
    testthat::local_mocked_bindings(
      .convert_pmcid_to_pmid_ncbi = function(x, ..., quiet = FALSE) {
        "111"
      },
      .convert_pmcid_to_pmid_epmc = function(x, ..., quiet = FALSE) {
        "222"
      }
    )
    
    testthat::expect_equal(
      .convert_pmcid_to_pmid(
        x = "PMC123",
        from = "pmcid",
        to = "pmid",
        provider = "ncbi"
      ),
      "111"
    )
    
    testthat::expect_equal(
      .convert_pmcid_to_pmid(
        x = "PMC123",
        from = "pmcid",
        to = "pmid",
        provider = "epmc"
      ),
      "222"
    )
    
    testthat::local_mocked_bindings(
      .convert_pmcid_to_pmid_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_pmcid_to_pmid_epmc = function(x, ..., quiet = FALSE) {
        NA_character_
      }
    )
    
    testthat::expect_no_warning(
      out <- .convert_pmcid_to_pmid(
        x = "PMC123",
        from = "pmcid",
        to = "pmid",
        provider = "auto",
        quiet = TRUE
      )
    )
    
    testthat::expect_true(is.na(out))
  }
)


testthat::test_that(
  ".convert_pmcid_to_doi explicit providers and warn branch work",
  {
    mock_scalar_chr_check()
    
    testthat::local_mocked_bindings(
      .convert_pmcid_to_doi_ncbi = function(x, ..., quiet = FALSE) {
        "10.1000/ncbi"
      },
      .convert_pmcid_to_doi_epmc = function(x, ..., quiet = FALSE) {
        "10.1000/epmc"
      }
    )
    
    testthat::expect_equal(
      .convert_pmcid_to_doi(
        x = "PMC123",
        from = "pmcid",
        to = "doi",
        provider = "ncbi"
      ),
      "10.1000/ncbi"
    )
    
    testthat::expect_equal(
      .convert_pmcid_to_doi(
        x = "PMC123",
        from = "pmcid",
        to = "doi",
        provider = "epmc"
      ),
      "10.1000/epmc"
    )
    
    testthat::local_mocked_bindings(
      .convert_pmcid_to_doi_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_pmcid_to_doi_epmc = function(x, ..., quiet = FALSE) {
        NA_character_
      }
    )
    
    testthat::expect_warning(
      out <- .convert_pmcid_to_doi(
        x = "PMC123",
        from = "pmcid",
        to = "doi",
        provider = "auto"
      ),
      "DOI for this PMCID could not be determined"
    )
    
    testthat::expect_true(is.na(out))
  }
)


testthat::test_that(
  ".convert_pmid_to_pmcid falls back, warns, and dispatches explicitly",
  {
    mock_scalar_chr_check()
    
    testthat::local_mocked_bindings(
      .convert_pmid_to_pmcid_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_pmid_to_pmcid_epmc = function(x, ..., quiet = FALSE) {
        "PMC999"
      }
    )
    
    testthat::expect_equal(
      .convert_pmid_to_pmcid(
        x = "123",
        from = "pmid",
        to = "pmcid",
        provider = "auto"
      ),
      "PMC999"
    )
    
    testthat::local_mocked_bindings(
      .convert_pmid_to_pmcid_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_pmid_to_pmcid_epmc = function(x, ..., quiet = FALSE) {
        NA_character_
      }
    )
    
    testthat::expect_warning(
      out <- .convert_pmid_to_pmcid(
        x = "123",
        from = "pmid",
        to = "pmcid",
        provider = "auto"
      ),
      "PMCID for this PMID could not be determined"
    )
    
    testthat::expect_true(is.na(out))
    
    testthat::local_mocked_bindings(
      .convert_pmid_to_pmcid_ncbi = function(x, ..., quiet = FALSE) {
        "PMC111"
      },
      .convert_pmid_to_pmcid_epmc = function(x, ..., quiet = FALSE) {
        "PMC222"
      }
    )
    
    testthat::expect_equal(
      .convert_pmid_to_pmcid(
        x = "123",
        from = "pmid",
        to = "pmcid",
        provider = "ncbi"
      ),
      "PMC111"
    )
    
    testthat::expect_equal(
      .convert_pmid_to_pmcid(
        x = "123",
        from = "pmid",
        to = "pmcid",
        provider = "epmc"
      ),
      "PMC222"
    )
  }
)


testthat::test_that(
  ".convert_doi_to_pmcid explicit providers and warn branch work",
  {
    mock_scalar_chr_check()
    
    testthat::local_mocked_bindings(
      .convert_doi_to_pmcid_ncbi = function(x, ..., quiet = FALSE) {
        "PMC111"
      },
      .convert_doi_to_pmcid_epmc = function(x, ..., quiet = FALSE) {
        "PMC222"
      }
    )
    
    testthat::expect_equal(
      .convert_doi_to_pmcid(
        x = "10.1000/xyz",
        from = "doi",
        to = "pmcid",
        provider = "ncbi"
      ),
      "PMC111"
    )
    
    testthat::expect_equal(
      .convert_doi_to_pmcid(
        x = "10.1000/xyz",
        from = "doi",
        to = "pmcid",
        provider = "epmc"
      ),
      "PMC222"
    )
    
    testthat::local_mocked_bindings(
      .convert_doi_to_pmcid_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_doi_to_pmcid_epmc = function(x, ..., quiet = FALSE) {
        NA_character_
      }
    )
    
    testthat::expect_warning(
      out <- .convert_doi_to_pmcid(
        x = "10.1000/xyz",
        from = "doi",
        to = "pmcid",
        provider = "auto"
      ),
      "PMCID for this DOI could not be determined"
    )
    
    testthat::expect_true(is.na(out))
  }
)


testthat::test_that(
  "all convert dispatchers abort on unknown provider",
  {
    mock_scalar_chr_check()
    
    testthat::expect_error(
      .convert_pmid_to_doi(
        x = "123",
        from = "pmid",
        to = "doi",
        provider = "bogus"
      ),
      "Unknown provider: bogus"
    )
    
    testthat::expect_error(
      .convert_doi_to_pmid(
        x = "10.1000/xyz",
        from = "doi",
        to = "pmid",
        provider = "bogus"
      ),
      "Unknown provider: bogus"
    )
    
    testthat::expect_error(
      .convert_pmcid_to_pmid(
        x = "PMC123",
        from = "pmcid",
        to = "pmid",
        provider = "bogus"
      ),
      "Unknown provider: bogus"
    )
    
    testthat::expect_error(
      .convert_pmcid_to_doi(
        x = "PMC123",
        from = "pmcid",
        to = "doi",
        provider = "bogus"
      ),
      "Unknown provider: bogus"
    )
    
    testthat::expect_error(
      .convert_pmid_to_pmcid(
        x = "123",
        from = "pmid",
        to = "pmcid",
        provider = "bogus"
      ),
      "Unknown provider: bogus"
    )
    
    testthat::expect_error(
      .convert_doi_to_pmcid(
        x = "10.1000/xyz",
        from = "doi",
        to = "pmcid",
        provider = "bogus"
      ),
      "Unknown provider: bogus"
    )
  }
)
