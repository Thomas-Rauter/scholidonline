testthat::test_that(
  ".convert_pmid_to_doi() dispatches explicit providers",
  {
    testthat::local_mocked_bindings(
      .convert_pmid_to_doi_ncbi = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, "31469695")
        testthat::expect_identical(quiet, TRUE)
        "10.1/ncbi"
      },
      .convert_pmid_to_doi_epmc = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, "31469695")
        testthat::expect_identical(quiet, TRUE)
        "10.1/epmc"
      }
    )
    
    testthat::expect_identical(
      .convert_pmid_to_doi(
        x = "31469695",
        from = "pmid",
        to = "doi",
        provider = "ncbi",
        quiet = TRUE
      ),
      "10.1/ncbi"
    )
    
    testthat::expect_identical(
      .convert_pmid_to_doi(
        x = "31469695",
        from = "pmid",
        to = "doi",
        provider = "epmc",
        quiet = TRUE
      ),
      "10.1/epmc"
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid() auto returns NCBI hit before EPMC fallback",
  {
    testthat::local_mocked_bindings(
      .convert_doi_to_pmid_ncbi = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, "10.1/example")
        testthat::expect_identical(quiet, TRUE)
        "12345"
      },
      .convert_doi_to_pmid_epmc = function(x, ..., quiet = FALSE) {
        stop("EPMC fallback should not be called.", call. = FALSE)
      }
    )
    
    out <- .convert_doi_to_pmid(
      x = "10.1/example",
      from = "doi",
      to = "pmid",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, "12345")
  }
)

testthat::test_that(
  ".convert_pmcid_to_pmid() auto returns NCBI hit before EPMC fallback",
  {
    testthat::local_mocked_bindings(
      .convert_pmcid_to_pmid_ncbi = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, "PMC6784763")
        testthat::expect_identical(quiet, TRUE)
        "31469695"
      },
      .convert_pmcid_to_pmid_epmc = function(x, ..., quiet = FALSE) {
        stop("EPMC fallback should not be called.", call. = FALSE)
      }
    )
    
    out <- .convert_pmcid_to_pmid(
      x = "PMC6784763",
      from = "pmcid",
      to = "pmid",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, "31469695")
  }
)

testthat::test_that(
  ".convert_pmcid_to_pmid() warns when auto providers fail and quiet is FALSE",
  {
    testthat::local_mocked_bindings(
      .convert_pmcid_to_pmid_ncbi = function(x, ..., quiet = FALSE) {
        NA_character_
      },
      .convert_pmcid_to_pmid_epmc = function(x, ..., quiet = FALSE) {
        NA_character_
      }
    )
    
    testthat::expect_warning(
      out <- .convert_pmcid_to_pmid(
        x = "PMC0000000",
        from = "pmcid",
        to = "pmid",
        provider = "auto",
        quiet = FALSE
      ),
      "PMID for this PMCID could not be determined via NCBI or Europe PMC.",
      fixed = TRUE
    )
    
    testthat::expect_identical(out, NA_character_)
  }
)

testthat::test_that(
  ".convert_pmcid_to_doi() auto returns NCBI hit before EPMC fallback",
  {
    testthat::local_mocked_bindings(
      .convert_pmcid_to_doi_ncbi = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, "PMC6784763")
        testthat::expect_identical(quiet, TRUE)
        "10.1/ncbi"
      },
      .convert_pmcid_to_doi_epmc = function(x, ..., quiet = FALSE) {
        stop("EPMC fallback should not be called.", call. = FALSE)
      }
    )
    
    out <- .convert_pmcid_to_doi(
      x = "PMC6784763",
      from = "pmcid",
      to = "doi",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, "10.1/ncbi")
  }
)

testthat::test_that(
  ".convert_doi_to_pmcid() auto returns NCBI hit before EPMC fallback",
  {
    testthat::local_mocked_bindings(
      .convert_doi_to_pmcid_ncbi = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, "10.1/example")
        testthat::expect_identical(quiet, TRUE)
        "PMC123"
      },
      .convert_doi_to_pmcid_epmc = function(x, ..., quiet = FALSE) {
        stop("EPMC fallback should not be called.", call. = FALSE)
      }
    )
    
    out <- .convert_doi_to_pmcid(
      x = "10.1/example",
      from = "doi",
      to = "pmcid",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, "PMC123")
  }
)

testthat::test_that(
  "batch conversion dispatchers reject non-character input",
  {
    testthat::expect_error(
      .convert_pmid_to_doi_batch(
        x = 1,
        from = "pmid",
        to = "doi",
        provider = "ncbi",
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
    
    testthat::expect_error(
      .convert_doi_to_pmid_batch(
        x = 1,
        from = "doi",
        to = "pmid",
        provider = "ncbi",
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
    
    testthat::expect_error(
      .convert_pmid_to_pmcid_batch(
        x = 1,
        from = "pmid",
        to = "pmcid",
        provider = "ncbi",
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
    
    testthat::expect_error(
      .convert_pmcid_to_pmid_batch(
        x = 1,
        from = "pmcid",
        to = "pmid",
        provider = "ncbi",
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
    
    testthat::expect_error(
      .convert_pmcid_to_doi_batch(
        x = 1,
        from = "pmcid",
        to = "doi",
        provider = "ncbi",
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
    
    testthat::expect_error(
      .convert_doi_to_pmcid_batch(
        x = 1,
        from = "doi",
        to = "pmcid",
        provider = "ncbi",
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".convert_pmid_to_doi_batch() uses auto NCBI then EPMC fallback",
  {
    testthat::local_mocked_bindings(
      .convert_pmid_to_doi_ncbi_batch = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, c("1", "2"))
        testthat::expect_identical(quiet, TRUE)
        c("10.1/ncbi", NA_character_)
      },
      .convert_pmid_to_doi_epmc = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, "2")
        testthat::expect_identical(quiet, TRUE)
        "10.1/epmc"
      }
    )
    
    out <- .convert_pmid_to_doi_batch(
      x = c("1", "2"),
      from = "pmid",
      to = "doi",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(
      out,
      c("10.1/ncbi", "10.1/epmc")
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid_batch() uses auto NCBI then EPMC fallback",
  {
    testthat::local_mocked_bindings(
      .convert_doi_to_pmid_ncbi_batch = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, c("10.1/a", "10.1/b"))
        testthat::expect_identical(quiet, TRUE)
        c("1", NA_character_)
      },
      .convert_doi_to_pmid_epmc = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, "10.1/b")
        testthat::expect_identical(quiet, TRUE)
        "2"
      }
    )
    
    out <- .convert_doi_to_pmid_batch(
      x = c("10.1/a", "10.1/b"),
      from = "doi",
      to = "pmid",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(
      out,
      c("1", "2")
    )
  }
)

testthat::test_that(
  ".convert_pmid_to_pmcid_batch() uses auto NCBI then EPMC fallback",
  {
    testthat::local_mocked_bindings(
      .convert_pmid_to_pmcid_ncbi_batch = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, c("1", "2"))
        testthat::expect_identical(quiet, TRUE)
        c("PMC1", NA_character_)
      },
      .convert_pmid_to_pmcid_epmc = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, "2")
        testthat::expect_identical(quiet, TRUE)
        "PMC2"
      }
    )
    
    out <- .convert_pmid_to_pmcid_batch(
      x = c("1", "2"),
      from = "pmid",
      to = "pmcid",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(
      out,
      c("PMC1", "PMC2")
    )
  }
)

testthat::test_that(
  ".convert_pmcid_to_pmid_batch() uses auto NCBI then EPMC fallback",
  {
    testthat::local_mocked_bindings(
      .convert_pmcid_to_pmid_ncbi_batch = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, c("PMC1", "PMC2"))
        testthat::expect_identical(quiet, TRUE)
        c("1", NA_character_)
      },
      .convert_pmcid_to_pmid_epmc = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, "PMC2")
        testthat::expect_identical(quiet, TRUE)
        "2"
      }
    )
    
    out <- .convert_pmcid_to_pmid_batch(
      x = c("PMC1", "PMC2"),
      from = "pmcid",
      to = "pmid",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(
      out,
      c("1", "2")
    )
  }
)

testthat::test_that(
  ".convert_pmcid_to_doi_batch() uses auto NCBI then EPMC fallback",
  {
    testthat::local_mocked_bindings(
      .convert_pmcid_to_doi_ncbi_batch = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, c("PMC1", "PMC2"))
        testthat::expect_identical(quiet, TRUE)
        c("10.1/a", NA_character_)
      },
      .convert_pmcid_to_doi_epmc = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, "PMC2")
        testthat::expect_identical(quiet, TRUE)
        "10.1/b"
      }
    )
    
    out <- .convert_pmcid_to_doi_batch(
      x = c("PMC1", "PMC2"),
      from = "pmcid",
      to = "doi",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(
      out,
      c("10.1/a", "10.1/b")
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmcid_batch() uses auto NCBI then EPMC fallback",
  {
    testthat::local_mocked_bindings(
      .convert_doi_to_pmcid_ncbi_batch = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, c("10.1/a", "10.1/b"))
        testthat::expect_identical(quiet, TRUE)
        c("PMC1", NA_character_)
      },
      .convert_doi_to_pmcid_epmc = function(x, ..., quiet = FALSE) {
        testthat::expect_identical(x, "10.1/b")
        testthat::expect_identical(quiet, TRUE)
        "PMC2"
      }
    )
    
    out <- .convert_doi_to_pmcid_batch(
      x = c("10.1/a", "10.1/b"),
      from = "doi",
      to = "pmcid",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(
      out,
      c("PMC1", "PMC2")
    )
  }
)

testthat::test_that(
  "batch conversion dispatchers return NULL for unsupported explicit provider",
  {
    testthat::expect_null(
      .convert_pmid_to_doi_batch(
        x = "1",
        from = "pmid",
        to = "doi",
        provider = "epmc",
        quiet = TRUE
      )
    )
    
    testthat::expect_null(
      .convert_doi_to_pmid_batch(
        x = "10.1/a",
        from = "doi",
        to = "pmid",
        provider = "epmc",
        quiet = TRUE
      )
    )
    
    testthat::expect_null(
      .convert_pmid_to_pmcid_batch(
        x = "1",
        from = "pmid",
        to = "pmcid",
        provider = "epmc",
        quiet = TRUE
      )
    )
    
    testthat::expect_null(
      .convert_pmcid_to_pmid_batch(
        x = "PMC1",
        from = "pmcid",
        to = "pmid",
        provider = "epmc",
        quiet = TRUE
      )
    )
    
    testthat::expect_null(
      .convert_pmcid_to_doi_batch(
        x = "PMC1",
        from = "pmcid",
        to = "doi",
        provider = "epmc",
        quiet = TRUE
      )
    )
    
    testthat::expect_null(
      .convert_doi_to_pmcid_batch(
        x = "10.1/a",
        from = "doi",
        to = "pmcid",
        provider = "epmc",
        quiet = TRUE
      )
    )
  }
)