scalar_check_bindings <- function() {
  list(
    .scholidonline_check_scalar_chr = function(x) {
      invisible(TRUE)
    },
    .package = "scholidonline"
  )
}


testthat::test_that(
  ".exists_arxiv() uses arxiv provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_arxiv_arxiv = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "1234.5678")
          testthat::expect_true(quiet)
          TRUE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_true(
      .exists_arxiv(
        x = "1234.5678",
        provider = "auto",
        quiet = TRUE
      )
    )
  }
)


testthat::test_that(
  ".exists_arxiv() dispatches explicit arxiv provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_arxiv_arxiv = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "1234.5678")
          testthat::expect_false(quiet)
          FALSE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_false(
      .exists_arxiv(
        x = "1234.5678",
        provider = "arxiv",
        quiet = FALSE
      )
    )
  }
)


testthat::test_that(
  ".exists_arxiv() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .exists_arxiv(
        x = "1234.5678",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)


testthat::test_that(
  ".exists_doi() uses doi.org provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_doi_doi_org = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "10.1000/test")
          testthat::expect_true(quiet)
          TRUE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_true(
      .exists_doi(
        x = "10.1000/test",
        provider = "auto",
        quiet = TRUE
      )
    )
  }
)


testthat::test_that(
  ".exists_doi() dispatches explicit doi.org provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_doi_doi_org = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "10.1000/test")
          testthat::expect_false(quiet)
          FALSE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_false(
      .exists_doi(
        x = "10.1000/test",
        provider = "doi.org",
        quiet = FALSE
      )
    )
  }
)


testthat::test_that(
  ".exists_doi() dispatches explicit crossref provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_doi_crossref = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "10.1000/test")
          testthat::expect_true(quiet)
          TRUE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_true(
      .exists_doi(
        x = "10.1000/test",
        provider = "crossref",
        quiet = TRUE
      )
    )
  }
)


testthat::test_that(
  ".exists_doi() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .exists_doi(
        x = "10.1000/test",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)


testthat::test_that(
  ".exists_orcid() uses orcid provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_orcid_orcid = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "0000-0002-1825-0097")
          testthat::expect_true(quiet)
          TRUE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_true(
      .exists_orcid(
        x = "0000-0002-1825-0097",
        provider = "auto",
        quiet = TRUE
      )
    )
  }
)


testthat::test_that(
  ".exists_orcid() dispatches explicit orcid provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_orcid_orcid = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "0000-0002-1825-0097")
          testthat::expect_false(quiet)
          FALSE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_false(
      .exists_orcid(
        x = "0000-0002-1825-0097",
        provider = "orcid",
        quiet = FALSE
      )
    )
  }
)


testthat::test_that(
  ".exists_orcid() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .exists_orcid(
        x = "0000-0002-1825-0097",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)


testthat::test_that(
  ".exists_pmcid() auto returns NCBI result when not NA",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    epmc_called <- FALSE
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "PMC123")
          testthat::expect_true(quiet)
          TRUE
        },
        .exists_pmcid_epmc = function(x, ..., quiet = FALSE) {
          epmc_called <<- TRUE
          FALSE
        },
        .package = "scholidonline"
      )
    )
    
    out <- .exists_pmcid(
      x = "PMC123",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_true(out)
    testthat::expect_false(epmc_called)
  }
)


testthat::test_that(
  ".exists_pmcid() auto falls back to EPMC when NCBI is NA",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_true(quiet)
          NA
        },
        .exists_pmcid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_true(quiet)
          FALSE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_false(
      .exists_pmcid(
        x = "PMC123",
        provider = "auto",
        quiet = FALSE
      )
    )
  }
)


testthat::test_that(
  ".exists_pmcid() auto warns and returns NA when both providers are NA",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          NA
        },
        .exists_pmcid_epmc = function(x, ..., quiet = FALSE) {
          NA
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_warning(
      out <- .exists_pmcid(
        x = "PMC123",
        provider = "auto",
        quiet = FALSE
      ),
      "PMCID existence could not be determined via NCBI or Europe PMC\\."
    )
    
    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".exists_pmcid() auto returns NA silently when quiet is TRUE",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          NA
        },
        .exists_pmcid_epmc = function(x, ..., quiet = FALSE) {
          NA
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_identical(
      .exists_pmcid(
        x = "PMC123",
        provider = "auto",
        quiet = TRUE
      ),
      NA
    )
  }
)


testthat::test_that(
  ".exists_pmcid() dispatches explicit ncbi provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "PMC123")
          testthat::expect_false(quiet)
          TRUE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_true(
      .exists_pmcid(
        x = "PMC123",
        provider = "ncbi",
        quiet = FALSE
      )
    )
  }
)


testthat::test_that(
  ".exists_pmcid() dispatches explicit epmc provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_pmcid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "PMC123")
          testthat::expect_true(quiet)
          FALSE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_false(
      .exists_pmcid(
        x = "PMC123",
        provider = "epmc",
        quiet = TRUE
      )
    )
  }
)


testthat::test_that(
  ".exists_pmcid() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .exists_pmcid(
        x = "PMC123",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)


testthat::test_that(
  ".exists_pmid() auto returns NCBI result when not NA",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    epmc_called <- FALSE
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_pmid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "12345")
          testthat::expect_true(quiet)
          FALSE
        },
        .exists_pmid_epmc = function(x, ..., quiet = FALSE) {
          epmc_called <<- TRUE
          TRUE
        },
        .package = "scholidonline"
      )
    )
    
    out <- .exists_pmid(
      x = "12345",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_false(out)
    testthat::expect_false(epmc_called)
  }
)


testthat::test_that(
  ".exists_pmid() auto falls back to EPMC when NCBI is NA",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_pmid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_true(quiet)
          NA
        },
        .exists_pmid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_true(quiet)
          TRUE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_true(
      .exists_pmid(
        x = "12345",
        provider = "auto",
        quiet = FALSE
      )
    )
  }
)


testthat::test_that(
  ".exists_pmid() auto warns and returns NA when both providers are NA",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_pmid_ncbi = function(x, ..., quiet = FALSE) {
          NA
        },
        .exists_pmid_epmc = function(x, ..., quiet = FALSE) {
          NA
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_warning(
      out <- .exists_pmid(
        x = "12345",
        provider = "auto",
        quiet = FALSE
      ),
      "PMID existence could not be determined via NCBI or Europe PMC\\."
    )
    
    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".exists_pmid() auto returns NA silently when quiet is TRUE",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_pmid_ncbi = function(x, ..., quiet = FALSE) {
          NA
        },
        .exists_pmid_epmc = function(x, ..., quiet = FALSE) {
          NA
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_identical(
      .exists_pmid(
        x = "12345",
        provider = "auto",
        quiet = TRUE
      ),
      NA
    )
  }
)


testthat::test_that(
  ".exists_pmid() dispatches explicit ncbi provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_pmid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "12345")
          testthat::expect_false(quiet)
          TRUE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_true(
      .exists_pmid(
        x = "12345",
        provider = "ncbi",
        quiet = FALSE
      )
    )
  }
)


testthat::test_that(
  ".exists_pmid() dispatches explicit epmc provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_pmid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "12345")
          testthat::expect_true(quiet)
          FALSE
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_false(
      .exists_pmid(
        x = "12345",
        provider = "epmc",
        quiet = TRUE
      )
    )
  }
)


testthat::test_that(
  ".exists_pmid() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .exists_pmid(
        x = "12345",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)