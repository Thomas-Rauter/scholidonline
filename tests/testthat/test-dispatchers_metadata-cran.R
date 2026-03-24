empty_df <- function() {
  data.frame(stringsAsFactors = FALSE)
}


scalar_check_bindings <- function() {
  list(
    .scholidonline_check_scalar_chr = function(x) {
      invisible(TRUE)
    },
    .package = "scholidonline"
  )
}


pmid_meta_warn_pattern <- paste0(
  "Metadata for this PMID could not be retrieved via ",
  "NCBI or Europe PMC\\."
)


pmcid_meta_warn_pattern <- paste0(
  "Metadata for this PMCID could not be retrieved via ",
  "NCBI or Europe PMC\\."
)


testthat::test_that(
  ".meta_doi() uses crossref provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_doi_crossref = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "10.1000/test")
          testthat::expect_true(quiet)
          data.frame(
            title = "Paper title",
            provider = "crossref",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_doi(
      x = "10.1000/test",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$provider, "crossref")
  }
)


testthat::test_that(
  ".meta_doi() dispatches explicit crossref provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_doi_crossref = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "10.1000/test")
          testthat::expect_false(quiet)
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_identical(
      .meta_doi(
        x = "10.1000/test",
        provider = "crossref",
        quiet = FALSE
      ),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_doi() dispatches explicit doi.org provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_doi_doi_org = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "10.1000/test")
          testthat::expect_true(quiet)
          data.frame(
            title = "Paper title",
            provider = "doi.org",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_doi(
      x = "10.1000/test",
      provider = "doi.org",
      quiet = TRUE
    )
    
    testthat::expect_identical(out$provider, "doi.org")
  }
)


testthat::test_that(
  ".meta_doi() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .meta_doi(
        x = "10.1000/test",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)


testthat::test_that(
  ".meta_pmid() auto returns NCBI result when non-empty",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    epmc_called <- FALSE
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "12345")
          testthat::expect_true(quiet)
          data.frame(
            title = "NCBI title",
            provider = "ncbi",
            stringsAsFactors = FALSE
          )
        },
        .meta_pmid_epmc = function(x, ..., quiet = FALSE) {
          epmc_called <<- TRUE
          data.frame(
            title = "EPMC title",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_pmid(
      x = "12345",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "ncbi")
    testthat::expect_false(epmc_called)
  }
)


testthat::test_that(
  ".meta_pmid() auto falls back to EPMC when NCBI is empty",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmid_ncbi = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .meta_pmid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_true(quiet)
          data.frame(
            title = "EPMC title",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_pmid(
      x = "12345",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".meta_pmid() auto falls back to EPMC when NCBI is NULL",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmid_ncbi = function(x, ..., quiet = FALSE) {
          NULL
        },
        .meta_pmid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_true(quiet)
          data.frame(
            title = "EPMC title",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_pmid(
      x = "12345",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".meta_pmid() auto warns and returns empty data.frame when both fail",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmid_ncbi = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .meta_pmid_epmc = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_warning(
      out <- .meta_pmid(
        x = "12345",
        provider = "auto",
        quiet = FALSE
      ),
      pmid_meta_warn_pattern
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_pmid() auto returns empty data.frame silently when quiet is TRUE",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmid_ncbi = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .meta_pmid_epmc = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_identical(
      .meta_pmid(
        x = "12345",
        provider = "auto",
        quiet = TRUE
      ),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_pmid() dispatches explicit ncbi provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "12345")
          testthat::expect_false(quiet)
          data.frame(
            title = "NCBI title",
            provider = "ncbi",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_pmid(
      x = "12345",
      provider = "ncbi",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "ncbi")
  }
)


testthat::test_that(
  ".meta_pmid() dispatches explicit epmc provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "12345")
          testthat::expect_true(quiet)
          data.frame(
            title = "EPMC title",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_pmid(
      x = "12345",
      provider = "epmc",
      quiet = TRUE
    )
    
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".meta_pmid() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .meta_pmid(
        x = "12345",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)


testthat::test_that(
  ".meta_pmcid() auto returns NCBI result when non-empty",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    epmc_called <- FALSE
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "PMC123")
          testthat::expect_true(quiet)
          data.frame(
            title = "NCBI title",
            provider = "ncbi",
            stringsAsFactors = FALSE
          )
        },
        .meta_pmcid_epmc = function(x, ..., quiet = FALSE) {
          epmc_called <<- TRUE
          data.frame(
            title = "EPMC title",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_pmcid(
      x = "PMC123",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "ncbi")
    testthat::expect_false(epmc_called)
  }
)


testthat::test_that(
  ".meta_pmcid() auto falls back to EPMC when NCBI is empty",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .meta_pmcid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_true(quiet)
          data.frame(
            title = "EPMC title",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_pmcid(
      x = "PMC123",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".meta_pmcid() auto falls back to EPMC when NCBI is NULL",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          NULL
        },
        .meta_pmcid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_true(quiet)
          data.frame(
            title = "EPMC title",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_pmcid(
      x = "PMC123",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".meta_pmcid() auto warns and returns empty data.frame when both fail",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .meta_pmcid_epmc = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_warning(
      out <- .meta_pmcid(
        x = "PMC123",
        provider = "auto",
        quiet = FALSE
      ),
      pmcid_meta_warn_pattern
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_pmcid() auto returns empty data.frame silently when quiet is TRUE",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .meta_pmcid_epmc = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_identical(
      .meta_pmcid(
        x = "PMC123",
        provider = "auto",
        quiet = TRUE
      ),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_pmcid() dispatches explicit ncbi provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "PMC123")
          testthat::expect_false(quiet)
          data.frame(
            title = "NCBI title",
            provider = "ncbi",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_pmcid(
      x = "PMC123",
      provider = "ncbi",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "ncbi")
  }
)


testthat::test_that(
  ".meta_pmcid() dispatches explicit epmc provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_pmcid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "PMC123")
          testthat::expect_true(quiet)
          data.frame(
            title = "EPMC title",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_pmcid(
      x = "PMC123",
      provider = "epmc",
      quiet = TRUE
    )
    
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".meta_pmcid() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .meta_pmcid(
        x = "PMC123",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)


testthat::test_that(
  ".meta_arxiv() uses arxiv provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_arxiv_arxiv = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "1234.5678")
          testthat::expect_true(quiet)
          data.frame(
            title = "Paper title",
            provider = "arxiv",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_arxiv(
      x = "1234.5678",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$provider, "arxiv")
  }
)


testthat::test_that(
  ".meta_arxiv() dispatches explicit arxiv provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_arxiv_arxiv = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "1234.5678")
          testthat::expect_false(quiet)
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_identical(
      .meta_arxiv(
        x = "1234.5678",
        provider = "arxiv",
        quiet = FALSE
      ),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_arxiv() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .meta_arxiv(
        x = "1234.5678",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)


testthat::test_that(
  ".meta_orcid() uses orcid provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_orcid_orcid = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "0000-0002-1825-0097")
          testthat::expect_true(quiet)
          data.frame(
            title = "Ada Lovelace",
            provider = "orcid",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .meta_orcid(
      x = "0000-0002-1825-0097",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$provider, "orcid")
  }
)


testthat::test_that(
  ".meta_orcid() dispatches explicit orcid provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_orcid_orcid = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "0000-0002-1825-0097")
          testthat::expect_false(quiet)
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_identical(
      .meta_orcid(
        x = "0000-0002-1825-0097",
        provider = "orcid",
        quiet = FALSE
      ),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_orcid() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .meta_orcid(
        x = "0000-0002-1825-0097",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)