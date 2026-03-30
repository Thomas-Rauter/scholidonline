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


pmid_links_warn_pattern <- paste0(
  "Linked identifiers for this PMID could not be determined via ",
  "NCBI or Europe PMC\\."
)


pmcid_links_warn_pattern <- paste0(
  "Linked identifiers for this PMCID could not be determined via ",
  "NCBI or Europe PMC\\."
)


testthat::test_that(
  ".links_arxiv() uses arxiv provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_arxiv_arxiv = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "1234.5678")
          testthat::expect_true(quiet)
          data.frame(
            linked_type = "doi",
            linked_value = "10.1000/test",
            provider = "arxiv",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_arxiv(
      x = "1234.5678",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$provider, "arxiv")
  }
)


testthat::test_that(
  ".links_arxiv() dispatches explicit arxiv provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_arxiv_arxiv = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "1234.5678")
          testthat::expect_false(quiet)
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_identical(
      .links_arxiv(
        x = "1234.5678",
        provider = "arxiv",
        quiet = FALSE
      ),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_arxiv() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .links_arxiv(
        x = "1234.5678",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)


testthat::test_that(
  ".links_doi() uses crossref provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_doi_crossref = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "10.1000/test")
          testthat::expect_true(quiet)
          data.frame(
            linked_type = "pmid",
            linked_value = "12345",
            provider = "crossref",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_doi(
      x = "10.1000/test",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$provider, "crossref")
  }
)


testthat::test_that(
  ".links_doi() dispatches explicit crossref provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_doi_crossref = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "10.1000/test")
          testthat::expect_false(quiet)
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_identical(
      .links_doi(
        x = "10.1000/test",
        provider = "crossref",
        quiet = FALSE
      ),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_doi() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .links_doi(
        x = "10.1000/test",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)


testthat::test_that(
  ".links_orcid() uses orcid provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_orcid_orcid = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "0000-0002-1825-0097")
          testthat::expect_true(quiet)
          data.frame(
            linked_type = "doi",
            linked_value = "10.1000/test",
            provider = "orcid",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_orcid(
      x = "0000-0002-1825-0097",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$provider, "orcid")
  }
)


testthat::test_that(
  ".links_orcid() dispatches explicit orcid provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_orcid_orcid = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "0000-0002-1825-0097")
          testthat::expect_false(quiet)
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_identical(
      .links_orcid(
        x = "0000-0002-1825-0097",
        provider = "orcid",
        quiet = FALSE
      ),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_orcid() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .links_orcid(
        x = "0000-0002-1825-0097",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)


testthat::test_that(
  ".links_pmcid() auto returns NCBI result when non-empty",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    epmc_called <- FALSE
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "PMC123")
          testthat::expect_true(quiet)
          data.frame(
            linked_type = "doi",
            linked_value = "10.1000/test",
            provider = "ncbi",
            stringsAsFactors = FALSE
          )
        },
        .links_pmcid_epmc = function(x, ..., quiet = FALSE) {
          epmc_called <<- TRUE
          data.frame(
            linked_type = "doi",
            linked_value = "10.1000/fallback",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_pmcid(
      x = "PMC123",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "ncbi")
    testthat::expect_false(epmc_called)
  }
)


testthat::test_that(
  ".links_pmcid() auto falls back to EPMC when NCBI is empty",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .links_pmcid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_true(quiet)
          data.frame(
            linked_type = "pmid",
            linked_value = "12345",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_pmcid(
      x = "PMC123",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".links_pmcid() auto falls back to EPMC when NCBI is NULL",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          NULL
        },
        .links_pmcid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_true(quiet)
          data.frame(
            linked_type = "pmid",
            linked_value = "12345",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_pmcid(
      x = "PMC123",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".links_pmcid() auto warns and returns empty data.frame when both fail",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .links_pmcid_epmc = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_warning(
      out <- .links_pmcid(
        x = "PMC123",
        provider = "auto",
        quiet = FALSE
      ),
      pmcid_links_warn_pattern
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_pmcid() auto returns empty data.frame silently when quiet is TRUE",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .links_pmcid_epmc = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_identical(
      .links_pmcid(
        x = "PMC123",
        provider = "auto",
        quiet = TRUE
      ),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_pmcid() dispatches explicit ncbi provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmcid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "PMC123")
          testthat::expect_false(quiet)
          data.frame(
            linked_type = "doi",
            linked_value = "10.1000/test",
            provider = "ncbi",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_pmcid(
      x = "PMC123",
      provider = "ncbi",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "ncbi")
  }
)


testthat::test_that(
  ".links_pmcid() dispatches explicit epmc provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmcid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "PMC123")
          testthat::expect_true(quiet)
          data.frame(
            linked_type = "doi",
            linked_value = "10.1000/test",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_pmcid(
      x = "PMC123",
      provider = "epmc",
      quiet = TRUE
    )
    
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".links_pmcid() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .links_pmcid(
        x = "PMC123",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)


testthat::test_that(
  ".links_pmid() auto returns NCBI result when non-empty",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    epmc_called <- FALSE
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "12345")
          testthat::expect_true(quiet)
          data.frame(
            linked_type = "doi",
            linked_value = "10.1000/test",
            provider = "ncbi",
            stringsAsFactors = FALSE
          )
        },
        .links_pmid_epmc = function(x, ..., quiet = FALSE) {
          epmc_called <<- TRUE
          data.frame(
            linked_type = "doi",
            linked_value = "10.1000/fallback",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_pmid(
      x = "12345",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "ncbi")
    testthat::expect_false(epmc_called)
  }
)


testthat::test_that(
  ".links_pmid() auto falls back to EPMC when NCBI is empty",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmid_ncbi = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .links_pmid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_true(quiet)
          data.frame(
            linked_type = "pmcid",
            linked_value = "PMC123",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_pmid(
      x = "12345",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".links_pmid() auto falls back to EPMC when NCBI is NULL",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmid_ncbi = function(x, ..., quiet = FALSE) {
          NULL
        },
        .links_pmid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_true(quiet)
          data.frame(
            linked_type = "pmcid",
            linked_value = "PMC123",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_pmid(
      x = "12345",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".links_pmid() auto warns and returns empty data.frame when both fail",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmid_ncbi = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .links_pmid_epmc = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_warning(
      out <- .links_pmid(
        x = "12345",
        provider = "auto",
        quiet = FALSE
      ),
      pmid_links_warn_pattern
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_pmid() auto returns empty data.frame silently when quiet is TRUE",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmid_ncbi = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .links_pmid_epmc = function(x, ..., quiet = FALSE) {
          empty_df()
        },
        .package = "scholidonline"
      )
    )
    
    testthat::expect_identical(
      .links_pmid(
        x = "12345",
        provider = "auto",
        quiet = TRUE
      ),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_pmid() dispatches explicit ncbi provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmid_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "12345")
          testthat::expect_false(quiet)
          data.frame(
            linked_type = "doi",
            linked_value = "10.1000/test",
            provider = "ncbi",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_pmid(
      x = "12345",
      provider = "ncbi",
      quiet = FALSE
    )
    
    testthat::expect_identical(out$provider, "ncbi")
  }
)


testthat::test_that(
  ".links_pmid() dispatches explicit epmc provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_pmid_epmc = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "12345")
          testthat::expect_true(quiet)
          data.frame(
            linked_type = "doi",
            linked_value = "10.1000/test",
            provider = "epmc",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )
    
    out <- .links_pmid(
      x = "12345",
      provider = "epmc",
      quiet = TRUE
    )
    
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".links_pmid() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    
    testthat::expect_error(
      .links_pmid(
        x = "12345",
        provider = "nope",
        quiet = TRUE
      ),
      "Unknown provider: nope"
    )
  }
)