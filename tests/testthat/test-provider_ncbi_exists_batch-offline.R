testthat::test_that(
  ".exists_pmid_ncbi_batch() rejects non-character input",
  {
    testthat::expect_error(
      .exists_pmid_ncbi_batch(1),
      "`x` must be a character vector.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".exists_pmid_ncbi_batch() returns NA for no valid inputs",
  {
    out <- .exists_pmid_ncbi_batch(
      c(NA_character_, ""),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA, NA)
    )
  }
)

testthat::test_that(
  ".exists_pmid_ncbi_batch() returns NA when NCBI fails",
  {
    testthat::local_mocked_bindings(
      .scholidonline_esummary_pubmed = function(id, ..., quiet = FALSE) {
        NULL
      }
    )
    
    out <- .exists_pmid_ncbi_batch(
      c("31452104", "999999999", NA_character_),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA, NA, NA)
    )
  }
)

testthat::test_that(
  ".exists_pmid_ncbi_batch() parses valid, invalid, and error records",
  {
    testthat::local_mocked_bindings(
      .scholidonline_esummary_pubmed = function(id, ..., quiet = FALSE) {
        testthat::expect_identical(
          id,
          "31452104,999999999,123"
        )
        
        list(
          result = list(
            uids = list("31452104", "123"),
            `31452104` = list(uid = "31452104"),
            `123` = list(error = "cannot get document summary")
          )
        )
      }
    )
    
    out <- .exists_pmid_ncbi_batch(
      c("31452104", "999999999", "123", NA_character_),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(TRUE, FALSE, FALSE, NA)
    )
  }
)

testthat::test_that(
  ".exists_pmid_ncbi_batch() returns FALSE for UID mismatch",
  {
    testthat::local_mocked_bindings(
      .scholidonline_esummary_pubmed = function(id, ..., quiet = FALSE) {
        list(
          result = list(
            uids = list("31452104"),
            `31452104` = list(uid = "different")
          )
        )
      }
    )
    
    out <- .exists_pmid_ncbi_batch(
      "31452104",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      FALSE
    )
  }
)

testthat::test_that(
  ".exists_pmid_ncbi_batch() keeps NA for missing record without uids",
  {
    testthat::local_mocked_bindings(
      .scholidonline_esummary_pubmed = function(id, ..., quiet = FALSE) {
        list(
          result = list()
        )
      }
    )
    
    out <- .exists_pmid_ncbi_batch(
      "31452104",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      NA
    )
  }
)

testthat::test_that(
  ".exists_pmcid_ncbi_batch() rejects non-character input",
  {
    testthat::expect_error(
      .exists_pmcid_ncbi_batch(1),
      "`x` must be a character vector.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".exists_pmcid_ncbi_batch() returns NA for no valid inputs",
  {
    out <- .exists_pmcid_ncbi_batch(
      c(NA_character_, ""),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA, NA)
    )
  }
)

testthat::test_that(
  ".exists_pmcid_ncbi_batch() returns NA when NCBI fails",
  {
    testthat::local_mocked_bindings(
      .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
        NULL
      }
    )
    
    out <- .exists_pmcid_ncbi_batch(
      c("PMC6784763", "PMC999999999", NA_character_),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA, NA, NA)
    )
  }
)

testthat::test_that(
  ".exists_pmcid_ncbi_batch() parses valid and error records",
  {
    testthat::local_mocked_bindings(
      .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
        testthat::expect_identical(
          ids,
          "PMC6784763,PMC999999999"
        )
        
        list(
          records = list(
            list(
              requested_id = "PMC6784763",
              pmcid = "PMC6784763"
            ),
            list(
              requested_id = "PMC999999999",
              status = "error"
            )
          )
        )
      }
    )
    
    out <- .exists_pmcid_ncbi_batch(
      c("PMC6784763", "PMC999999999", NA_character_),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(TRUE, FALSE, NA)
    )
  }
)

testthat::test_that(
  ".exists_pmcid_ncbi_batch() uses pmcid as record key fallback",
  {
    testthat::local_mocked_bindings(
      .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
        list(
          records = list(
            list(
              pmcid = "PMC6784763"
            )
          )
        )
      }
    )
    
    out <- .exists_pmcid_ncbi_batch(
      "PMC6784763",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      TRUE
    )
  }
)

testthat::test_that(
  ".exists_pmcid_ncbi_batch() leaves unmatched records as NA",
  {
    testthat::local_mocked_bindings(
      .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
        list(
          records = list(
            list(
              requested_id = "PMC0000001",
              pmcid = "PMC0000001"
            )
          )
        )
      }
    )
    
    out <- .exists_pmcid_ncbi_batch(
      "PMC6784763",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      NA
    )
  }
)

testthat::test_that(
  ".exists_pmcid_ncbi_batch() returns FALSE for matched empty record",
  {
    testthat::local_mocked_bindings(
      .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
        list(
          records = list(
            list(
              requested_id = "PMC6784763"
            )
          )
        )
      }
    )
    
    out <- .exists_pmcid_ncbi_batch(
      "PMC6784763",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      FALSE
    )
  }
)