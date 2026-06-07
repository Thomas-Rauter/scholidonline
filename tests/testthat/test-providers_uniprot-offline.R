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


uniprot_http_bindings <- function(
    resp = structure(list(), class = "fake_resp"),
    status = 200L,
    json = NULL,
    perform_null = FALSE
) {
  list(
    .scholidonline_request = function(url) {
      structure(list(url = url), class = "fake_req")
    },
    .scholidonline_req_error = function(req, is_error) {
      req
    },
    .scholidonline_req_perform_safe = function(req) {
      if (isTRUE(perform_null)) {
        return(NULL)
      }
      resp
    },
    .scholidonline_resp_status = function(resp) {
      status
    },
    .scholidonline_resp_body_json = function(resp, ...) {
      json
    },
    .package = "scholidonline"
  )
}


uniprot_entry_json <- function() {
  list(
    primaryAccession = "P04637",
    uniProtkbId = "P53_HUMAN",
    entryAudit = list(
      firstPublicDate = "1987-08-13"
    ),
    organism = list(
      scientificName = "Homo sapiens"
    ),
    proteinDescription = list(
      recommendedName = list(
        fullName = list(
          value = "Cellular tumor antigen p53"
        )
      )
    )
  )
}


testthat::test_that(
  ".uniprot_api_url() builds the UniProt REST endpoint",
  {
    testthat::expect_identical(
      .uniprot_api_url("P04637"),
      "https://rest.uniprot.org/uniprotkb/P04637?format=json"
    )
  }
)


testthat::test_that(
  ".exists_uniprot_uniprot() returns TRUE on 2xx status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      uniprot_http_bindings(status = 200L)
    )

    testthat::expect_true(
      .exists_uniprot_uniprot("P04637", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_uniprot_uniprot() returns FALSE on 404 status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      uniprot_http_bindings(status = 404L)
    )

    testthat::expect_false(
      .exists_uniprot_uniprot("P04637", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_uniprot_uniprot() returns NA on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      uniprot_http_bindings(perform_null = TRUE)
    )

    testthat::expect_warning(
      out <- .exists_uniprot_uniprot(
        "P04637",
        quiet = FALSE
      ),
      "UniProt request failed\\."
    )

    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".meta_uniprot_uniprot() returns empty data.frame on 404 status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      uniprot_http_bindings(status = 404L)
    )

    testthat::expect_identical(
      .meta_uniprot_uniprot("P04637", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_uniprot_uniprot() returns harmonized protein metadata",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      uniprot_http_bindings(
        status = 200L,
        json = uniprot_entry_json()
      )
    )

    out <- .meta_uniprot_uniprot("P04637", quiet = TRUE)

    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_equal(nrow(out), 1L)
    testthat::expect_identical(out$title, "Cellular tumor antigen p53")
    testthat::expect_identical(out$year, 1987L)
    testthat::expect_identical(out$container, "Homo sapiens")
    testthat::expect_true(is.na(out$doi))
    testthat::expect_true(is.na(out$pmid))
    testthat::expect_true(is.na(out$pmcid))
    testthat::expect_identical(
      out$url,
      "https://www.uniprot.org/uniprotkb/P04637"
    )
    testthat::expect_identical(out$provider, "uniprot")
  }
)


testthat::test_that(
  ".uniprot_protein_name() falls back to alternative names",
  {
    obj <- list(
      proteinDescription = list(
        alternativeNames = list(
          list(
            fullName = list(
              value = "Fallback protein name"
            )
          )
        )
      )
    )

    testthat::expect_identical(
      .uniprot_protein_name(obj),
      "Fallback protein name"
    )
  }
)
