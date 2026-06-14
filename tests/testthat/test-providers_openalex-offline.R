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


openalex_http_bindings <- function(
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


openalex_work_json <- function() {
  list(
    id = "https://openalex.org/W2741809807",
    doi = "https://doi.org/10.1038/nature12373",
    title = "Sample work title",
    publication_year = 2013L,
    ids = list(
      doi = "https://doi.org/10.1038/nature12373",
      pmid = "https://pubmed.ncbi.nlm.nih.gov/24136969",
      pmcid = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3812345/",
      openalex = "https://openalex.org/W2741809807"
    ),
    primary_location = list(
      source = list(
        display_name = "Nature"
      )
    )
  )
}


testthat::test_that(
  ".openalex_api_url() maps work and author prefixes to collection URLs",
  {
    testthat::expect_identical(
      .openalex_api_url("W2741809807"),
      "https://api.openalex.org/works/W2741809807"
    )
    testthat::expect_identical(
      .openalex_api_url("A5023888391"),
      "https://api.openalex.org/authors/A5023888391"
    )
  }
)


testthat::test_that(
  ".openalex_collection() maps supported OpenAlex key prefixes",
  {
    testthat::expect_identical(.openalex_collection("W2741809807"), "works")
    testthat::expect_identical(.openalex_collection("A5023888391"), "authors")
    testthat::expect_identical(.openalex_collection("S4210172580"), "sources")
    testthat::expect_identical(.openalex_collection("I118347636"), "institutions")
    testthat::expect_identical(.openalex_collection("T10001"), "topics")
    testthat::expect_identical(.openalex_collection("K123"), "keywords")
    testthat::expect_identical(.openalex_collection("P4310320990"), "publishers")
    testthat::expect_identical(.openalex_collection("F4320306079"), "funders")
    testthat::expect_identical(.openalex_collection("G1234567890"), "grants")
    testthat::expect_null(.openalex_collection("X123"))
  }
)


testthat::test_that(
  ".openalex_api_url() appends polite-pool mailto when configured",
  {
    old_mailto <- getOption("scholidonline.openalex.mailto")
    
    on.exit(
      options(scholidonline.openalex.mailto = old_mailto),
      add = TRUE
    )
    
    options(scholidonline.openalex.mailto = "user@example.org")
    
    testthat::expect_identical(
      .openalex_api_url("W2741809807"),
      "https://api.openalex.org/works/W2741809807?mailto=user%40example.org"
    )
  }
)


testthat::test_that(
  ".exists_openalex_openalex() returns TRUE on 2xx status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      openalex_http_bindings(status = 200L)
    )

    testthat::expect_true(
      .exists_openalex_openalex("W2741809807", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_openalex_openalex() returns FALSE on 404 status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      openalex_http_bindings(status = 404L)
    )

    testthat::expect_false(
      .exists_openalex_openalex("W2741809807", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_openalex_openalex() returns NA on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      openalex_http_bindings(perform_null = TRUE)
    )

    testthat::expect_warning(
      out <- .exists_openalex_openalex(
        "W2741809807",
        quiet = FALSE
      ),
      "OpenAlex request failed\\."
    )

    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".exists_openalex_openalex() returns NA on unsupported entity prefix",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_identical(
      .exists_openalex_openalex("X12345678", quiet = TRUE),
      NA
    )
  }
)


testthat::test_that(
  ".links_openalex_openalex() returns empty data.frame for non-work entities",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_identical(
      .links_openalex_openalex("A5023888391", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_openalex_openalex() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      openalex_http_bindings(perform_null = TRUE)
    )

    testthat::expect_warning(
      out <- .links_openalex_openalex(
        "W2741809807",
        quiet = FALSE
      ),
      "OpenAlex request failed\\."
    )

    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_openalex_openalex() returns DOI, PMID, and PMCID links",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      openalex_http_bindings(
        status = 200L,
        json = openalex_work_json()
      )
    )

    out <- .links_openalex_openalex("W2741809807", quiet = TRUE)

    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      out$linked_type,
      c("doi", "pmid", "pmcid")
    )
    testthat::expect_identical(
      out$linked_value,
      c("10.1038/nature12373", "24136969", "PMC3812345")
    )
    testthat::expect_identical(
      out$provider,
      rep("openalex", 3L)
    )
  }
)


testthat::test_that(
  ".meta_openalex_openalex() returns empty data.frame for non-work entities",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_identical(
      .meta_openalex_openalex("A5023888391", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_openalex_openalex() returns bibliographic metadata for works",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      openalex_http_bindings(
        status = 200L,
        json = openalex_work_json()
      )
    )

    out <- .meta_openalex_openalex("W2741809807", quiet = TRUE)

    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_equal(nrow(out), 1L)
    testthat::expect_identical(out$title, "Sample work title")
    testthat::expect_identical(out$year, 2013L)
    testthat::expect_identical(out$container, "Nature")
    testthat::expect_identical(out$doi, "10.1038/nature12373")
    testthat::expect_identical(out$pmid, "24136969")
    testthat::expect_identical(out$pmcid, "PMC3812345")
    testthat::expect_identical(
      out$url,
      "https://openalex.org/W2741809807"
    )
    testthat::expect_identical(out$provider, "openalex")
  }
)


testthat::test_that(
  ".convert_openalex_to_doi_openalex() returns normalized DOI",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      openalex_http_bindings(
        status = 200L,
        json = openalex_work_json()
      )
    )

    testthat::expect_identical(
      .convert_openalex_to_doi_openalex("W2741809807", quiet = TRUE),
      "10.1038/nature12373"
    )
  }
)


testthat::test_that(
  ".convert_openalex_to_pmid_openalex() returns normalized PMID",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      openalex_http_bindings(
        status = 200L,
        json = openalex_work_json()
      )
    )

    testthat::expect_identical(
      .convert_openalex_to_pmid_openalex("W2741809807", quiet = TRUE),
      "24136969"
    )
  }
)


testthat::test_that(
  ".convert_openalex_to_doi_openalex() returns NA for non-work entities",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_identical(
      .convert_openalex_to_doi_openalex("A5023888391", quiet = TRUE),
      NA_character_
    )
  }
)
