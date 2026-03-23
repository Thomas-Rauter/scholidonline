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


orcid_http_bindings <- function(
    resp = structure(list(), class = "fake_resp"),
    status = 200L,
    json = NULL,
    perform_null = FALSE
) {
  list(
    .scholidonline_request = function(url) {
      structure(list(url = url), class = "fake_req")
    },
    .scholidonline_req_headers = function(req, ...) {
      req
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


testthat::test_that(
  ".exists_orcid_orcid() returns TRUE on 2xx status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(status = 200L)
    )
    
    testthat::expect_true(
      .exists_orcid_orcid("0000-0002-1825-0097", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_orcid_orcid() returns FALSE on 404 status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(status = 404L)
    )
    
    testthat::expect_false(
      .exists_orcid_orcid("0000-0002-1825-0097", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_orcid_orcid() returns NA on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .exists_orcid_orcid(
        "0000-0002-1825-0097",
        quiet = FALSE
      ),
      "ORCID request failed\\."
    )
    
    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".exists_orcid_orcid() returns NA on non-404 non-2xx status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .exists_orcid_orcid(
        "0000-0002-1825-0097",
        quiet = FALSE
      ),
      "ORCID request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".links_orcid_orcid() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .links_orcid_orcid(
        "0000-0002-1825-0097",
        quiet = FALSE
      ),
      "ORCID request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_orcid_orcid() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(status = 503L)
    )
    
    testthat::expect_warning(
      out <- .links_orcid_orcid(
        "0000-0002-1825-0097",
        quiet = FALSE
      ),
      "ORCID request returned HTTP 503\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_orcid_orcid() returns empty data.frame on NULL json",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(
        status = 200L,
        json = NULL
      )
    )
    
    testthat::expect_identical(
      .links_orcid_orcid("0000-0002-1825-0097", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_orcid_orcid() returns empty data.frame on missing groups",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(
        status = 200L,
        json = list(group = NULL)
      )
    )
    
    testthat::expect_identical(
      .links_orcid_orcid("0000-0002-1825-0097", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_orcid_orcid() returns empty data.frame with no DOI ids",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(
        status = 200L,
        json = list(
          group = list(
            list(
              `external-ids` = list(
                `external-id` = list(
                  list(
                    `external-id-type` = "pmid",
                    `external-id-value` = "12345"
                  )
                )
              )
            )
          )
        )
      )
    )
    
    testthat::expect_identical(
      .links_orcid_orcid("0000-0002-1825-0097", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_orcid_orcid() returns DOI links",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(
        status = 200L,
        json = list(
          group = list(
            list(
              `external-ids` = list(
                `external-id` = list(
                  list(
                    `external-id-type` = "doi",
                    `external-id-value` = "10.1000/a"
                  ),
                  list(
                    `external-id-type` = "doi",
                    `external-id-value` = "10.1000/b"
                  )
                )
              )
            )
          )
        )
      )
    )
    
    out <- .links_orcid_orcid("0000-0002-1825-0097", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      out$linked_type,
      c("doi", "doi")
    )
    testthat::expect_identical(
      out$linked_value,
      c("10.1000/a", "10.1000/b")
    )
    testthat::expect_identical(
      out$provider,
      c("orcid", "orcid")
    )
  }
)


testthat::test_that(
  ".links_orcid_orcid() skips malformed id entries",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(
        status = 200L,
        json = list(
          group = list(
            list(
              `external-ids` = list(
                `external-id` = list(
                  list(
                    `external-id-type` = "doi",
                    `external-id-value` = "10.1000/a"
                  ),
                  list(
                    `external-id-type` = NULL,
                    `external-id-value` = "10.1000/b"
                  ),
                  list(
                    `external-id-type` = "doi",
                    `external-id-value` = NULL
                  )
                )
              )
            )
          )
        )
      )
    )
    
    out <- .links_orcid_orcid("0000-0002-1825-0097", quiet = TRUE)
    
    testthat::expect_identical(out$linked_type, "doi")
    testthat::expect_identical(out$linked_value, "10.1000/a")
    testthat::expect_identical(out$provider, "orcid")
  }
)


testthat::test_that(
  ".meta_orcid_orcid() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .meta_orcid_orcid(
        "0000-0002-1825-0097",
        quiet = FALSE
      ),
      "ORCID request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_orcid_orcid() returns empty data.frame on 404 status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(status = 404L)
    )
    
    testthat::expect_identical(
      .meta_orcid_orcid("0000-0002-1825-0097", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_orcid_orcid() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .meta_orcid_orcid(
        "0000-0002-1825-0097",
        quiet = FALSE
      ),
      "ORCID request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_orcid_orcid() returns harmonized metadata",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(
        status = 200L,
        json = list(
          person = list(
            name = list(
              `given-names` = list(value = "Ada"),
              `family-name` = list(value = "Lovelace")
            )
          )
        )
      )
    )
    
    out <- .meta_orcid_orcid("0000-0002-1825-0097", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$title, "Ada Lovelace")
    testthat::expect_true(is.na(out$year))
    testthat::expect_true(is.na(out$container))
    testthat::expect_true(is.na(out$doi))
    testthat::expect_true(is.na(out$pmid))
    testthat::expect_true(is.na(out$pmcid))
    testthat::expect_identical(
      out$url,
      "https://orcid.org/0000-0002-1825-0097"
    )
    testthat::expect_identical(out$provider, "orcid")
  }
)


testthat::test_that(
  ".meta_orcid_orcid() trims full name correctly",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(
        status = 200L,
        json = list(
          person = list(
            name = list(
              `given-names` = list(value = "Ada"),
              `family-name` = list(value = "")
            )
          )
        )
      )
    )
    
    out <- .meta_orcid_orcid("0000-0002-1825-0097", quiet = TRUE)
    
    testthat::expect_identical(out$title, "Ada")
  }
)


testthat::test_that(
  ".meta_orcid_orcid() returns empty string when both names missing",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(
        status = 200L,
        json = list(
          person = list(
            name = list(
              `given-names` = list(value = NULL),
              `family-name` = list(value = NULL)
            )
          )
        )
      )
    )
    
    out <- .meta_orcid_orcid("0000-0002-1825-0097", quiet = TRUE)
    
    testthat::expect_identical(out$title, "")
  }
)


testthat::test_that(
  ".meta_orcid_orcid() returns NA title when name block is missing",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      orcid_http_bindings(
        status = 200L,
        json = list(person = list(name = NULL))
      )
    )
    
    out <- .meta_orcid_orcid("0000-0002-1825-0097", quiet = TRUE)
    
    testthat::expect_true(is.na(out$title))
    testthat::expect_true(is.na(out$year))
    testthat::expect_true(is.na(out$container))
    testthat::expect_true(is.na(out$doi))
    testthat::expect_true(is.na(out$pmid))
    testthat::expect_true(is.na(out$pmcid))
    testthat::expect_identical(
      out$url,
      "https://orcid.org/0000-0002-1825-0097"
    )
    testthat::expect_identical(out$provider, "orcid")
  }
)