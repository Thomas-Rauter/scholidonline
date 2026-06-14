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


ror_http_bindings <- function(
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


ror_organization_json <- function() {
  list(
    id = "https://ror.org/01an7q238",
    names = list(
      list(
        lang = "en",
        types = c("ror_display", "label"),
        value = "University of Bern"
      ),
      list(
        lang = "de",
        types = "label",
        value = "Universität Bern"
      )
    ),
    locations = list(
      list(
        geonames_details = list(
          country_code = "CH",
          country_name = "Switzerland"
        )
      )
    )
  )
}


testthat::test_that(
  ".ror_api_url() builds the ROR v2 organizations endpoint",
  {
    testthat::expect_identical(
      .ror_api_url("01an7q238"),
      "https://api.ror.org/v2/organizations/01an7q238"
    )
  }
)


testthat::test_that(
  ".ror_display_name() prefers ror_display names",
  {
    testthat::expect_identical(
      .ror_display_name(ror_organization_json()),
      "University of Bern"
    )
  }
)


testthat::test_that(
  ".ror_display_name() falls back to label names",
  {
    obj <- list(
      names = list(
        list(
          types = "label",
          value = "Label Organization"
        ),
        list(
          types = "other",
          value = "Other Name"
        )
      )
    )

    testthat::expect_identical(
      .ror_display_name(obj),
      "Label Organization"
    )
  }
)


testthat::test_that(
  ".ror_display_name() falls back to the first available name value",
  {
    obj <- list(
      names = list(
        list(
          types = "other",
          value = "First Available Name"
        )
      )
    )

    testthat::expect_identical(
      .ror_display_name(obj),
      "First Available Name"
    )
  }
)


testthat::test_that(
  ".ror_country_name() extracts the first available country name",
  {
    testthat::expect_identical(
      .ror_country_name(ror_organization_json()),
      "Switzerland"
    )
  }
)


testthat::test_that(
  ".exists_ror_ror() returns TRUE on 2xx status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ror_http_bindings(status = 200L)
    )

    testthat::expect_true(
      .exists_ror_ror("01an7q238", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_ror_ror() returns FALSE on 404 status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ror_http_bindings(status = 404L)
    )

    testthat::expect_false(
      .exists_ror_ror("01an7q238", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_ror_ror() returns NA on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ror_http_bindings(perform_null = TRUE)
    )

    testthat::expect_warning(
      out <- .exists_ror_ror(
        "01an7q238",
        quiet = FALSE
      ),
      "ROR request failed\\."
    )

    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".meta_ror_ror() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ror_http_bindings(perform_null = TRUE)
    )

    testthat::expect_warning(
      out <- .meta_ror_ror(
        "01an7q238",
        quiet = FALSE
      ),
      "ROR request failed\\."
    )

    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_ror_ror() returns empty data.frame on 404 status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ror_http_bindings(status = 404L)
    )

    testthat::expect_identical(
      .meta_ror_ror("01an7q238", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_ror_ror() returns harmonized organization metadata",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ror_http_bindings(
        status = 200L,
        json = ror_organization_json()
      )
    )

    out <- .meta_ror_ror("01an7q238", quiet = TRUE)

    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$title, "University of Bern")
    testthat::expect_true(is.na(out$year))
    testthat::expect_identical(out$container, "Switzerland")
    testthat::expect_true(is.na(out$doi))
    testthat::expect_true(is.na(out$pmid))
    testthat::expect_true(is.na(out$pmcid))
    testthat::expect_identical(
      out$url,
      "https://ror.org/01an7q238"
    )
    testthat::expect_identical(out$provider, "ror")
  }
)
