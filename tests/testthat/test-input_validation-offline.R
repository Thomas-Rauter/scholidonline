testthat::test_that(".scholidonline_check_x validates main input classes", {
  testthat::expect_no_error(
    .scholidonline_check_x(c("a", "b"))
  )
  
  testthat::expect_error(
    .scholidonline_check_x(),
    "`x` is required"
  )
  
  testthat::expect_error(
    .scholidonline_check_x(NULL),
    "`x` must not be NULL"
  )
  
  testthat::expect_error(
    .scholidonline_check_x(data.frame(x = "a")),
    "`x` must not be a data frame"
  )
  
  testthat::expect_error(
    .scholidonline_check_x(1:3),
    "`x` must be a character vector"
  )
})


testthat::test_that(
  ".scholidonline_conversion_providers returns choices and errors",
  {
    out <- .scholidonline_conversion_providers("pmid", "doi")
    
    testthat::expect_true(is.character(out))
    testthat::expect_true(all(c("auto", "ncbi", "epmc") %in% out))
    
    testthat::expect_error(
      .scholidonline_conversion_providers("foo", "bar"),
      "Unsupported conversion: foo -> bar"
    )
  }
)


testthat::test_that(
  ".scholidonline_check_provider validates provider and choices",
  {
    testthat::expect_no_error(
      .scholidonline_check_provider(
        provider = "ncbi",
        choices = c("auto", "ncbi", "epmc")
      )
    )
    
    testthat::expect_error(
      .scholidonline_check_provider(
        provider = NA_character_,
        choices = c("auto", "ncbi", "epmc")
      ),
      "must be a single, non-empty character string"
    )
    
    testthat::expect_error(
      .scholidonline_check_provider(
        provider = "ncbi",
        choices = character()
      ),
      "`choices` must be a non-empty character vector"
    )
    
    testthat::expect_error(
      .scholidonline_check_provider(
        provider = "bogus",
        choices = c("auto", "ncbi", "epmc")
      ),
      "`provider` must be one of"
    )
  }
)


testthat::test_that(".scholidonline_check_quiet validates scalar logical", {
  testthat::expect_no_error(
    .scholidonline_check_quiet(TRUE)
  )
  
  testthat::expect_error(
    .scholidonline_check_quiet(NA),
    "`quiet` must be a single TRUE or FALSE value"
  )
  
  testthat::expect_error(
    .scholidonline_check_quiet(c(TRUE, FALSE)),
    "`quiet` must be a single TRUE or FALSE value"
  )
  
  testthat::expect_error(
    .scholidonline_check_quiet("TRUE"),
    "`quiet` must be a single TRUE or FALSE value"
  )
})


testthat::test_that(
  ".scholidonline_check_type_provider accepts valid combinations",
  {
    testthat::expect_no_error(
      .scholidonline_check_type_provider("auto", "ncbi")
    )
    
    testthat::expect_no_error(
      .scholidonline_check_type_provider("pmid", "ncbi")
    )
  }
)


testthat::test_that(
  ".scholidonline_check_type_provider errors on invalid combination",
  {
    testthat::expect_error(
      .scholidonline_check_type_provider("pmid", "totallybogus"),
      "is not supported for type 'pmid'"
    )
  }
)


testthat::test_that(
  ".scholidonline_providers returns sorted unique non-auto providers",
  {
    out <- .scholidonline_providers()
    
    testthat::expect_true(is.character(out))
    testthat::expect_false("auto" %in% out)
    testthat::expect_equal(out, sort(unique(out)))
    testthat::expect_true(all(c("ncbi", "epmc") %in% out))
  }
)


testthat::test_that(
  ".scholidonline_check_conversion_pair handles identity and errors",
  {
    testthat::expect_true(
      isTRUE(.scholidonline_check_conversion_pair("pmid", "pmid"))
    )
    
    testthat::expect_true(
      isTRUE(.scholidonline_check_conversion_pair("pmid", "doi"))
    )
    
    testthat::expect_error(
      .scholidonline_check_conversion_pair("pmid", "foobar"),
      "Unsupported conversion: pmid -> foobar"
    )
  }
)


testthat::test_that(".scholidonline_check_scalar_chr validates scalar text", {
  testthat::expect_identical(
    .scholidonline_check_scalar_chr("abc"),
    invisible("abc")
  )
  
  testthat::expect_error(
    .scholidonline_check_scalar_chr(c("a", "b")),
    "must be a single, non-missing character string"
  )
  
  testthat::expect_error(
    .scholidonline_check_scalar_chr(NA_character_),
    "must be a single, non-missing character string"
  )
  
  testthat::expect_error(
    .scholidonline_check_scalar_chr(1),
    "must be a single, non-missing character string"
  )
})


testthat::test_that(
  paste(
    ".scholidonline_detect_types falls back to classify",
    "for non-online detections"
  ),
  {
    testthat::local_mocked_bindings(
      detect_scholid_type = function(x) {
        c("issn", "doi", "isbn")
      },
      classify_scholid = function(x) {
        c("pmid", "isbn")
      },
      .package = "scholid"
    )

    out <- .scholidonline_detect_types(
      x = c("29456894", "10.1000/182", "9780306406157")
    )

    testthat::expect_identical(
      out,
      c("pmid", "doi", NA_character_)
    )
  }
)


testthat::test_that(
  paste(
    ".scholidonline_prepare_inputs keeps PMID when detect",
    "returns a non-online type"
  ),
  {
    testthat::local_mocked_bindings(
      detect_scholid_type = function(x) {
        rep("issn", length(x))
      },
      classify_scholid = function(x) {
        rep("pmid", length(x))
      },
      normalize_scholid = function(x, type) {
        x
      },
      .package = "scholid"
    )

    prepared <- .scholidonline_prepare_inputs(
      x = c("29456894", "17170141"),
      type = "auto"
    )

    testthat::expect_identical(
      prepared$type_vec,
      c("pmid", "pmid")
    )
    testthat::expect_identical(
      prepared$x_norm,
      c("29456894", "17170141")
    )
    testthat::expect_identical(prepared$ok, c(TRUE, TRUE))
  }
)


testthat::test_that(
  paste(
    ".scholidonline_prepare_inputs auto-convert keeps PMID",
    "when detect returns issn"
  ),
  {
    testthat::local_mocked_bindings(
      detect_scholid_type = function(x) {
        "issn"
      },
      classify_scholid = function(x) {
        "pmid"
      },
      normalize_scholid = function(x, type) {
        x
      },
      .package = "scholid"
    )

    prepared <- .scholidonline_prepare_inputs(
      x = "29456894",
      type = "auto",
      to = "doi"
    )

    testthat::expect_identical(prepared$type_vec, "pmid")
    testthat::expect_identical(prepared$x_norm, "29456894")
    testthat::expect_true(prepared$ok)
  }
)
