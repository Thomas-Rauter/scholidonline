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
