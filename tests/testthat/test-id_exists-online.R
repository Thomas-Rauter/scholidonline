testthat::test_that(
  "id_exists (online): DOI existence works via doi.org and crossref",
  {
    skip_if_no_internet_for_live_tests()
    
    doi_valid <- "10.1038/s41586-020-2649-2"
    doi_invalid <- "10.9999/this-does-not-exist"
    
    out_doi_org_valid <- scholidonline::id_exists(
      x = doi_valid,
      type = "doi",
      provider = "doi.org",
      quiet = TRUE
    )
    
    out_crossref_valid <- scholidonline::id_exists(
      x = doi_valid,
      type = "doi",
      provider = "crossref",
      quiet = TRUE
    )
    
    out_doi_org_invalid <- scholidonline::id_exists(
      x = doi_invalid,
      type = "doi",
      provider = "doi.org",
      quiet = TRUE
    )
    
    out_crossref_invalid <- scholidonline::id_exists(
      x = doi_invalid,
      type = "doi",
      provider = "crossref",
      quiet = TRUE
    )
    
    testthat::expect_identical(out_doi_org_valid, TRUE)
    testthat::expect_identical(out_crossref_valid, TRUE)
    testthat::expect_identical(out_doi_org_invalid, FALSE)
    testthat::expect_identical(out_crossref_invalid, FALSE)
  }
)


testthat::test_that(
  "id_exists (online): DOI normalization works against live providers",
  {
    skip_if_no_internet_for_live_tests()
    
    out <- scholidonline::id_exists(
      x = "https://doi.org/10.1038/s41586-021-03819-2",
      type = "doi",
      provider = "doi.org",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, TRUE)
  }
)


testthat::test_that(
  "id_exists (online): PMID existence works via Europe PMC",
  {
    skip_if_no_internet_for_live_tests()
    
    out_valid <- scholidonline::id_exists(
      x = "31452104",
      type = "pmid",
      provider = "epmc",
      quiet = TRUE
    )
    
    out_invalid <- scholidonline::id_exists(
      x = "999999999999",
      type = "pmid",
      provider = "epmc",
      quiet = TRUE
    )
    
    testthat::expect_identical(out_valid, TRUE)
    testthat::expect_identical(out_invalid, FALSE)
  }
)


testthat::test_that(
  "id_exists (online): PMCID existence works via Europe PMC",
  {
    skip_if_no_internet_for_live_tests()
    
    out_valid <- scholidonline::id_exists(
      x = "PMC6784763",
      type = "pmcid",
      provider = "epmc",
      quiet = TRUE
    )
    
    out_invalid <- scholidonline::id_exists(
      x = "PMC0000000",
      type = "pmcid",
      provider = "epmc",
      quiet = TRUE
    )
    
    testthat::expect_identical(out_valid, TRUE)
    testthat::expect_identical(out_invalid, FALSE)
  }
)


testthat::test_that(
  "id_exists (online): auto fallback works for PMID existence",
  {
    skip_if_no_internet_for_live_tests()
    
    out <- scholidonline::id_exists(
      x = c("31452104", "999999999999"),
      type = "pmid",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_type(out, "logical")
    testthat::expect_identical(length(out), 2L)
    testthat::expect_identical(out[[1]], TRUE)
    testthat::expect_identical(out[[2]], FALSE)
  }
)


testthat::test_that(
  "id_exists (online): auto fallback works for PMCID existence",
  {
    skip_if_no_internet_for_live_tests()
    
    out <- scholidonline::id_exists(
      x = c("PMC6784763", "PMC0000000"),
      type = "pmcid",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_type(out, "logical")
    testthat::expect_identical(length(out), 2L)
    testthat::expect_identical(out[[1]], TRUE)
    testthat::expect_identical(out[[2]], FALSE)
  }
)


testthat::test_that(
  "id_exists (online): ORCID existence works",
  {
    skip_if_no_internet_for_live_tests()
    
    out_valid <- scholidonline::id_exists(
      x = "0000-0002-1825-0097",
      type = "orcid",
      provider = "orcid",
      quiet = TRUE
    )
    
    testthat::expect_identical(out_valid, TRUE)
  }
)


testthat::test_that(
  "id_exists (online): mixed type inference works against live services",
  {
    skip_if_no_internet_for_live_tests()
    
    old_rate_limit <- getOption("scholidonline.rate_limit")
    old_interval <- getOption("scholidonline.arxiv.min_interval")
    
    on.exit(
      {
        options(
          scholidonline.rate_limit = old_rate_limit,
          scholidonline.arxiv.min_interval = old_interval
        )
      },
      add = TRUE
    )
    
    options(
      scholidonline.rate_limit = TRUE,
      scholidonline.arxiv.min_interval = 3
    )
    
    .arxiv_rate_limit_reset()
    
    arxiv_txt <- .arxiv_query_id_list(
      "2203.00001",
      quiet = TRUE
    )
    
    skip_if_arxiv_live_unavailable(arxiv_txt)
    
    testthat::local_mocked_bindings(
      .arxiv_query_id_list = function(x, ..., quiet = FALSE) {
        arxiv_txt
      }
    )
    
    out <- scholidonline::id_exists(
      x = c(
        "10.1038/s41586-020-2649-2",
        "31452104",
        "PMC6784763",
        "0000-0002-1825-0097",
        "2203.00001"
      ),
      type = NULL,
      quiet = TRUE
    )
    
    testthat::expect_type(out, "logical")
    testthat::expect_identical(
      out,
      c(TRUE, TRUE, TRUE, TRUE, TRUE)
    )
  }
)


testthat::test_that("id_exists checks arXiv vectors against public API", {
  skip_if_no_internet_for_live_tests()
  
  old_rate_limit <- getOption("scholidonline.rate_limit")
  old_interval <- getOption("scholidonline.arxiv.min_interval")
  
  on.exit(
    {
      options(
        scholidonline.rate_limit = old_rate_limit,
        scholidonline.arxiv.min_interval = old_interval
      )
    },
    add = TRUE
  )
  
  options(
    scholidonline.rate_limit = TRUE,
    scholidonline.arxiv.min_interval = 3
  )
  
  .arxiv_rate_limit_reset()
  
  x <- c(
    "hep-ex/0307015",
    "0706.0001",
    "1234.12345",
    NA_character_
  )
  
  txt <- .arxiv_query_id_list(
    x,
    quiet = TRUE
  )
  
  skip_if_arxiv_live_unavailable(txt)
  
  testthat::local_mocked_bindings(
    .arxiv_query_id_list = function(x, ..., quiet = FALSE) {
      txt
    }
  )
  
  out <- id_exists(
    x,
    type = "arxiv",
    quiet = TRUE
  )
  
  testthat::expect_identical(
    out,
    c(TRUE, TRUE, FALSE, NA)
  )
})