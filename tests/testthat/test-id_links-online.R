testthat::test_that("id_links() works online for PMID via NCBI", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_links(
    x = "31452104",
    type = "pmid",
    provider = "ncbi"
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(ncol(out) == 5L)
  testthat::expect_identical(
    names(out),
    c(
      "query",
      "query_type",
      "linked_type",
      "linked_id",
      "provider"
    )
  )
  
  if (nrow(out) > 0L) {
    testthat::expect_true(all(out$query == "31452104"))
    testthat::expect_true(all(out$query_type == "pmid"))
    testthat::expect_true(all(out$provider == "ncbi"))
    testthat::expect_false(any(
      out$linked_type == "pmid" &
        out$linked_id == "31452104"
    ))
    testthat::expect_true(any(out$linked_type %in% c("pmcid", "doi")))
  }
})


testthat::test_that("id_links() works online for PMCID via NCBI", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_links(
    x = "PMC6821181",
    type = "pmcid",
    provider = "ncbi"
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_identical(
    names(out),
    c(
      "query",
      "query_type",
      "linked_type",
      "linked_id",
      "provider"
    )
  )
  
  if (nrow(out) > 0L) {
    testthat::expect_true(all(out$query == "PMC6821181"))
    testthat::expect_true(all(out$query_type == "pmcid"))
    testthat::expect_true(all(out$provider == "ncbi"))
    testthat::expect_false(any(
      out$linked_type == "pmcid" &
        out$linked_id == "PMC6821181"
    ))
    testthat::expect_true(any(out$linked_type %in% c("pmid", "doi")))
  }
})


testthat::test_that("id_links() works online for PMID via Europe PMC", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_links(
    x = "31452104",
    type = "pmid",
    provider = "epmc"
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_identical(
    names(out),
    c(
      "query",
      "query_type",
      "linked_type",
      "linked_id",
      "provider"
    )
  )
  testthat::expect_true(nrow(out) >= 1L)
  testthat::expect_true(all(out$query == "31452104"))
  testthat::expect_true(all(out$query_type == "pmid"))
  testthat::expect_true(all(out$provider == "epmc"))
  testthat::expect_false(any(
    out$linked_type == "pmid" &
      out$linked_id == "31452104"
  ))
  testthat::expect_true(any(out$linked_type %in% c("pmcid", "doi")))
})


testthat::test_that("id_links() works online for PMCID via Europe PMC", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_links(
    x = "PMC6821181",
    type = "pmcid",
    provider = "epmc"
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_identical(
    names(out),
    c(
      "query",
      "query_type",
      "linked_type",
      "linked_id",
      "provider"
    )
  )
  testthat::expect_true(nrow(out) >= 1L)
  testthat::expect_true(all(out$query == "PMC6821181"))
  testthat::expect_true(all(out$query_type == "pmcid"))
  testthat::expect_true(all(out$provider == "epmc"))
  testthat::expect_false(any(
    out$linked_type == "pmcid" &
      out$linked_id == "PMC6821181"
  ))
  testthat::expect_true(any(out$linked_type %in% c("pmid", "doi")))
})


testthat::test_that("id_links() auto provider works for PMID", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_links(
    x = "31452104",
    type = "pmid",
    provider = "auto"
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_identical(
    names(out),
    c(
      "query",
      "query_type",
      "linked_type",
      "linked_id",
      "provider"
    )
  )
  
  if (nrow(out) > 0L) {
    testthat::expect_true(all(out$query == "31452104"))
    testthat::expect_true(all(out$query_type == "pmid"))
    testthat::expect_true(all(out$provider %in% c("ncbi", "epmc")))
    testthat::expect_false(any(
      out$linked_type == "pmid" &
        out$linked_id == "31452104"
    ))
    testthat::expect_true(any(out$linked_type %in% c("pmcid", "doi")))
  }
})


testthat::test_that("id_links() auto provider works for PMCID", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_links(
    x = "PMC6821181",
    type = "pmcid",
    provider = "auto"
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_identical(
    names(out),
    c(
      "query",
      "query_type",
      "linked_type",
      "linked_id",
      "provider"
    )
  )
  
  if (nrow(out) > 0L) {
    testthat::expect_true(all(out$query == "PMC6821181"))
    testthat::expect_true(all(out$query_type == "pmcid"))
    testthat::expect_true(all(out$provider %in% c("ncbi", "epmc")))
    testthat::expect_false(any(
      out$linked_type == "pmcid" &
        out$linked_id == "PMC6821181"
    ))
    testthat::expect_true(any(out$linked_type %in% c("pmid", "doi")))
  }
})


testthat::test_that(
  "id_links() vectorizes online across multiple identifiers",
  {
    skip_if_no_internet_for_live_tests()
    
    out <- id_links(
      x = c("31452104", "PMC6821181"),
      provider = "auto"
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      names(out),
      c(
        "query",
        "query_type",
        "linked_type",
        "linked_id",
        "provider"
      )
    )
    
    if (nrow(out) > 0L) {
      testthat::expect_true(all(
        out$query %in% c("31452104", "PMC6821181")
      ))
      testthat::expect_true(all(
        out$query_type %in% c("pmid", "pmcid")
      ))
      testthat::expect_false(any(
        out$query == "31452104" &
          out$linked_type == "pmid" &
          out$linked_id == "31452104"
      ))
      testthat::expect_false(any(
        out$query == "PMC6821181" &
          out$linked_type == "pmcid" &
          out$linked_id == "PMC6821181"
      ))
    }
  }
)


testthat::test_that(
  "id_links() returns zero rows for clearly invalid identifiers online",
  {
    skip_if_no_internet_for_live_tests()
    
    out <- id_links(
      x = c("not_a_real_id", "definitely_not_a_pmid"),
      type = "pmid",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      names(out),
      c(
        "query",
        "query_type",
        "linked_type",
        "linked_id",
        "provider"
      )
    )
    testthat::expect_identical(nrow(out), 0L)
  }
)


testthat::test_that(
  "id_links() works online for ORCID and returns the expected schema",
  {
    skip_if_no_internet_for_live_tests()
    
    out <- id_links(
      x = "0000-0002-1825-0097",
      type = "orcid",
      provider = "orcid",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      names(out),
      c(
        "query",
        "query_type",
        "linked_type",
        "linked_id",
        "provider"
      )
    )
    
    if (nrow(out) > 0L) {
      testthat::expect_true(all(out$query == "0000-0002-1825-0097"))
      testthat::expect_true(all(out$query_type == "orcid"))
      testthat::expect_true(all(out$provider == "orcid"))
      testthat::expect_true(all(out$linked_type == "doi"))
    }
  }
)


testthat::test_that(
  "id_links() works online for arXiv and returns the expected schema",
  {
    skip_if_no_internet_for_live_tests()
    
    out <- id_links(
      x = "2101.00001",
      type = "arxiv",
      provider = "arxiv",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      names(out),
      c(
        "query",
        "query_type",
        "linked_type",
        "linked_id",
        "provider"
      )
    )
    
    if (nrow(out) > 0L) {
      testthat::expect_true(all(out$query == "2101.00001"))
      testthat::expect_true(all(out$query_type == "arxiv"))
      testthat::expect_true(all(out$provider == "arxiv"))
      testthat::expect_true(all(out$linked_type == "doi"))
    }
  }
)


testthat::test_that(
  "id_links() works online for DOI via Crossref with stable schema",
  {
    skip_if_no_internet_for_live_tests()
    
    out <- id_links(
      x = "10.1038/nature12373",
      type = "doi",
      provider = "crossref",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      names(out),
      c(
        "query",
        "query_type",
        "linked_type",
        "linked_id",
        "provider"
      )
    )
    
    if (nrow(out) > 0L) {
      testthat::expect_true(all(out$query == "10.1038/nature12373"))
      testthat::expect_true(all(out$query_type == "doi"))
      testthat::expect_true(all(out$provider == "crossref"))
      testthat::expect_true(all(
        out$linked_type %in% c("doi", "pmid", "pmcid")
      ))
      testthat::expect_false(any(
        out$linked_type == "doi" &
          out$linked_id == "10.1038/nature12373"
      ))
    }
  }
)