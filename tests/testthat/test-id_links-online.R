test_that("id_links() works online for PMID via NCBI", {
  skip_on_cran()
  skip_if_offline()
  
  out <- id_links(
    x = "31452104",
    type = "pmid",
    provider = "ncbi"
  )
  
  expect_s3_class(out, "data.frame")
  expect_true(ncol(out) == 5L)
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  expect_true(nrow(out) >= 1L)
  expect_true(all(out$input == "31452104"))
  expect_true(all(out$input_type == "pmid"))
  expect_true(all(out$provider == "ncbi"))
  expect_true("pmid" %in% out$linked_type)
  expect_true("31452104" %in% out$linked_value)
})

test_that("id_links() works online for PMCID via NCBI", {
  skip_on_cran()
  skip_if_offline()
  
  out <- id_links(
    x = "PMC6821181",
    type = "pmcid",
    provider = "ncbi"
  )
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  expect_true(nrow(out) >= 1L)
  expect_true(all(out$input == "PMC6821181"))
  expect_true(all(out$input_type == "pmcid"))
  expect_true(all(out$provider == "ncbi"))
  expect_true("pmcid" %in% out$linked_type)
  expect_true("PMC6821181" %in% out$linked_value)
})

test_that("id_links() works online for PMID via Europe PMC", {
  skip_on_cran()
  skip_if_offline()
  
  out <- id_links(
    x = "31452104",
    type = "pmid",
    provider = "epmc"
  )
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  expect_true(nrow(out) >= 1L)
  expect_true(all(out$input == "31452104"))
  expect_true(all(out$input_type == "pmid"))
  expect_true(all(out$provider == "epmc"))
  expect_true(any(out$linked_type %in% c("pmid", "pmcid", "doi")))
})

test_that("id_links() works online for PMCID via Europe PMC", {
  skip_on_cran()
  skip_if_offline()
  
  out <- id_links(
    x = "PMC6821181",
    type = "pmcid",
    provider = "epmc"
  )
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  expect_true(nrow(out) >= 1L)
  expect_true(all(out$input == "PMC6821181"))
  expect_true(all(out$input_type == "pmcid"))
  expect_true(all(out$provider == "epmc"))
  expect_true(any(out$linked_type %in% c("pmid", "pmcid", "doi")))
})

test_that("id_links() auto provider works for PMID", {
  skip_on_cran()
  skip_if_offline()
  
  out <- id_links(
    x = "31452104",
    type = "pmid",
    provider = "auto"
  )
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  expect_true(nrow(out) >= 1L)
  expect_true(all(out$input == "31452104"))
  expect_true(all(out$input_type == "pmid"))
  expect_true(all(out$provider %in% c("ncbi", "epmc")))
  expect_true(any(out$linked_type %in% c("pmid", "pmcid", "doi")))
})

test_that("id_links() auto provider works for PMCID", {
  skip_on_cran()
  skip_if_offline()
  
  out <- id_links(
    x = "PMC6821181",
    type = "pmcid",
    provider = "auto"
  )
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  expect_true(nrow(out) >= 1L)
  expect_true(all(out$input == "PMC6821181"))
  expect_true(all(out$input_type == "pmcid"))
  expect_true(all(out$provider %in% c("ncbi", "epmc")))
  expect_true(any(out$linked_type %in% c("pmid", "pmcid", "doi")))
})

test_that("id_links() vectorizes online across multiple identifiers", {
  skip_on_cran()
  skip_if_offline()
  
  out <- id_links(
    x = c("31452104", "PMC6821181"),
    provider = "auto"
  )
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  expect_true(nrow(out) >= 2L)
  expect_true(all(out$input %in% c("31452104", "PMC6821181")))
  expect_true(all(out$input_type %in% c("pmid", "pmcid")))
})

test_that("id_links() returns zero rows for clearly invalid identifiers online", {
  skip_on_cran()
  skip_if_offline()
  
  out <- id_links(
    x = c("not_a_real_id", "definitely_not_a_pmid"),
    type = "pmid",
    provider = "auto",
    quiet = TRUE
  )
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  expect_identical(nrow(out), 0L)
})

test_that("id_links() works online for ORCID and returns the expected schema", {
  skip_on_cran()
  skip_if_offline()
  
  out <- id_links(
    x = "0000-0002-1825-0097",
    type = "orcid",
    provider = "orcid",
    quiet = TRUE
  )
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  
  if (nrow(out) > 0L) {
    expect_true(all(out$input == "0000-0002-1825-0097"))
    expect_true(all(out$input_type == "orcid"))
    expect_true(all(out$provider == "orcid"))
    expect_true(all(out$linked_type == "doi"))
  }
})

test_that("id_links() works online for arXiv and returns the expected schema", {
  skip_on_cran()
  skip_if_offline()
  
  out <- id_links(
    x = "2101.00001",
    type = "arxiv",
    provider = "arxiv",
    quiet = TRUE
  )
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  
  if (nrow(out) > 0L) {
    expect_true(all(out$input == "2101.00001"))
    expect_true(all(out$input_type == "arxiv"))
    expect_true(all(out$provider == "arxiv"))
    expect_true(all(out$linked_type == "doi"))
  }
})

test_that("id_links() works online for DOI via Crossref with stable schema", {
  skip_on_cran()
  skip_if_offline()
  
  out <- id_links(
    x = "10.1038/nature12373",
    type = "doi",
    provider = "crossref",
    quiet = TRUE
  )
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  
  if (nrow(out) > 0L) {
    expect_true(all(out$input == "10.1038/nature12373"))
    expect_true(all(out$input_type == "doi"))
    expect_true(all(out$provider == "crossref"))
    expect_true(all(out$linked_type %in% c("doi", "pmid", "pmcid")))
  }
})