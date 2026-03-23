# empty_df <- function() {
#   data.frame(stringsAsFactors = FALSE)
# }
# 
# 
# mock_scalar_check <- function() {
#   testthat::local_mocked_bindings(
#     .scholidonline_check_scalar_chr = function(x) {
#       invisible(TRUE)
#     }
#   )
# }
# 
# 
# mock_httr2 <- function(
#     body = NULL,
#     status = 200L,
#     perform_error = FALSE
# ) {
#   testthat::local_mocked_bindings(
#     request = function(url) {
#       structure(list(url = url), class = "fake_req")
#     },
#     req_error = function(req, is_error) {
#       req
#     },
#     req_url_query = function(req, ...) {
#       req
#     },
#     req_perform = function(req) {
#       if (isTRUE(perform_error)) {
#         stop("boom")
#       }
#       
#       structure(list(), class = "fake_resp")
#     },
#     resp_status = function(resp) {
#       status
#     },
#     resp_body_json = function(resp, ...) {
#       body
#     },
#     .package = "httr2"
#   )
# }
# 
# 
# testthat::test_that(
#   ".exists_pmid_ncbi() returns NA when payload is NULL",
#   {
#     mock_scalar_check()
#     
#     testthat::local_mocked_bindings(
#       .scholidonline_esummary_pubmed = function(id, ..., quiet) {
#         NULL
#       }
#     )
#     
#     testthat::expect_identical(
#       .exists_pmid_ncbi("12345"),
#       NA
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".exists_pmid_ncbi() returns NA when result block is NULL",
#   {
#     mock_scalar_check()
#     
#     testthat::local_mocked_bindings(
#       .scholidonline_esummary_pubmed = function(id, ..., quiet) {
#         list(result = NULL)
#       }
#     )
#     
#     testthat::expect_identical(
#       .exists_pmid_ncbi("12345"),
#       NA
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".exists_pmid_ncbi() returns FALSE when PMID is absent from uids",
#   {
#     mock_scalar_check()
#     
#     testthat::local_mocked_bindings(
#       .scholidonline_esummary_pubmed = function(id, ..., quiet) {
#         list(
#           result = list(
#             uids = c("99999")
#           )
#         )
#       }
#     )
#     
#     testthat::expect_false(
#       .exists_pmid_ncbi("12345")
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".exists_pmid_ncbi() returns NA when record is NULL but uid is listed",
#   {
#     mock_scalar_check()
#     
#     testthat::local_mocked_bindings(
#       .scholidonline_esummary_pubmed = function(id, ..., quiet) {
#         list(
#           result = list(
#             uids = c("12345")
#           )
#         )
#       }
#     )
#     
#     testthat::expect_identical(
#       .exists_pmid_ncbi("12345"),
#       NA
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".exists_pmid_ncbi() returns FALSE on record error",
#   {
#     mock_scalar_check()
#     
#     testthat::local_mocked_bindings(
#       .scholidonline_esummary_pubmed = function(id, ..., quiet) {
#         list(
#           result = list(
#             `12345` = list(
#               error = "not found"
#             )
#           )
#         )
#       }
#     )
#     
#     testthat::expect_false(
#       .exists_pmid_ncbi("12345")
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".exists_pmid_ncbi() returns TRUE on exact uid match",
#   {
#     mock_scalar_check()
#     
#     testthat::local_mocked_bindings(
#       .scholidonline_esummary_pubmed = function(id, ..., quiet) {
#         list(
#           result = list(
#             `12345` = list(
#               uid = "12345"
#             )
#           )
#         )
#       }
#     )
#     
#     testthat::expect_true(
#       .exists_pmid_ncbi("12345")
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".exists_pmid_ncbi() returns FALSE on uid mismatch",
#   {
#     mock_scalar_check()
#     
#     testthat::local_mocked_bindings(
#       .scholidonline_esummary_pubmed = function(id, ..., quiet) {
#         list(
#           result = list(
#             `12345` = list(
#               uid = "99999"
#             )
#           )
#         )
#       }
#     )
#     
#     testthat::expect_false(
#       .exists_pmid_ncbi("12345")
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".exists_pmcid_ncbi() returns NA on missing records",
#   {
#     mock_scalar_check()
#     
#     testthat::local_mocked_bindings(
#       .scholidonline_pmc_idconv = function(ids, ..., quiet) {
#         list(records = NULL)
#       }
#     )
#     
#     testthat::expect_identical(
#       .exists_pmcid_ncbi("PMC123"),
#       NA
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".exists_pmcid_ncbi() returns FALSE on error record",
#   {
#     mock_scalar_check()
#     
#     testthat::local_mocked_bindings(
#       .scholidonline_pmc_idconv = function(ids, ..., quiet) {
#         list(
#           records = list(
#             list(
#               status = "error"
#             )
#           )
#         )
#       }
#     )
#     
#     testthat::expect_false(
#       .exists_pmcid_ncbi("PMC123")
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".exists_pmcid_ncbi() returns TRUE when pmcid is present",
#   {
#     mock_scalar_check()
#     
#     testthat::local_mocked_bindings(
#       .scholidonline_pmc_idconv = function(ids, ..., quiet) {
#         list(
#           records = list(
#             list(
#               pmcid = "PMC123"
#             )
#           )
#         )
#       }
#     )
#     
#     testthat::expect_true(
#       .exists_pmcid_ncbi("PMC123")
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".exists_pmcid_ncbi() returns FALSE when pmcid is empty",
#   {
#     mock_scalar_check()
#     
#     testthat::local_mocked_bindings(
#       .scholidonline_pmc_idconv = function(ids, ..., quiet) {
#         list(
#           records = list(
#             list(
#               pmcid = ""
#             )
#           )
#         )
#       }
#     )
#     
#     testthat::expect_false(
#       .exists_pmcid_ncbi("PMC123")
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".links_pmid_ncbi() warns and returns empty data.frame on failure",
#   {
#     mock_scalar_check()
#     mock_httr2(perform_error = TRUE)
#     
#     testthat::expect_warning(
#       out <- .links_pmid_ncbi("12345", quiet = FALSE),
#       "NCBI request failed\\."
#     )
#     
#     testthat::expect_identical(out, empty_df())
#   }
# )
# 
# 
# testthat::test_that(
#   ".links_pmid_ncbi() returns empty data.frame for non-2xx status",
#   {
#     mock_scalar_check()
#     mock_httr2(status = 500L)
#     
#     testthat::expect_warning(
#       out <- .links_pmid_ncbi("12345", quiet = FALSE),
#       "NCBI request returned HTTP 500\\."
#     )
#     
#     testthat::expect_identical(out, empty_df())
#   }
# )
# 
# 
# testthat::test_that(
#   ".links_pmid_ncbi() returns empty data.frame on NULL json",
#   {
#     mock_scalar_check()
#     mock_httr2(body = NULL)
#     
#     testthat::expect_identical(
#       .links_pmid_ncbi("12345", quiet = TRUE),
#       empty_df()
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".links_pmid_ncbi() returns linked identifiers",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         records = list(
#           list(
#             pmid = "12345",
#             pmcid = "PMC123",
#             doi = "10.1000/test"
#           )
#         )
#       )
#     )
#     
#     out <- .links_pmid_ncbi("12345", quiet = TRUE)
#     
#     testthat::expect_s3_class(out, "data.frame")
#     testthat::expect_identical(
#       out$linked_type,
#       c("pmid", "pmcid", "doi")
#     )
#     testthat::expect_identical(
#       out$linked_value,
#       c("12345", "PMC123", "10.1000/test")
#     )
#     testthat::expect_identical(
#       out$provider,
#       c("ncbi", "ncbi", "ncbi")
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".links_pmcid_ncbi() warns and returns empty data.frame on failure",
#   {
#     mock_scalar_check()
#     mock_httr2(perform_error = TRUE)
#     
#     testthat::expect_warning(
#       out <- .links_pmcid_ncbi("PMC123", quiet = FALSE),
#       "NCBI request failed\\."
#     )
#     
#     testthat::expect_identical(out, empty_df())
#   }
# )
# 
# 
# testthat::test_that(
#   ".links_pmcid_ncbi() returns empty data.frame for non-2xx status",
#   {
#     mock_scalar_check()
#     mock_httr2(status = 503L)
#     
#     testthat::expect_warning(
#       out <- .links_pmcid_ncbi("PMC123", quiet = FALSE),
#       "NCBI request returned HTTP 503\\."
#     )
#     
#     testthat::expect_identical(out, empty_df())
#   }
# )
# 
# 
# testthat::test_that(
#   ".links_pmcid_ncbi() returns linked identifiers",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         records = list(
#           list(
#             pmid = "12345",
#             pmcid = "PMC123",
#             doi = "10.1000/test"
#           )
#         )
#       )
#     )
#     
#     out <- .links_pmcid_ncbi("PMC123", quiet = TRUE)
#     
#     testthat::expect_s3_class(out, "data.frame")
#     testthat::expect_identical(
#       out$linked_type,
#       c("pmid", "pmcid", "doi")
#     )
#     testthat::expect_identical(
#       out$linked_value,
#       c("12345", "PMC123", "10.1000/test")
#     )
#     testthat::expect_identical(
#       out$provider,
#       c("ncbi", "ncbi", "ncbi")
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".meta_pmid_ncbi() warns and returns empty data.frame on failure",
#   {
#     mock_scalar_check()
#     mock_httr2(perform_error = TRUE)
#     
#     testthat::expect_warning(
#       out <- .meta_pmid_ncbi("12345", quiet = FALSE),
#       "NCBI request failed\\."
#     )
#     
#     testthat::expect_identical(out, empty_df())
#   }
# )
# 
# 
# testthat::test_that(
#   ".meta_pmid_ncbi() returns empty data.frame for non-2xx status",
#   {
#     mock_scalar_check()
#     mock_httr2(status = 500L)
#     
#     testthat::expect_warning(
#       out <- .meta_pmid_ncbi("12345", quiet = FALSE),
#       "NCBI request returned HTTP 500\\."
#     )
#     
#     testthat::expect_identical(out, empty_df())
#   }
# )
# 
# 
# testthat::test_that(
#   ".meta_pmid_ncbi() returns empty data.frame when record is missing",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         result = list()
#       )
#     )
#     
#     testthat::expect_identical(
#       .meta_pmid_ncbi("12345", quiet = TRUE),
#       empty_df()
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".meta_pmid_ncbi() returns harmonized metadata",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         result = list(
#           `12345` = list(
#             title = "Paper title",
#             pubdate = "2021 Dec",
#             source = "Journal Name",
#             elocationid = "10.1000/test"
#           )
#         )
#       )
#     )
#     
#     out <- .meta_pmid_ncbi("12345", quiet = TRUE)
#     
#     testthat::expect_s3_class(out, "data.frame")
#     testthat::expect_identical(out$title, "Paper title")
#     testthat::expect_identical(out$year, 2021L)
#     testthat::expect_identical(out$container, "Journal Name")
#     testthat::expect_identical(out$doi, "10.1000/test")
#     testthat::expect_identical(out$pmid, "12345")
#     testthat::expect_true(is.na(out$pmcid))
#     testthat::expect_identical(
#       out$url,
#       "https://pubmed.ncbi.nlm.nih.gov/12345/"
#     )
#     testthat::expect_identical(out$provider, "ncbi")
#   }
# )
# 
# 
# testthat::test_that(
#   ".meta_pmid_ncbi() sets DOI to NA when elocationid is not a DOI",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         result = list(
#           `12345` = list(
#             title = "Paper title",
#             pubdate = "2021 Dec",
#             source = "Journal Name",
#             elocationid = "e123456"
#           )
#         )
#       )
#     )
#     
#     out <- .meta_pmid_ncbi("12345", quiet = TRUE)
#     
#     testthat::expect_true(is.na(out$doi))
#   }
# )
# 
# 
# testthat::test_that(
#   ".meta_pmcid_ncbi() warns and returns empty data.frame on failure",
#   {
#     mock_scalar_check()
#     mock_httr2(perform_error = TRUE)
#     
#     testthat::expect_warning(
#       out <- .meta_pmcid_ncbi("PMC123", quiet = FALSE),
#       "NCBI request failed\\."
#     )
#     
#     testthat::expect_identical(out, empty_df())
#   }
# )
# 
# 
# testthat::test_that(
#   ".meta_pmcid_ncbi() returns empty data.frame for non-2xx status",
#   {
#     mock_scalar_check()
#     mock_httr2(status = 502L)
#     
#     testthat::expect_warning(
#       out <- .meta_pmcid_ncbi("PMC123", quiet = FALSE),
#       "NCBI request returned HTTP 502\\."
#     )
#     
#     testthat::expect_identical(out, empty_df())
#   }
# )
# 
# 
# testthat::test_that(
#   ".meta_pmcid_ncbi() returns empty data.frame when record is missing",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         result = list()
#       )
#     )
#     
#     testthat::expect_identical(
#       .meta_pmcid_ncbi("PMC123", quiet = TRUE),
#       empty_df()
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".meta_pmcid_ncbi() returns harmonized metadata",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         result = list(
#           `123` = list(
#             title = "Paper title",
#             pubdate = "2020 Jan",
#             source = "Journal Name",
#             elocationid = "10.1000/test",
#             pmid = "99999"
#           )
#         )
#       )
#     )
#     
#     out <- .meta_pmcid_ncbi("PMC123", quiet = TRUE)
#     
#     testthat::expect_s3_class(out, "data.frame")
#     testthat::expect_identical(out$title, "Paper title")
#     testthat::expect_identical(out$year, 2020L)
#     testthat::expect_identical(out$container, "Journal Name")
#     testthat::expect_identical(out$doi, "10.1000/test")
#     testthat::expect_identical(out$pmid, "99999")
#     testthat::expect_identical(out$pmcid, "PMC123")
#     testthat::expect_identical(
#       out$url,
#       "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC123/"
#     )
#     testthat::expect_identical(out$provider, "ncbi")
#   }
# )
# 
# 
# testthat::test_that(
#   ".meta_pmcid_ncbi() sets DOI to NA when elocationid is not a DOI",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         result = list(
#           `123` = list(
#             title = "Paper title",
#             pubdate = "2020 Jan",
#             source = "Journal Name",
#             elocationid = "e123456",
#             pmid = "99999"
#           )
#         )
#       )
#     )
#     
#     out <- .meta_pmcid_ncbi("PMC123", quiet = TRUE)
#     
#     testthat::expect_true(is.na(out$doi))
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_pmid_to_doi_ncbi() returns DOI from data.frame articleids",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         result = list(
#           `12345` = list(
#             articleids = data.frame(
#               idtype = c("pubmed", "doi"),
#               value = c("12345", "10.1000/test"),
#               stringsAsFactors = FALSE
#             )
#           )
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_pmid_to_doi_ncbi("12345", quiet = TRUE),
#       "10.1000/test"
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_pmid_to_doi_ncbi() returns DOI from list articleids",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         result = list(
#           `12345` = list(
#             articleids = list(
#               list(
#                 idtype = "pubmed",
#                 value = "12345"
#               ),
#               list(
#                 idtype = "doi",
#                 value = "10.1000/test"
#               )
#             )
#           )
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_pmid_to_doi_ncbi("12345", quiet = TRUE),
#       "10.1000/test"
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_pmid_to_doi_ncbi() returns NA when DOI is absent",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         result = list(
#           `12345` = list(
#             articleids = data.frame(
#               idtype = "pubmed",
#               value = "12345",
#               stringsAsFactors = FALSE
#             )
#           )
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_pmid_to_doi_ncbi("12345", quiet = TRUE),
#       NA_character_
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_pmid_to_doi_ncbi() warns and returns NA on failure",
#   {
#     mock_scalar_check()
#     mock_httr2(perform_error = TRUE)
#     
#     testthat::expect_warning(
#       out <- .convert_pmid_to_doi_ncbi("12345", quiet = FALSE),
#       "NCBI request failed\\."
#     )
#     
#     testthat::expect_identical(out, NA_character_)
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_doi_to_pmid_ncbi() returns first PMID hit",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         esearchresult = list(
#           idlist = list("12345", "99999")
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_doi_to_pmid_ncbi("10.1000/test", quiet = TRUE),
#       "12345"
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_doi_to_pmid_ncbi() returns NA on empty idlist",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         esearchresult = list(
#           idlist = list()
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_doi_to_pmid_ncbi("10.1000/test", quiet = TRUE),
#       NA_character_
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_doi_to_pmid_ncbi() warns and returns NA on HTTP error",
#   {
#     mock_scalar_check()
#     mock_httr2(status = 500L)
#     
#     testthat::expect_warning(
#       out <- .convert_doi_to_pmid_ncbi("10.1000/test", quiet = FALSE),
#       "NCBI request returned HTTP 500\\."
#     )
#     
#     testthat::expect_identical(out, NA_character_)
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_pmcid_to_pmid_ncbi() returns PMID from idconv record",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         records = list(
#           list(
#             pmid = "12345"
#           )
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_pmcid_to_pmid_ncbi("PMC123", quiet = TRUE),
#       "12345"
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_pmcid_to_pmid_ncbi() returns NA for missing PMID",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         records = list(
#           list(
#             pmid = ""
#           )
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_pmcid_to_pmid_ncbi("PMC123", quiet = TRUE),
#       NA_character_
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_pmcid_to_doi_ncbi() returns DOI from idconv record",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         records = list(
#           list(
#             doi = "10.1000/test"
#           )
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_pmcid_to_doi_ncbi("PMC123", quiet = TRUE),
#       "10.1000/test"
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_pmcid_to_doi_ncbi() returns NA for missing DOI",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         records = list(
#           list(
#             doi = ""
#           )
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_pmcid_to_doi_ncbi("PMC123", quiet = TRUE),
#       NA_character_
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_pmid_to_pmcid_ncbi() returns PMCID from idconv record",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         records = list(
#           list(
#             pmcid = "PMC123"
#           )
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_pmid_to_pmcid_ncbi("12345", quiet = TRUE),
#       "PMC123"
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_pmid_to_pmcid_ncbi() returns NA for missing PMCID",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         records = list(
#           list(
#             pmcid = ""
#           )
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_pmid_to_pmcid_ncbi("12345", quiet = TRUE),
#       NA_character_
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_doi_to_pmcid_ncbi() returns PMCID from idconv record",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         records = list(
#           list(
#             pmcid = "PMC123"
#           )
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_doi_to_pmcid_ncbi("10.1000/test", quiet = TRUE),
#       "PMC123"
#     )
#   }
# )
# 
# 
# testthat::test_that(
#   ".convert_doi_to_pmcid_ncbi() returns NA for missing PMCID",
#   {
#     mock_scalar_check()
#     mock_httr2(
#       body = list(
#         records = list(
#           list(
#             pmcid = ""
#           )
#         )
#       )
#     )
#     
#     testthat::expect_identical(
#       .convert_doi_to_pmcid_ncbi("10.1000/test", quiet = TRUE),
#       NA_character_
#     )
#   }
# )