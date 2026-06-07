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


accession_esummary_bindings <- function(result = NULL) {
  calls <- new.env(parent = emptyenv())
  calls$db <- NULL
  calls$id <- NULL

  list(
    bindings = list(
      .scholidonline_esummary_entrez = function(
          db,
          id,
          ...,
          quiet = FALSE
      ) {
        calls$db <- db
        calls$id <- id
        result
      },
      .package = "scholidonline"
    ),
    calls = calls
  )
}


bioproject_record_json <- function() {
  list(
    result = list(
      uids = "PRJNA257197",
      PRJNA257197 = list(
        uid = "PRJNA257197",
        accession = "PRJNA257197",
        project_title = "Example BioProject",
        organism = list(organismname = "Homo sapiens"),
        registrationdate = "2014/05/01"
      )
    )
  )
}


refseq_nuccore_record_json <- function() {
  list(
    result = list(
      uids = "NM_000546.6",
      `NM_000546.6` = list(
        uid = "NM_000546.6",
        accession = "NM_000546.6",
        title = "Example transcript",
        organism = "Homo sapiens",
        createdate = "2020/08/10"
      )
    )
  )
}


refseq_protein_record_json <- function() {
  list(
    result = list(
      uids = "NP_000537.3",
      `NP_000537.3` = list(
        uid = "NP_000537.3",
        accession = "NP_000537.3",
        title = "Example protein",
        organism = "Homo sapiens",
        createdate = "2019/01/15"
      )
    )
  )
}


sra_record_json <- function() {
  list(
    result = list(
      uids = "SRR1234567",
      SRR1234567 = list(
        uid = "SRR1234567",
        accession = "SRR1234567",
        title = "Example run",
        organism = "Mus musculus",
        createdate = "2021/11/20"
      )
    )
  )
}


assembly_record_json <- function() {
  list(
    result = list(
      uids = "GCF_000001405.40",
      `GCF_000001405.40` = list(
        uid = "GCF_000001405.40",
        accession = "GCF_000001405.40",
        assemblyname = "GRCh38.p14",
        organism = "Homo sapiens",
        submissiondate = "2022/02/03"
      )
    )
  )
}


testthat::test_that(
  "Entrez routing maps accession families to expected databases",
  {
    testthat::expect_identical(
      .ncbi_bioproject_entrez_db("PRJNA257197"),
      "bioproject"
    )
    testthat::expect_identical(
      .ncbi_refseq_entrez_db("NM_000546.6"),
      "nuccore"
    )
    testthat::expect_identical(
      .ncbi_refseq_entrez_db("NP_000537.3"),
      "protein"
    )
    testthat::expect_identical(
      .ncbi_sra_entrez_db("SRR1234567"),
      "sra"
    )
    testthat::expect_identical(
      .ncbi_assembly_entrez_db("GCF_000001405.40"),
      "assembly"
    )
  }
)


testthat::test_that(
  ".exists_bioproject_ncbi() returns TRUE for resolved records",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      accession_esummary_bindings(result = bioproject_record_json())$bindings
    )

    testthat::expect_true(
      .exists_bioproject_ncbi("PRJNA257197", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_refseq_ncbi() returns FALSE for empty uids",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      accession_esummary_bindings(
        result = list(result = list(uids = character()))
      )$bindings
    )

    testthat::expect_false(
      .exists_refseq_ncbi("NM_000546.6", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_sra_ncbi() returns NA for unsupported accessions",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_identical(
      .exists_sra_ncbi("NOTSRA123", quiet = TRUE),
      NA
    )
  }
)


testthat::test_that(
  ".meta_bioproject_ncbi() returns harmonized metadata",
  {
    mock <- accession_esummary_bindings(result = bioproject_record_json())

    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      mock$bindings
    )

    out <- .meta_bioproject_ncbi("PRJNA257197", quiet = TRUE)

    testthat::expect_identical(mock$calls$db, "bioproject")
    testthat::expect_identical(out$title, "Example BioProject")
    testthat::expect_identical(out$year, 2014L)
    testthat::expect_identical(out$container, "Homo sapiens")
    testthat::expect_identical(
      out$url,
      "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA257197"
    )
    testthat::expect_identical(out$provider, "ncbi")
  }
)


testthat::test_that(
  ".meta_refseq_ncbi() routes nucleotide accessions to nuccore URLs",
  {
    mock <- accession_esummary_bindings(result = refseq_nuccore_record_json())

    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      mock$bindings
    )

    out <- .meta_refseq_ncbi("NM_000546.6", quiet = TRUE)

    testthat::expect_identical(mock$calls$db, "nuccore")
    testthat::expect_identical(out$title, "Example transcript")
    testthat::expect_identical(out$year, 2020L)
    testthat::expect_identical(
      out$url,
      "https://www.ncbi.nlm.nih.gov/nuccore/NM_000546.6"
    )
  }
)


testthat::test_that(
  ".meta_refseq_ncbi() routes protein accessions to protein URLs",
  {
    mock <- accession_esummary_bindings(result = refseq_protein_record_json())

    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      mock$bindings
    )

    out <- .meta_refseq_ncbi("NP_000537.3", quiet = TRUE)

    testthat::expect_identical(mock$calls$db, "protein")
    testthat::expect_identical(
      out$url,
      "https://www.ncbi.nlm.nih.gov/protein/NP_000537.3"
    )
  }
)


testthat::test_that(
  ".meta_sra_ncbi() returns harmonized metadata",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      accession_esummary_bindings(result = sra_record_json())$bindings
    )

    out <- .meta_sra_ncbi("SRR1234567", quiet = TRUE)

    testthat::expect_identical(out$title, "Example run")
    testthat::expect_identical(out$year, 2021L)
    testthat::expect_identical(out$container, "Mus musculus")
    testthat::expect_identical(
      out$url,
      "https://www.ncbi.nlm.nih.gov/sra/SRR1234567"
    )
  }
)


testthat::test_that(
  ".meta_assembly_ncbi() returns harmonized metadata",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      accession_esummary_bindings(result = assembly_record_json())$bindings
    )

    out <- .meta_assembly_ncbi("GCF_000001405.40", quiet = TRUE)

    testthat::expect_identical(out$title, "GRCh38.p14")
    testthat::expect_identical(out$year, 2022L)
    testthat::expect_identical(out$container, "Homo sapiens")
    testthat::expect_identical(
      out$url,
      "https://www.ncbi.nlm.nih.gov/assembly/GCF_000001405.40"
    )
  }
)


testthat::test_that(
  "typed accession meta providers return empty data.frame for unsupported ids",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_identical(
      .meta_assembly_ncbi("NOTASSEMBLY", quiet = TRUE),
      empty_df()
    )
  }
)
