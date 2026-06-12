scholidonline 0.2.0
===========

* Added OpenAlex support for existence checks, metadata, linked identifiers,
  and conversion to DOI and PMID for work records.
* Added ROR support for organization existence checks and metadata.
* Added NCBI accession support for GEO, BioProject, RefSeq, SRA, and genome
  assembly existence checks and metadata via Entrez ESummary.
* Added UniProt support for protein accession existence checks and metadata.
* Introduced shared NCBI accession helpers for generic Entrez ESummary
  querying, record resolution, and harmonized metadata frames.
* Refactored internal provider infrastructure: shared HTTP helpers, unified
  rate limiters, registry reader helpers, shared `id_*()` input preparation,
  arXiv scalar-to-batch delegation, and consolidated engine provider
  resolution.

scholidonline 0.1.1
===========

* Added provider-level batching for selected arXiv and NCBI operations to reduce unnecessary live-service requests while preserving existing public return shapes.
* Added batched NCBI support for PMID, PMCID, and DOI metadata, linked-identifier lookup, existence checks, and supported identifier conversions.
* Moved batch execution into the unary and binary dispatch engines so batching is handled consistently through internal dispatchers rather than exported API functions.
* Added package-managed throttling for arXiv, NCBI, and Europe PMC requests, with user-configurable rate-limit options.
* Added a provider-etiquette vignette documenting batching, throttling, live-service behavior, and relevant user options.

scholidonline 0.1.0
===========
Initial release.

