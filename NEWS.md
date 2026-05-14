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

