# envharmonizer

Offline access to versioned tables that harmonize environmental method and
ecotoxicology metadata: AMOS analytical methods mapped to the Environment
Ontology (ENVO) and the Chemical Methods Ontology (CHMO), and explicit,
release-matched ECOTOX lifestage mappings.

The package makes no network requests. The released tables are embedded in the
installed package, and `release_metadata()` reports which data version, schema
version and source snapshots you are using.

**Experimental mappings; not independently reviewed. Users must assess
suitability for their analysis.**

## Install

Source tarballs are attached to each `v<version>` release. Pick a version from
the [releases page](https://github.com/seanthimons/envharmonizer/releases):

```r
install.packages(
  "https://github.com/seanthimons/envharmonizer/releases/download/v0.1.1/envharmonizer_0.1.1.tar.gz",
  repos = NULL,
  type = "source"
)
```

Requires R 4.1 or later and the `dplyr`, `jsonlite` and `tibble` packages. No
system libraries are needed.

Releases named `tables-v<version>` hold the data only (CSV, Parquet and R
bundles with checksums). They are not installable packages.

## Walk the matrix hierarchy

```r
library(envharmonizer)

matrix_roots()                          # "analytical matrix"
matrix_children("analytical matrix")    # its direct children
matrix_descendants("analytical matrix") # everything below it

harmonize_matrix(c("surface water", "sediment; soil"))
```

The analytical hierarchy has the same shape: `analytical_roots()`,
`analytical_children()`, `analytical_descendants()`.

## ECOTOX lifestage

Use an explicit matching ECOTOX release. Mapping is optional and does not change
your database:

```r
release <- "ecotox_ascii_06_11_2026.zip"
lifestage_dictionary(release)
lifestage_review(release)
harmonize_lifestage(data.frame(org_lifestage = "Adult"), release)
```

## Data provenance and licences

The tables are built from public sources in a separate private build
repository and published here as checksummed releases. A sync workflow copies
a verified release into `inst/extdata/release/` through a pull request. Source
licences remain applicable; see `ATTRIBUTION.md` and `DATA_LICENSE.md` (also
installed with the package). New mapping contributions are CC BY 4.0. Curation
and software parity checks do not constitute independent scientific review.

Releases are immutable. A correction is a new version.

Cite: Thimons, S. envharmonizer, with the release version from
`release_metadata()`. Also cite the source ontologies listed in the attribution
file.
