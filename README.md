# envharmonizer releases

Versioned Environmental Data Harmonization: checked source packages and offline
data artifacts for AMOS and ECOTOX. Source development history remains private.

**Experimental mappings; not independently reviewed. Users must assess
suitability for their analysis.**

Install the experimental package:

```r
install.packages(
  'https://github.com/seanthimons/envharmonizer-releases/releases/download/v0.1.0/envharmonizer_0.1.0.tar.gz',
  repos = NULL,
  type = 'source'
)
```

Use an explicit matching ECOTOX release. Mapping is optional and does not change
the user's database:

```r
release <- 'ecotox_ascii_06_11_2026.zip'
envharmonizer::lifestage_dictionary(release)
envharmonizer::lifestage_review(release)
envharmonizer::harmonize_lifestage(data.frame(org_lifestage = 'Adult'), release)
```

Each release includes a source package, dictionary/seed/review CSV and Parquet
files, evidence archive, manifest, review status, attribution, and SHA256SUMS.
CSV column types and missing-value rules are defined in the manifest. Preserve
the manifest with each analysis. Existing versioned release files are immutable.

Source licences remain applicable; new mapping contributions are CC BY 4.0.
See the release attribution file for each source and its terms. Curation and
software parity checks do not constitute independent scientific review.

Cite: Thimons, S. (2026). envharmonizer 0.1.0, ECOTOX artifact ecotox-v0.1.0.
Also cite the source ontologies listed in the attribution file.
