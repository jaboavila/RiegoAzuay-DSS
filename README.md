# RiegoAzuay DSS v1.0.0

Bilingual evidence-aware decision-support prototype for parcel- and turn-level irrigation
planning under community turn constraints in Azuay, Ecuador.

## Release status

This archive is the public `v1.0.0` software release. It was promoted from the evaluated
scientific source state `v1.0.0-rc6` without changes to the scientific engine or the
reported computational results. The retained RC6 freeze and test records document the
source state evaluated in the manuscript; `FINAL_RELEASE_PROMOTION_RECORD.md` documents
the licence and release-metadata promotion.

No DOI is embedded in this archive at build time. Use the version-specific archival
record and repository release metadata after deposit.

## Quick start

1. Extract the ZIP into a new folder.
2. Open `RiegoAzuay-DSS.Rproj`.
3. If required, run `source("00_install_packages.R", encoding = "UTF-8")` once.
4. Run `source("04_run_app.R", encoding = "UTF-8")`.

## Reproducibility self-test

Run:

```r
source("VERIFY_PUBLIC_PACKAGE.R", encoding = "UTF-8")
```

The workflow parses the R files and executes the static preflight, scientific regression,
and integration tests. It does not rerun the closed 2304-scenario retrospective experiment
or make new live-source requests.

## Data-governance boundary

The public release uses five synthetic territorial fixtures and aggregate selector counts.
It excludes the processed 159-system selector, record-level territorial audits, exact
institutional intake coordinates, and raw institutional records. The single previously
published Santul scientific-demonstration context is retained where required by the
archived scientific modules, but the restricted selector is not distributed.

## Licence

The original RiegoAzuay DSS software is released under the MIT License:

`Copyright (c) 2026 Javier Ávila-Larrea`

See `LICENSE` and `DATA_AND_THIRD_PARTY_MATERIALS.md`. The restricted institutional
registry and third-party materials are not relicensed by this software licence.

## Citation

Use `CITATION.cff` and the version-specific repository/archival metadata. The associated
Land article should also be cited after publication.

## Confidential reviewer package

A separate controlled package contains the processed 159-system selector for confidential
editorial and peer-review use. That package must not be uploaded to a public repository or
redistributed.
