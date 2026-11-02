# Live public-source audit protocol — v1.0.0-rc6

## Purpose

This release gate checks the exact external routes configured for RiegoAzuay DSS before the environment is frozen. It is independent of the retrospective experiment and does not establish parcel-scale accuracy.

## Routes under audit

1. **Operational weather**: ECMWF deterministic and 0.25° ensemble forecasts accessed through Open-Meteo, with raw JSON, request URL, retrieval time, HTTP status, response metadata and SHA-256 receipts.
2. **Public soil reference**: SoilGrids WCS as the primary route and WebDAV/VRT as fallback, with per-layer request attempts, selected URLs/coverage identifiers, extracted values, profile validity and file hashes.

The configured weather gate requires at least 40 complete ensemble members over an eight-day decision horizon. The configured soil gate requires all 36 field-capacity and wilting-point layers used to build low, central and high storage profiles.


## Ensemble completeness contract

The source may return an incomplete member during an operational update. Such a member is archived in the all-member coverage table and excluded individually. The release gate passes only when at least 40 retained members each contain exactly one valid daily row for every date in the eight-day decision horizon. Excluded member IDs, retained coverage, all-member coverage, and per-member horizon diagnostics are preserved separately.

## Execution

Open the project and run:

```r
source("AUDITAR_FUENTES_EN_VIVO.R")
```

The script writes a versioned audit directory and ZIP under `outputs/live_source_audit/`. It packages evidence even when a route fails, so the returned bundle can be diagnosed without repeating the entire application workflow.

## Interpretation boundary

A PASS means that the configured source was reachable during that run, returned the required variables/layers, passed structural and physical validity checks, and was archived with provenance receipts. It does not mean that forecast or SoilGrids values are validated against measurements at the parcel.
