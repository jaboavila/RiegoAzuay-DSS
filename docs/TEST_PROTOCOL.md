# Test protocol — v1.0.0-rc6

## Layer 0 — syntax gate

Expected: **31/31 R files** parsed with base R before package installation. Any parse error stops the workflow and identifies the file.

## Layer 1 — static preflight

Expected: **190 checks**. Alpha15 retains the accepted project, registry, configuration, bilingual, privacy, navigation, calendar, and packaging checks and adds the final acceptance-clarity guards:

- direct native text search by system name or public ID;
- accent-insensitive search and explicit same-name disambiguation;
- canton-level map bounds independent of parish filtering;
- representative-point and guided crop-polygon controls in the simple workflow;
- map legend, intake-to-productive-point line, and distance;
- territorial ambiguity audit for duplicate names and combined parish labels;
- simplified evidence presentation;
- registered `openxlsx` dependency and user workbook generation;
- primary Excel/HTML versus technical CSV/RDS/ZIP download boundaries;
- strict crop-specific area semantics;
- no-footprint point semantics;
- polygon area and centroid calculation;
- explicit declared-versus-polygon area selection;
- polygon invalidation after crop change;
- crop-specific area provenance in reports and exports;
- formatted Excel output through `openxlsx`;
- staged system → canton → parish synchronisation that waits for browser acknowledgement;
- Santul resolving specifically to Paute/Tomebamba before pending targets are cleared.
- explicit Step 3 evidence/source labelling;
- non-weekly turn-horizon explanation;
- turn-level E2/E3 terminology;
- removal of the raw Boolean verification column from the user Excel workbook while retaining technical provenance.
- exact six-row user source table in both languages;
- localised weather, soil, flow, turn, location, and area descriptions;
- absence of raw scope/verification/internal-code tokens from the user workbook;
- localisation of the productive-location source in the summary sheet;

## Layer 2 — scientific regression

Expected: **108 checks**. Covers FAO-56, layered root-zone balance, crop stages, stress, flow measurement, turn scheduling, delivery capacity, evidence gate, and mass-balance closure. Alpha15 does not change this layer. No retrospective rerun or public API call occurs.

## Layer 3 — integration smoke test

Expected: **84 checks** for the controlled Santul scenario. Alpha15 does not modify this layer; the accepted alpha13 runtime evidence remains applicable because the interface and scientific modules are unchanged. The checks cover:

- direct name and accent-insensitive search;
- canton map bounds;
- visible point/polygon controls;
- map legend and intake-to-productive-point communication;
- valid Spanish and English OOXML workbooks with four user sheets each;
- workbook inclusion in the reproducibility ZIP;
- user versus technical download separation;
- territorial ambiguity documentation;
- action-oriented evidence presentation;
- strict Santul name/ID matching without unrelated prior selections;
- point mode retaining declared crop area without creating a footprint;
- polygon area and centroid calculation;
- explicit polygon-area selection controlling the active area;
- proportional scaling of required volume with selected polygon area;
- crop-specific scope and area-source metadata in reports;
- styled Excel workbook structure and readability controls;
- complete administrative target resolution for the selected system;
- inclusion of the target parish in the canton-specific choices;
- staged, client-confirmed canton/parish synchronisation in the runtime source.
- exact, complete, localised source rows in Spanish and English;
- exclusion of internal scope, Boolean, and location-source codes from user-facing values.

## Layer 4 — author interface audit

The author verifies actual typing, map behaviour, point/polygon interaction, comprehension, responsive layout, ES/EN switching, and opening the Excel/HTML exports. Automated tests do not replace this layer.

## Layer 5 — live gateways

Run `source("AUDITAR_FUENTES_EN_VIVO.R")`. Expected: **36/36 checks** covering current Open-Meteo deterministic/ensemble acquisition, minimum complete-member coverage, explicit retained/excluded-member separation, exact per-member horizon diagnostics, raw JSON receipts and hashes, SoilGrids WCS/WebDAV extraction, required retention layers, profile validity, `sessionInfo()`, and an auditable ZIP bundle. The gate packages evidence even when a source fails.

## Territorial interpretation contract

One registry row contains one institutional intake coordinate. Multiple parish choices in a canton mean that different registry records occur in those parishes; they do not prove that one system has several intakes. A source label mentioning more than one locality is retained as source information and is not converted into an inferred second intake.

## Spatial-input contract

The institutional intake is a territorial reference. Weather and soil calculations use the productive point supplied or confirmed by the user. A marked point is only a representative coordinate: it creates no footprint and distributes no hectares around itself, so the numeric crop area declared by the user governs point-mode volume. A polygon must delineate only the land planted with the selected crop. The application calculates its estimated map-derived area and centroid, then requires the user to choose explicitly between declared and polygon-derived area. The selected area governs volume; the centroid supplies a representative coordinate for point-based weather and soil queries.


## RC6 freeze

Run `source("PREPARAR_CANDIDATA_RC6.R")`. Expected gates: 36/36 syntax files, 208/208 static preflight checks, 108/108 scientific checks, 84/84 integration checks, 15/15 packaging checks, 12/12 freeze checks and 32/32 final RC6 checks.
