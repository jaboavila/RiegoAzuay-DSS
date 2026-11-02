# Administrative-label crosswalk audit — v0.9.0-alpha15

## Purpose

The institutional registry deliberately preserves source canton and parish text. The user-facing filters require a separate display layer so that orthographic, accent, case, punctuation, typographical, and embedded-code variants do not appear as duplicate options.

## Restricted-registry results

- Registry rows before and after display harmonisation: **159**.
- Nonblank source canton labels: **25 distinct values**.
- Harmonised canton display labels: **13 distinct values**.
- Nonblank source parish labels: **71 distinct values**.
- Harmonised parish display labels: **50 distinct values**.
- Missing source canton values: **4 records**.
- Missing source parish values: **5 records**.
- Records with at least one missing administrative label: **5**; all remain selectable.
- Coordinate values modified: **0**.
- Public IDs modified: **0**.
- System names modified: **0**.

## Governance rule

`canton` and `parish` remain the institutional source fields. `canton_source` and `parish_source` retain their safe textual representations. `canton_display` and `parish_display` are generated from `config/administrative_label_crosswalk_v1.csv` and are used only for labels, filters, map popups, and selected-record exports. No display correction is written back to the institutional source CSV.

## Examples of consolidated display variants

- `Giron` and `Girón` → `Girón`.
- `Nabon` and `Nabón` → `Nabón`.
- `SIGSIG` and `Sigsig` → `Sígsig`.
- `Chordelg` and `Chordeleg` → `Chordeleg`.
- `Pucara`, `Pucará`, `Pucará (06)`, and `Pucará 06` → `Pucará`.
- Parish variants such as `Las.Nieves/Las Nieves`, `San Bartolome/San Bartolomé/San Boftolome`, and `Zhidmad/Zhudmad/Znhidmad` are consolidated analogously.

## Automated guards

The static and integration suites verify complete crosswalk coverage, source-value preservation, absence of normalised duplicate canton/parish labels, uniqueness of parish labels within each harmonised canton, and retention of all 159 coordinate-valid selectable records.
