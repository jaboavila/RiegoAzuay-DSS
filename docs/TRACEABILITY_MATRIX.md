# Scientific-to-software traceability matrix — alpha15

| Methods / functional component | Implementation | Evidence path |
|---|---|---|
| 159-system registry and intake semantics | `R/app_data.R`, restricted registry, `app.R` | `REG001`–`REG024`, `INT01`–`INT03`, `INT21`–`INT31`, `INT47`–`INT49` |
| Searchable territorial workflow | native text field, matching selector, and synchronisation in `app.R` | `REG017`, `REG022`–`REG024`, `CODE015`–`CODE016`, `INT41`, `INT47`–`INT48`, `INT58`, author audit |
| Territorial ambiguity control | `data/audit/territorial_name_ambiguity_audit_v1.csv`, warnings | `AUD006`–`AUD007`, `INT56` |
| Productive point and polygon | `app.R`, productive-location metadata, coordinate routing in `R/app_engine.R` | `CODE029`–`CODE030`, `CODE035`, `INT50`–`INT51`, author audit |
| Five-layer architecture | `app.R`, `R/app_engine.R`, modules | `CODE001`–`CODE008` |
| Evidence hierarchy / E0–E4 | evidence gate and versioned configurations | `EVID001`–`EVID003`, `INT08`, `INT13` |
| Simplified evidence communication | action list and core source summary | `CODE031`, `INT57`, author audit |
| Crop library and phenology | profile library and crop configuration | `CROP001`–`CROP003`, scientific regression |
| Operational weather and soil references | `R/app_weather.R`, `R/app_soil.R`, gateways | offline integration; live gateways pending |
| Simple initial-water declarations | versioned preset table and input mapping | `INIT001`–`INIT004`, `INT40` |
| Daily root-zone balance | FAO-56 and layered balance modules | 108 scientific tests, `INT09` |
| Turn-constrained scheduling | layered scheduler and engine | scientific tests, `INT10`–`INT12` |
| Persistent navigation and step guidance | wizard footer and bilingual instruction cards | `CODE017`–`CODE018`, `INT41`, author audit |
| Next-turn and daily action outputs | reporting/UI helper modules and simple results | `CODE019`–`CODE021`, `INT33`–`INT39`, `INT42`, `INT45`–`INT46` |
| User workbook export | `R/app_reporting.R`, `openxlsx` | `CODE020`, `CODE032`–`CODE033`, `CODE036`, `INT52`–`INT55` |
| Bilingual reporting | i18n dictionary, daily plan, HTML, workbooks | dictionary parity audit, `INT15`, `INT37`–`INT38`, `INT52`–`INT53` |
| Reproducibility and restricted-data separation | reporting, `.gitignore`, release scripts | `GOV001`–`GOV002`, `INT18`–`INT20`, `INT39`, `INT54` |

Only functions accepted in the frozen v1.0.0 build should be reported as final Results 3.1 claims.
