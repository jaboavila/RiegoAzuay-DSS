# Release-candidate protocol — RiegoAzuay DSS v1.0.0-rc6

The candidate is accepted only if the single wrapper `PREPARAR_CANDIDATA_RC6.R` completes every gate and creates the consolidated evidence bundle. The wrapper must run under R 4.6.0 with Shiny 1.14.0.

Required layers:

1. R syntax gate.
2. Static preflight and governance checks.
3. Scientific regression suite.
4. Integrated application smoke tests.
5. Packaging/path-resolved manifest gate.
6. Environment freeze (`renv.lock`, `sessionInfo()`, package inventory and manifest).
7. Public-preview and controlled-reviewer package verification.
8. Final RC6 gate.

A PASS authorises review of the candidate; it does not create an immutable public release, approve a software licence or mint a DOI.

Expected RC6 counts: 36/36 syntax files, 208/208 static preflight checks, 108/108 scientific checks, 84/84 integration checks, 15/15 packaging checks, 12/12 freeze checks, and 32/32 final candidate checks.
