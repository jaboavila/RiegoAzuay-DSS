# GitHub and archival-release steps for v1.0.0

## Public repository

1. Create a public GitHub repository named `RiegoAzuay-DSS`.
2. Upload the **contents** of `RiegoAzuay-DSS-v1.0.0.zip` to the repository root.
3. Confirm that `data/restricted/` contains only its explanatory `README.md` and that no
   reviewer-controlled files are present.
4. Commit the final public tree.
5. Create the immutable tag `RiegoAzuay-DSS-v1.0.0`.
6. Create the GitHub release `v1.0.0` and attach `RiegoAzuay-DSS-v1.0.0.zip` and its
   SHA-256 sidecar.

## Archival DOI

1. Connect the public GitHub repository to Zenodo.
2. Archive the `v1.0.0` GitHub release.
3. Copy the version-specific Zenodo DOI and the immutable GitHub release URL.
4. Insert those real identifiers in the manuscript Data Availability Statement.
5. Do not rewrite the `v1.0.0` tag after archival deposit.

## Reviewer-controlled package

Upload `RiegoAzuay-DSS-v1.0.0-reviewer-controlled.zip` only through the confidential
editorial route. Never attach it to the public GitHub or Zenodo record.
