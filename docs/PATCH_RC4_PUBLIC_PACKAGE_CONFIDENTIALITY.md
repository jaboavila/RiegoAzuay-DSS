# RC4 correction: public-package confidentiality and self-containment

## Independent RC3 finding

The RC3 release-candidate gates passed, but an independent inspection of the generated
public-preview ZIP found that two territorial audit files had been copied into the package:

- `data/audit/territorial_selector_audit_final_v1.csv`
- `data/audit/territorial_name_ambiguity_audit_v1.csv`

The first file contained 168 institutional audit rows, including irrigation-system and
organisation names and precise coordinates. The second contained named ambiguity cases
and coordinates. Their inclusion contradicted the stated data-governance boundary even
though `data/restricted/systems_registry_159_restricted.csv` itself was absent.

The same inspection found that the public package omitted the safe
`data/restricted/README.md`, so its own preflight could not be treated as self-contained,
and its frozen package inventory exposed author-workstation library paths.

## RC4 correction

RC4:

1. excludes the detailed territorial audit files from the public package;
2. retains only aggregate selector counts and a non-identifying public summary;
3. recreates a safe `data/restricted/README.md` stating that the registry is absent;
4. omits historical author-runtime logs, accepted restricted-registry test outputs, and
   intermediate troubleshooting notes from the public package while retaining them in the
   author/reviewer evidence chain;
5. sanitises package inventories in both public and reviewer packages by removing local
   library and package paths;
6. makes static and integration tests work in both restricted and synthetic-registry modes;
7. executes the extracted public package through syntax, preflight, scientific, and
   integration gates before accepting the candidate;
8. scans the public ZIP for forbidden audit files, exact institutional coordinate
   fingerprints, and local workstation paths.

The reviewer-controlled package retains the processed 159-system registry and detailed
territorial audit under the existing non-redistribution notice.

No equation, scientific module, interface behaviour, retrospective result, or public-source
response was changed.
