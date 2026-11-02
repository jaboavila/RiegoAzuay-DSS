# RC5 patch: mode-aware public integration soil-source contract

## Trigger

RC4 completed the author/reviewer workflow and created both distributed packages, but the extracted public package self-test returned 82/84 integration checks. The two failures were INT82 and INT83.

## Root cause

The public package intentionally omits the restricted 159-system registry and runs with the five-record synthetic territorial fixture. Its controlled smoke scenario therefore uses `soil_mode = "fao_reference"`. INT82 and INT83 nevertheless required the user-facing source table to contain the Santul SoilGrids wording used only by the restricted author/reviewer scenario (`soil_mode = "archived_santul"`). The scientific computation and source labelling were correct; the test expectation was not conditional on the active registry/soil mode.

## Correction

The integration suite now defines explicit Spanish and English expected soil-source descriptions for every supported soil mode. INT82 and INT83 verify that the exported text:

- excludes internal tokens;
- is fully localised; and
- matches the active soil mode.

Thus the restricted author/reviewer scenario expects the archived Santul SoilGrids label, whereas the non-confidential public scenario expects the general FAO reference-soil label.

## Regression protection

Static check CODE077 verifies that the integration contract remains mode-aware. No scientific module, equation, parameter, interface calculation, retrospective result, or confidentiality rule was changed.
