# Output dictionary — alpha10

| Element | Interpretation |
|---|---|
| Decision unit | One parcel or homogeneous zone planted with one selected crop. |
| Effective crop area | Area actually used in the depth-to-volume equation. |
| Declared crop area | Numeric area entered by the user for the selected crop. |
| Polygon-derived area | Estimated map-derived area of the crop polygon. |
| Area source | Explicit choice: declared or polygon-derived. |
| Representative location | User point, polygon centroid, manual coordinate, or explicitly labelled intake approximation. |
| Point mode | No footprint or buffer is generated; declared area remains active. |
| Polygon mode | Polygon area may become active only after explicit user choice; centroid supports point queries. |
| Required volume | Gross volume required for the effective crop area. |
| Deliverable volume | Volume that can be supplied under active flow and turn duration. |
| Unmet volume | Nonnegative operational shortfall. |
| E0–E4 | Internal evidence-readiness engineering rule, not an externally validated scale. |
