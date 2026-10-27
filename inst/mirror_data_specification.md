# The `mirror_data` Dataset Specification

**Version 1.0 · schema_version = "1.0"**

This document defines the canonical `mirror_data` structure that underlies the
`mirrorslope` R package and the `%mirror_slope` SAS macro. Both implementations
construct and consume datasets conforming to this specification. Conformance is
what guarantees that the R package and the SAS macro produce equivalent figures
from equivalent inputs.

This is a stable contract. Changes to the structure require a `schema_version`
increment and a documented migration path.

---

## 1. Purpose and scope

A `mirror_data` object is a single tabular structure plus attached metadata that
together contain everything required to render a mirror slope plot:

- the subject-level observations used for optional individual trajectory lines,
- the group-level summary estimates that define the mean slope lines,
- the optional inferential quantities (difference, precision, p-value),
- and the metadata describing arms, timepoints, population, and provenance.

The structure is deliberately model-agnostic. It stores pre-computed estimates;
neither implementation fits statistical models. This keeps the contract stable
across analysis methods (observed means, LS-means from MMRM, medians, Bayesian
posterior summaries, and so on).

The plot is intrinsically a two-arm comparison. A conforming `mirror_data`
object contains exactly two treatment arms: a reference (control) arm and a
comparator (treatment) arm.

---

## 2. Tabular structure

The data table is in long format. Each row carries a `ROWTYPE` value that
identifies the kind of information it holds. Two `ROWTYPE` values are defined.

### 2.1 Row types

| ROWTYPE | Meaning | Cardinality |
|---------|---------|-------------|
| `OBS`   | One subject-level observation at one timepoint | One row per subject per retained timepoint |
| `SUMM`  | One group-level summary for one arm | Exactly one row per arm (two total) |

### 2.2 Column definitions

The following columns constitute the conforming structure. Column names are
fixed and case-sensitive. Both implementations emit these exact names.

| Column | Type | Applies to | Required | Definition |
|--------|------|-----------|----------|------------|
| `ROWTYPE`   | character | all rows | Yes | `"OBS"` or `"SUMM"`. |
| `SUBJID`    | character | OBS | Yes (OBS) | Subject identifier. Empty/missing on SUMM rows. |
| `ARM`       | character | all rows | Yes | Treatment arm label. Must be one of the two arm values recorded in metadata. |
| `ARMROLE`   | character | all rows | Yes | `"REFERENCE"` or `"COMPARATOR"`. Identifies which side of the mirror the arm occupies. REFERENCE plots left, COMPARATOR plots right. |
| `VISIT`     | character | OBS | Yes (OBS) | Visit label. Must equal either the baseline or endpoint label in metadata. Empty/missing on SUMM rows. |
| `TIMEROLE`  | character | OBS | Yes (OBS) | `"BASELINE"` or `"ENDPOINT"`. Disambiguates the visit independent of its display label. Empty/missing on SUMM rows. |
| `AVAL`      | numeric | OBS | Yes (OBS) | Observed analysis value for the subject at the visit. Missing on SUMM rows. |
| `ESTIMATE`  | numeric | SUMM | Yes (SUMM) | Group-level summary estimate at the endpoint visit. Missing on OBS rows. |
| `BASELINE`  | numeric | SUMM | Yes (SUMM) | Group-level baseline value from which the mean slope line is drawn. For change-from-baseline outcomes this is `0`. For absolute-scale outcomes this is the group baseline estimate. Missing on OBS rows. |
| `STATTYPE`  | character | SUMM | Yes (SUMM) | Free-text label for the estimate type, e.g. `"Mean"`, `"LS-Mean"`, `"Median"`. Drives the strip-header statistic label. Missing on OBS rows. |
| `SE`        | numeric | SUMM | Optional | Standard error of the arm estimate. Missing if not supplied. |
| `CI_LOWER`  | numeric | SUMM | Optional | Lower confidence bound for the arm estimate. |
| `CI_UPPER`  | numeric | SUMM | Optional | Upper confidence bound for the arm estimate. |
| `N`         | numeric | SUMM | Yes (SUMM) | Number of analyzed subjects in the arm after population filtering. Drives the `n=` display in the strip header. |

### 2.3 Difference quantities

The between-arm difference is a single value pertaining to the comparison, not
to either arm individually. It is stored on the COMPARATOR arm's SUMM row in the
following columns. On the REFERENCE SUMM row and on all OBS rows these are
missing.

| Column | Type | Required | Definition |
|--------|------|----------|------------|
| `DIFF`        | numeric | Optional | Difference, comparator minus reference, on the endpoint scale. |
| `DIFF_SE`     | numeric | Optional | Standard error of the difference. |
| `DIFF_CILOW`  | numeric | Optional | Lower confidence bound for the difference. |
| `DIFF_CIHIGH` | numeric | Optional | Upper confidence bound for the difference. |
| `DIFF_CILEVEL`| numeric | Optional | Confidence level for the difference interval, e.g. `0.95`. Label only. |
| `PVALUE`      | numeric | Optional | P-value for the difference. |

A difference is considered "present" when `DIFF` is non-missing. The relative
benefit (e.g. "percent less decline") is not stored; it is derived at render
time from `ESTIMATE` and `BASELINE` of the two arms.

---

## 3. Metadata

Metadata travels with the data table. In R it is attached as attributes on the
`mirror_data` object. In SAS it is emitted as a companion single-row dataset
(`<name>_META`) and/or as dataset labels, with the same field names. Both
implementations populate every field below.

| Field | Type | Definition |
|-------|------|------------|
| `schema_version`   | character | Specification version this object conforms to. `"1.0"` for this document. |
| `reference_arm`    | character | The `ARM` value whose `ARMROLE` is `REFERENCE`. |
| `comparator_arm`   | character | The `ARM` value whose `ARMROLE` is `COMPARATOR`. |
| `baseline_label`   | character | Display label for the baseline visit. |
| `endpoint_label`   | character | Display label for the endpoint visit. |
| `value_label`      | character | Human-readable description of `AVAL`, used as the default y-axis label. |
| `population_label` | character | Description of the analysis population after flag filtering, e.g. `"ITT"`, `"Safety"`. |
| `pop_filters`      | character | Record of the population flags applied, e.g. `"SAFFL=Y; ANL01FL=Y"`. Empty if none applied. |
| `direction`        | character | `"decrease"` or `"increase"`, or empty if to be auto-derived at render time. |
| `n_reference`      | numeric | Analyzed N in the reference arm (mirrors the REFERENCE SUMM `N`). |
| `n_comparator`     | numeric | Analyzed N in the comparator arm (mirrors the COMPARATOR SUMM `N`). |
| `created`          | character | ISO 8601 timestamp of object creation. |
| `source_tool`      | character | `"mirrorslope (R) <version>"` or `"%mirror_slope (SAS) <version>"`. |
| `call`             | character | The originating function/macro call, captured for provenance. |

### 3.1 Direction derivation

When `direction` is empty, render-time logic derives it by comparing the
reference arm's endpoint `ESTIMATE` to its `BASELINE`:

- `ESTIMATE < BASELINE` → `"decrease"` (the outcome declined; less decline is better)
- `ESTIMATE > BASELINE` → `"increase"` (the outcome rose; less increase is better)
- `ESTIMATE == BASELINE` → ambiguous; the implementation must require an explicit `direction`.

This rule is correct for both change-from-baseline data (`BASELINE = 0`) and
absolute-scale data (`BASELINE` is the real starting value).

---

## 4. Construction pipeline

Both implementations build a conforming object through an ordered sequence. The
R package exposes these as composable, pipe-friendly functions; the SAS macro
performs the equivalent steps internally and emits the conforming dataset.

1. **Prepare** — filter to the two named arms, apply population flags, retain
   only baseline and endpoint timepoints, coerce types, assign `ARMROLE` and
   `TIMEROLE`, count `N` per arm, populate base metadata including
   `schema_version` and provenance. Produces OBS rows.
2. **Add summary** — attach `ESTIMATE`, `BASELINE`, `STATTYPE` per arm as SUMM
   rows.
3. **Add precision** *(optional)* — attach per-arm `SE` or `CI_LOWER`/`CI_UPPER`
   to the SUMM rows.
4. **Add difference** *(optional)* — attach `DIFF` and any of `DIFF_SE`,
   `DIFF_CILOW`, `DIFF_CIHIGH`, `DIFF_CILEVEL`, `PVALUE` to the COMPARATOR SUMM
   row.

Steps 3 and 4 are independent and may occur in either order or be omitted.

---

## 5. Validation rules

A conforming object satisfies all of the following. Both implementations enforce
these and emit equivalent diagnostics.

### 5.1 Errors (construction fails)

- A required column is absent.
- More than two distinct `ARM` values are present. *(Message names the arms and
  directs the user to subset to a reference and comparator before proceeding.)*
- `ARMROLE` does not contain exactly one `REFERENCE` and one `COMPARATOR`.
- A SUMM row's `ARM` does not match a recorded arm.
- `STATTYPE` differs between the two SUMM rows. *(Message names the mismatched
  arm.)*
- `ESTIMATE` is missing on a SUMM row.
- A non-numeric value is supplied where `AVAL`, `ESTIMATE`, or `BASELINE` is
  required.
- `direction` cannot be derived (reference `ESTIMATE == BASELINE`) and none was
  supplied.
- A request to render a difference annotation is made while `DIFF` is missing.
  *(Message: difference display was requested but no difference is attached; add
  it first.)*

### 5.2 Warnings (construction proceeds)

- A subject has a baseline observation but no endpoint observation, or the
  reverse. *(Affected subjects are dropped from OBS rows and named.)*
- A `SUBJID` appears under both arms. *(Permitted: may reflect a crossover or
  before/after design. The user is alerted.)*
- Duplicate `SUBJID`/`VISIT` combinations exist within an arm. *(Permitted: may
  reflect a multiple-record design. The user is alerted.)*
- An arm contains only one analyzed subject.
- Missing values are present in `AVAL`. *(Count is reported; rows are excluded
  from individual trajectory lines.)*
- A factor arm variable carries levels not present in the data. *(Coerced to
  character; the user is alerted.)*
- The sign of `DIFF` is inconsistent with the relationship between the two arms'
  `ESTIMATE` values. *(Possible data-entry error.)*
- A manually supplied plot `x_max` is smaller than the space required by the
  annotation zone. *(The value is overridden and the user is alerted.)*

---

## 6. Rendering contract

The following visual rules are part of the specification because they determine
how a conforming object becomes a figure, and both implementations honor them.

- **Shared coordinate space.** Both arms share one x-axis. The reference arm's
  baseline sits left of center; the comparator's baseline sits right of center;
  both mean lines terminate at the central endpoint position. The between-arm
  gap at the endpoint is the primary visual quantity.
- **Mean lines** are drawn from each arm's `BASELINE` at its baseline x-position
  to its `ESTIMATE` at the central endpoint x-position.
- **Strip headers** show, per arm, the arm label, the `STATTYPE` and `ESTIMATE`,
  and — when enabled — the arm `N` as `n=`.
- **Annotation zone** occupies space to the right of the data panel. The x-axis
  maximum expands automatically to accommodate it; the data panel geometry does
  not change. The zone presents the difference, its precision and p-value, and
  the derived relative benefit, as requested. It always sits on the right.
- **Optional elements are off by default.** Individual trajectory lines, the
  difference annotation, the relative-benefit annotation, and per-arm precision
  indicators are each shown only on explicit request. A conforming object stores
  all available information; the renderer exposes only what is asked for.

---

## 7. Versioning and migration

The `schema_version` field records the specification version of every object.
Implementations check it on load. When a future specification changes the
structure:

- the version is incremented,
- the change and its rationale are recorded in a revision history,
- and a documented migration converts prior-version objects, or — at minimum —
  the implementation detects the older version and reports clearly rather than
  failing silently.

This discipline supports reproducibility requirements in journal and regulatory
settings, where a figure produced today must be reconstructable later.

---

## 8. Revision history

| schema_version | Date | Summary |
|----------------|------|---------|
| 1.0 | 2026 | Initial specification. |
