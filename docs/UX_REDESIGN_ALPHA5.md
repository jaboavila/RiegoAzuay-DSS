# UX redesign record — v0.9.0-alpha5

## Evidence triggering the redesign

The author executed v0.9.0-alpha4 locally under R 4.6.0 and Shiny 1.14.0. The returned logs contain 116/116 static, 108/108 scientific, and 32/32 integration passes. No runtime error was reported. The redesign therefore addresses author-observed usability limitations rather than a failure of the scientific engine.

## Changes

1. **Global system search.** The 159-record selector is searchable by system name or surrogate ID. A selected system synchronises the harmonised canton and parish controls and recentres the territorial context without changing the source registry fields.
2. **Persistent navigation.** Back and Next remain available at the bottom of every workflow state. The scenario Next action calculates the plan before proceeding to evidence review.
3. **Plain-language sources.** Public labels use “demonstration weather data” and “saved demonstration forecast”, while internal mode identifiers remain unchanged for reproducibility.
4. **Operational daily calendar.** The simple result shows one action for every day from the current turn through the day before the next turn. Water is assigned only on an allowed turn day. Non-turn days state that no irrigation is scheduled or flag monitoring when stress is projected.
5. **Progressive disclosure.** Core action, volume, litres, duration, capacity, readiness, and priority warnings remain visible. Scenario envelopes, raw water-balance states, diagnostics, and complete provenance tables are available in technical view.
6. **Step instructions.** Each screen states what the user must do, why the information is requested, and what the boundary of the output is.
7. **Simplified initial-water input.** Four user-declared conditions map transparently to documented technical states. They remain unverified declarations and do not raise evidence readiness.

## Scientific invariants

No FAO-56 equation, soil-storage profile, turn-capacity equation, evidence rule, crop parameter, meteorological scenario, or retrospective result was changed. Alpha5 changes interaction and reporting only.
