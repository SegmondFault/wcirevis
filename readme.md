# WCI Revisualised

This project rebuilds the World Cybercrime Index (WCI) visualisation with
explicit correction for respondent bias and accusation over-representation.

## Core ideas

- Raw accusation counts are misleading because high-response countries dominate.
- Accusations are therefore normalised **per accuser**, not per target.
- Respondent counts are treated as ground truth and explicitly modelled,
  including a required “Not given” category.

## Data sources

- Original WCI survey data
- Derived respondent tallies (nationality and residence)
- Accusation matrices (nationality and residence)

## What this visualisation allows

- Compare WCI, WCI per capita, WCI per GDP
- Inspect who accuses a given country (normalised)
- Toggle nationality vs residence perspectives
- Avoid false dominance caused by response volume

This repo focuses on *methodological correctness*, not aesthetics.
