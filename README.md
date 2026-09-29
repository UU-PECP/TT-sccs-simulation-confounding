
# [PROJECT NAME] PROJECT [NUMBER]

**Study Status:** [Design Finalized / Under Analysis / Results Under Review / Complete]

## Research Question

[State the primary research question. Include the treatment or exposure of interest, the outcome, and the target population and time period.]

### Sub-objectives

- [Sub-objective 1]
- [Sub-objective 2]
- [Sub-objective 3]

## Background

[Provide scientific context for the study. Summarize existing evidence from randomized trials and real-world studies, and identify the knowledge gap this study addresses.]

## Study Design

| Element | Description |
|---|---|
| Design | [e.g. New-user active comparator cohort study] |
| Exposure | [e.g. Dapagliflozin] |
| Comparator | [e.g. DPP-4 inhibitor] |
| Primary outcome | [e.g. Time to first MACE] |
| Follow-up | [e.g. Up to X years from treatment initiation] |
| Analysis approach | [e.g. Propensity score matching, IPTW] |

## Data Sources

| Database | Country | Coverage period | Data type |
|---|---|---|---|
| [e.g. CPRD Aurum] | [e.g. UK] | [e.g. 2010–2023] | [e.g. Primary care EHR] |
| [e.g. PHARMO] | [e.g. NL] | [e.g. 2010–2023] | [e.g. Primary care EHR + Hospitalization] |
| [e.g. CF registry] | [e.g. NL] | [e.g. 2007–2023] | [e.g. EHR + primary data] |


## Data Access
Analysis-ready datasets and aggregate outputs are archived on DataverseNL:

[Dataset](https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/91TAZD)

Raw data cannot be shared openly due to [privacy restrictions / data 
governance / DUA]. Access requests should be directed to [contact or 
institution].

Add something here about synthetic data being uploaded to dataverse (CPRD or PHARMO)

## Protocol

[Published version](LINK_TO_PUBLISHED_PROTOCOL)

[Track Changes version](LINK_TO_TRACK_CHANGES_VERSION)

[Shell tables and figures](LINK_TO_SHELL_TABLES)

## Manuscript

[Manuscript draft](LINK_TO_MANUSCRIPT)

[Supplementary materials](LINK_TO_SUPPLEMENTARY)

## Repository Structure

```
project-root/
├── scripts/                # Analysis scripts
│   ├── 01_cohort.R
│   ├── 02_analysis.R
│   └── 03_tables.R
├── codelists/              # Code lists used for variable definitions
├── documentation/
│   ├── Decision_logs.md    # Decision logbook and other project documentation
├── output/                 # table and figures for the project
├── DATA.md                 #Instructions for accessing data via Dataverse NL    
└── README.md
```

## How to Run

### 1. Data setup
Download the following files from the [DataverseNL repository]
(https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/91TAZD) 
and place them in a local `/data` folder (not tracked by Git):

- `cohort_extract.rds`
- `codelists_final.csv`

### 2. Environment setup
**R version:** ≥ 4.2.0

**Key packages:**

```r
install.packages(c("tidyverse", "survival", "MatchIt", "tableone"))
```

**Execution order:**

1. `scripts/01_cohort.R` — Defines and extracts the study cohort
2. `scripts/02_analysis.R` — Runs primary and sensitivity analyses
3. `scripts/03_tables.R` — Generates tables and figures

[Add any database connection setup or environment-specific notes here.]

## Status Log

| Date | Milestone |
|---|---|
| [YYYY-MM-DD] | Protocol v1 submitted |
| [YYYY-MM-DD] | Analysis started |
| [YYYY-MM-DD] | Results reviewed |
| [YYYY-MM-DD] | Manuscript submitted |

## Citation

**Paper:**
> Author(s). Title. Journal. Year. DOI

**Code:**
> Author(s). Project code. GitHub. Year. URL

**Data:**
> Author(s). Dataset title. DataverseNL. Year. 
> https://doi.org/10.34894/91TAZD

[Pre-print available here](LINK_TO_PREPRINT)

## Contact

For questions or support, contact:

- Project Lead
  - Name: [Name]
  - Email: [email@institution.XXX]
- Programming Lead
  - Organisation: [Organisation Name]
  - Email: [email@institution.XXX]
