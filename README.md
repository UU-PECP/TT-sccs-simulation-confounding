
# SCCS TIME-VARYING CONFOUNDING SIMULATION PROJECT

**Full title:** Impact of different approaches to handle time-varying confounders affected by past exposure in the self-controlled case series design: A Monte Carlo simulation on the effect of COVID-19 infection on estimates of myocarditis risk after COVID-19 vaccination

**Study Status:** Submitted to journal

## Research Question

In self-controlled case series (SCCS) studies of adverse events following COVID-19 vaccination, COVID-19 infection can act as a time-varying confounder affected by past exposure. Using DAGs and simulation, we assessed possible biases of four commonly used approaches to handling such a confounder (C): (1) ignore C, (2) exclude individuals with C, (3) censor at C, and (4) adjust for C as a time-varying covariate.

### Sub-objectives

1) Use directed acyclic graphs (DAGs) to describe the implied estimand and the potential biases of four approaches: (1) ignore the confounder, (2) exclude individuals with the confounder, (3) censor follow-up at confounder occurrence, and (4) adjust for the confounder as a time-varying covariate.

2) Use Monte Carlo simulation to quantify the bias, empirical standard error and mean squared error of each approach across 288 scenarios that vary infection incidence, the effect of infection on vaccination, the effect of vaccination on myocarditis, the duration of the infection effect on myocarditis, and the strength of an unmeasured confounder.



## Study Design

| Element | Description |
|---|---|
| Design | Monte Carlo simulation study of the SCCS design, planned and reported with the ADEMP framework (Morris et al., 2019) |
| Exposure | Single dose of COVID-19 vaccine ($E$). Focal window: 30 days starting on the vaccination day (day 0 included) |
| Comparator | Within-person reference window: all observation time outside the focal window |
| Time-varying confounder | COVID-19 infection ($C$), affected by past vaccination |
| Unmeasured confounder | Adherence to lockdown measures ($U$), affecting both $C$ and $Y$ |
| Primary outcome | Myocarditis ($Y$), a rare and independently recurrent outcome |
| Target estimand | Log incidence rate ratio of the direct effect of $E$ on $Y$, $\log(IRR_E)$. True value: $\gamma_1 = \ln(1)$ or $\ln(2)$ |
| Follow-up | 500-day observation period for each simulated subject |
| Analysis approach | Conditional Poisson SCCS model, applied with four approaches to handle $C$ (see below) |
| Performance measures | Bias, relative bias (for true $IRR_E = 2$), empirical standard error and mean squared error, each with its Monte Carlo standard error; 1000 replications per scenario |
| Scenarios | 288 (full factorial combination of varied parameters) |
| Software | R 4.4.1 |

### Analytical approaches compared

| Approach | Method | Implied estimand |
|---|---|---|
| 1. Ignore C | Only $E$ in the SCCS model | Total effect of $E$ on $Y$ (direct effect plus indirect effect through protection against $C$) |
| 2. Exclude C | Remove subjects with at least one $C = 1$, then analyse as approach 1 | Direct effect in individuals never infected |
| 3. Censor C | Censor follow-up on the day of the first $C = 1$, then analyse as approach 1 | Direct effect in individuals while not infected |
| 4. Adjust for C | Include $E$ (30-day focal window) and $C$ (10, 30 or 60-day focal window, matching the scenario) in the model | Direct effect within each level of infection status |

The direct effect of $E$ on $Y$ is used as the reference for bias in all approaches, because this is usually the estimand of interest in vaccine safety studies.

### Data-generating mechanism

For each subject $i$ and day $t$, the statuses of $U$, $C$, $E$ and $Y$ are generated in sequence from Bernoulli distributions. $U$ has a fixed daily probability $p_U$. The probabilities of $C$, $E$ and $Y$ follow logistic models:

```math
\text{logit}\left[p_{C,it}\right] = \alpha_0 + \alpha_1 E_{i[t-90,\,t-1]} + \alpha_2 C_{i[t-90,\,t-1]} + \alpha_3 U_{i[t-89,\,t]}
```

```math
\text{logit}\left[\Pr(E_{it} = 1)\right] = \beta_0 + \beta_1 E_{i[1,\,t-1]} + \beta_2 C_{i[t-27,\,t]}
```

```math
\text{logit}\left[\Pr(Y_{it} = 1)\right] = \gamma_0 + \gamma_1 E_{i[t-29,\,t]} + \gamma_2 C_{i[t-a+1,\,t]} + \gamma_3 U_{i[t-89,\,t]}
```

where $a$ is the length of the focal window of $C$ on $Y$.

### Simulation parameters

| Parameter | Value(s) | Rationale |
|---|---|---|
| Daily probability of $U$ ($p_U$) | $10^{-3}$ | About 40% chance of at least one period with low adherence over 500 days |
| Baseline daily odds of $C$ ($e^{\alpha_0}$) | **Varied:** $5 \times 10^{-4}$, $10^{-3}$, $5 \times 10^{-3}$ | Low, moderate and high COVID-19 incidence seasons |
| Effect of past $E$ on $C$ ($e^{\alpha_1}$) | OR = 0.2 | About 80% vaccine effectiveness against infection within 3 months |
| Effect of past $C$ on $C$ ($e^{\alpha_2}$) | OR = 0.3 | About 70% protection against reinfection within 3 months |
| Effect of $U$ on $C$ ($e^{\alpha_3}$) | OR = 5 | Higher infection risk when lockdown measures are not followed |
| Baseline daily odds of $E$ ($e^{\beta_0}$) | $3.2 \times 10^{-3}$ | About 80% of subjects vaccinated within 500 days |
| Effect of past $E$ on $E$ ($e^{\beta_1}$) | OR = $10^{-6}$ | Single-dose vaccine setting |
| Effect of recent $C$ on $E$ ($e^{\beta_2}$) | **Varied:** OR = 1, 0.25, 2, 5 | No confounding, infection delays vaccination, moderate or strong confounding |
| Baseline daily odds of $Y$ ($e^{\gamma_0}$) | $2 \times 10^{-5}$ | Rare outcome; about 1000 cases per cohort |
| Effect of $E$ on $Y$ ($e^{\gamma_1}$) | **Varied:** OR = 1, 2 | No effect or harmful effect of vaccine |
| Focal window of $C$ on $Y$ ($a$) | **Varied:** 10, 30, 60 days | Myocarditis risk increased for at least 42 days after infection |
| Effect of $C$ on $Y$ ($e^{\gamma_2}$) | OR = 5 | Observed association between infection and myocarditis |
| Effect of $U$ on $Y$ ($e^{\gamma_3}$) | **Varied:** OR = 1, 0.25, 2, 15 | No, strong negative, moderate positive and very strong positive confounding |


## Data Sources

This project uses no real-world patient data. All data are simulated with parameter values infomred by published literature.

## Data Access

The scripts to generate the data and the can be accessed on [Github](https://github.com/UU-PECP/TT-sccs-simulation-confounding.git)


## Manuscript

[To be updated]

## Repository Structure

```
project-root/
├── Codes/                                  # R scripts for the simulation
│   ├── SCCS_helper_functions.R             # Helper functions
│   ├── SCCS_main_functions.R               # Main functions
│   ├── SCCS_sim_workflow_functions.R       # Workflow functions for the simulation
│   ├── SCCS_sim_execution.R                # Runs the simulation
│   ├── SCCS_sim_plot_functions.R           # Plotting functions
│   └── SCCS_sim_plot.R                     # Creates the figures
├── Documentation/                          # Project documentation
├── Results/                                # Raw simulation results
│   ├── adjust_C/                           # 288 scenarios, approach 4: adjust for C
│   ├── censor_C/                           # 288 scenarios, approach 3: censor at C
│   ├── exclude_C/                          # 288 scenarios, approach 2: exclude C
│   ├── ignore_C/                           # 288 scenarios, approach 1: ignore C
│   └── Summary/
│       └── Summary_all_scens_2026_05_01.xlsx   # Summary of performance measures of 4 approaches across all scenarios
├── renv/                                   # Project-specific R package library (renv)
├── renv.lock                               # Package versions used, to restore with renv::restore()
└── README.md
```

## How to Run

### 1. Data setup


### 2. Environment setup
**R version:** ≥ 4.4.1

1. Install the `renv` package
```r
install.packages("renv")
```

2. Ensure the following folder and files are present in the project directory:

```text
renv/
renv.lock
.Rprofile [hidden file]
```
3. Restore the project environment:
```r
renv::restore()
```

### 3. Scrip execution order

1. `Codes/SCCS_sim_execution.R` — Runs the simulation 
2. `Codes/SCCS_sim_plot.R` — Summarize the results and create plots


## Status Log

| Date | Milestone |
|---|---|
| [2025-09-04] | Protocol v1 submitted |
| [2026-05-01] | Results reviewed |
| [2026-10-27] | Manuscript submitted |


## Contact

For questions or support, contact:

- Project Lead
  - Name: Sophie Bots
  - Email: s.h.bots@uu.nl
- Programming Lead
  - Name: Trang Tu
  - Email: t.p.h.trang@uu.nl
