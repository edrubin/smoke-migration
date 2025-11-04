# Smoke migration

This repository contains the code and documentation for our smoke-migration project entitled "Inequalities in wildfire smoke avoidance".

__Authors__ [Edward Rubin](https://edrub.in) and [M. Steven Holloway](https://mstevenholloway.mmm.page/home)

__Abstract__ Humans can reduce risk exposure through short-term avoidance, but this strategy may not be equally accessible to all members of a population. We combine data from cellphone movements, satellite-based wildfire smoke plumes, and Census-reported demographics to document substantial heterogeneity and inequity in communities' tendencies to out-migrate to avoid smoke. Higher-income and whiter populations leave their counties at significantly higher rates during smoke events. These results suggest that the same populations who face social and environmental injustice on many other measures are less able to avoid wildfire smoke—underscoring equity concerns for wildfire damages and climate adaptation.

## Repository organization

**Code** All scripts/code live in the `./code` folder. Within the `code` folder, files are named according to their order and task. For example, `000a-download-smoke.R` is the first script (`a`) to run among all the data-prep scripts (`000`); it downloads smoke-plume data from the NOAA Hazard Mapping System.

In other words, scripts in the `./code` folder follow the pattern: `[stage][sequence]-[description].R`

- **Stage numbers** `000` (data prep), `001` (processing), `002` (analysis), _etc._
- **Sequence letters** `a`, `b`, `c`, ... (execution order within stage)
- **Example** `000a-download-smoke.R` is the first data-prep script

**Data** Raw data should be placed in the `./data-raw` folder. The required data files for the analyses not included in this repository due to (1) their size and (2) data-use agreements. However, `000a-download-smoke.R` downloads the smoke plumes and saves them in `./data-raw/noaa`. Described Census data should be downloaded from the Census into `./data-raw/census`. Cellphone-movement data are available from [Dewey](https://www.deweydata.io/). The processed data files used in the analyses are saved in the `./data-processed` folder. Results from analyses go into `./data-results`, `./data-figures`, and ultimately `./figures` and `./tables`.

Please contact [Edward Rubin](https://edrub.in) with any questions/concerns about the project.

## Directory tree

```
smoke-migration/
├── code/
│   ├── 000a-download-smoke.R
│   ├── 000b-*.R
│   ├── ...
│   ├── 001a-*.R
│   ├── 001b-*.R
│   ├── ...
│   ├── 002a-*.R
│   ├── 002b-*.R
│   └── ...
├── data-raw/
│   ├── census/
│   ├── noaa/
│   └── ...
├── data-processed/
├── data-results/
├── data-figures/
├── figures/
└── tables/
```

