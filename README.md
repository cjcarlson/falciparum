# The historical fingerprint and future impact of climate change on childhood malaria in Africa 🦟🦠

![banner](https://github.com/cjcarlson/falciparum/blob/master/TempFiles/GithubPreview.jpg)

This repository contains code for Carlson, Carleton, Odoulami, and Trisos (2022), "The historical fingerprint and future impact of climate change on childhood malaria in Africa," preprint forthcoming on medRxiv. Primary data on malaria were previously published by Snow et al. (2017) and are available online; climate data are too large for Github.

R scripts to execute data analysis and visualization are stored in the Pipeline directory:

```
A - Utility functions
├─ A01 - Utility code for calculations.R
└─ A02 - Utility code for plotting.R

B - Extract climate and prevalence data
├─ B01 - Extract CRU data and temperature.R
├─ B02 - Extract GCMs - historical.R
├─ B03 - Extract GCMs - natural counterfactual.R
└─ B04 - Extract GCMs - future.R

C - Model estimation
├─ C01 - MainSpec.R
└─ C02 - Bootstrap.R 

D - Model sensitivity analyses and checks
├─ D01 - Model sensitivity.R
└─ D02 - Randomization tests.R

E - Estimate historical and future prevalence
├─ E01 - Predict prevalence - historicals.R
└─ E02 - Predict prevalence - futures.R

F - Figure generation for main text
|
├─ F01 - Fig1 - Maps.R
├─ F02 - Fig1 - Time series and assembly.R
|
├─ F03 - Fig2 - Extract global time series.R
├─ F04 - Fig2 - Coefficients.R
├─ F05 - Fig2 - Global time series.R
├─ F06 - Fig2 - Assembly.R
| 
├─ F07 - Fig3 - Extract historical maps.R
├─ F08 - Fig3 - Extract time series.R
├─ F09 - Fig3 - Maps.R
├─ F10 - Fig3 - Time series.R
├─ F11 - Fig3 - Assembly.R
|
| ### in below scripts, pipeline reflects that 4.5 was added to figure later
├─ F12 - Fig4 - Extract 2050 maps - 2.6 and 8.5.R
├─ F13 - Fig4 - Extract 2100 maps - 2.6 and 8.5.R
├─ F14 - Fig4 - Extract 2050 and 2100 maps - 4.5.R
├─ F15 - Fig4 - Extract time series.R
├─ F16 - Fig4 - 2050 maps - 2.6 and 8.5.R
├─ F17 - Fig4 - 2100 maps - 2.6 and 8.5.R
├─ F18 - Fig4 - 2050 and 2100 maps - 4.5.R
├─ F19 - Fig4 - Time series.R
└─ F20 - Fig4 - Assembly.R

G - Figure generation for supplement
├─ G01 - Visualize thermal curve expectations and data.R
├─ G02 - Extract historical data.R
├─ G03 - Historical partials.R
├─ G04 - Extract future data.R
├─ G05 - Future partials.R
└─ G06 - Visualize background and sampled climate variability.R

H - Summary statistics for main text
├─ H01 - Thermal responses.R
├─ H02 - Global historicals.R ### (reuses data from G02)
├─ H03 - Extract historical regionals.R
├─ H04 - Regional historicals.R
├─ H05 - Global futures.R ### (reuses data from G04)
├─ H06 - Extract future regionals.R
└─ H07 - Regional futures.R
```
