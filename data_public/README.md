# data_public/

Data folder for `code/reproduce fear of elephants analyses.R`, accompanying:

> Wood, B.M., Deffner, D., Paolo, B., Anyawire, M., Mabulla, A., Kiffner, C.
> "Fear of Elephants Shapes Hadza Hunter-Gatherer Movement and Creates a Refuge for their Prey."

## Running it

1. Open `Fear of Elephants.Rproj` in RStudio (`data_public/`, `fit_models_public/`, and `code/`
   need to be siblings of one another).
2. Source `code/reproduce fear of elephants analyses.R`.
3. Run:
   ```r
   reproduce_all(data_location, figures_location, tables_location, max_figure_dimensions)
   ```
4. Outputs land in `figures_public/`, `tables_public/`, and `text_reported_results_public/`.

No internet access, compilation, or model fitting required. A full run takes a few minutes.

## Checking model formulas and priors

- Models 7–10 (`model_7.rds` = diurnality, `model_8.rds`/`model_9.rds`/`model_10.rds` = RAI) are
  `brms` objects — load any one with `readRDS()` and call `formula(fit)`, `fit$prior`, or
  `summary(fit)`.
- The path-selection (PSA) model was fit directly in Stan, not `brms`, so the posterior-draw
  objects (`s_Elephants_Environment_rot.rds`, etc.) don't carry a queryable formula. The model
  itself is in `Hadza_PSA_latent.stan` (full model) and `Hadza_PSA_latent_elephantonly.stan`
  (elephant-distance-only model).
- The `brm()` calls that produced Models 7–10 are also in `code/reproduce fear of elephants
  analyses.R` (`fit_model_7()`, `fit_model_8()`, `fit_model_9()`, `fit_model_10()`, and the priors
  in `rai_priors()`), directly above `reproduce_all()`. `reproduce_all()` doesn't call
  them — Models 7–10 load pre-fit, as described above — but they run as-is against the data
  already in this package if you want to verify or rerun a fit yourself.

## Coordinates and location data

No raw geographic coordinates are included, and no geometry — anonymized or otherwise — is
included for Figure 2.

Figure 3/S1 Panel A's illustrative path uses translated coordinates — relative distances and shapes are exact but absolute position isn't recoverable.

## Models are loaded, not refit

The diurnality, RAI, and PSA models all load from `fit_models_public/` rather than being fit
from scratch.

## Environment

R 4.3.2. Key packages: brms 2.22.0, rethinking 2.42, magick 2.8.6, loo 2.8.0, patchwork 1.3.0,
dplyr 1.1.4, tidyr 1.3.1. Full `sessionInfo()` is in `session_info.txt`.
