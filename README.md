# Fear of Elephants — Reproducibility Package

Code and de-identified data to reproduce the results in:

> Wood, B.M., Deffner, D., Paolo, B., Anyawire, M., Mabulla, A., Kiffner, C.
> "Fear of Elephants Shapes Hadza Hunter-Gatherer Movement and Creates a Refuge for their Prey."

`code/reproduce fear of elephants analyses.R` is the reproduction script — source it, then run
`reproduce_all(data_location, figures_location, tables_location, max_figure_dimensions)`.
`data_public/` and `fit_models_public/` hold the data and pre-fit models it reads. See
`data_public/README.md` for details on running it, checking model formulas/priors, and a couple
of known caveats.

R 4.3.2. Key packages: brms 2.22.0, rethinking 2.42, sf 1.0-20, terra 1.8-42, raster 3.6-32,
loo 2.8.0, patchwork 1.3.0, ggspatial 1.1.9, dplyr 1.1.4, tidyr 1.3.1. Full `sessionInfo()` is
in `data_public/session_info.txt`. No internet access, compilation, or model fitting required.
