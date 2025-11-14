# 

# Install `httpgd`
Rscript --vanilla -e "remotes::install_github('nx10/httpgd')"

# Download NOAA HMS smoke shapefiles
Rscript --vanilla code/000a-download-smoke.R
# Build CBG centroids (SafeGraph CBG geometry → centroids)
Rscript --vanilla code/000b-build-cbg-centroids.R
# Build POI coordinates (aggregate placekey lat/lon)
Rscript --vanilla code/000c-build-poi-coords.R
# Match historic fire perimeters to CBGs
Rscript --vanilla code/000d-match-fires-cbgs.R

# Join daily smoke shapes to CBGs (produce CBG-day smoke files)
Rscript --vanilla code/001a-process-smoke-cbg-data.R
# Process SafeGraph weekly POI data and attach weekly smoke info
Rscript --vanilla code/001b-process-poi-data.R
# Collapse weekly POI-smoke data to home-CBG weekly summaries
Rscript --vanilla code/001c-collapse-poi-smoke-data-to-cbg.R
# Aggregate weekly CBG datasets (combine/clean weekly files)
Rscript --vanilla code/001d-aggregate-cbg-week.R
# Count weekly hospital visits by CBG (hospital POIs)
Rscript --vanilla code/001e-count-cbg-hospital-visits.R
# Match POI weekly visits to smoke PM predictions (Burke et al. grid)
Rscript --vanilla code/001f-match-poi-smoke-pm.R

# Build analysis-ready dataset (merge census, areas, etc.)
Rscript --vanilla code/002a-build-analysis-data.R
# Prepare analysis data (load fonts, finalize prepped datasets)
Rscript --vanilla code/002b-prep-analysis-data.R

# Main regression analyses and result objects
Rscript --vanilla code/003a-main-analysis.R
# Quantile-heterogeneity plotting (percentile plots)
# NOTE Optional parameter excludes fire-affected areas
Rscript --vanilla code/003b-plot-quantiles.R nofire
# Build discussion plots / tables (CBG characteristic visuals)
Rscript --vanilla code/003c-build-discussion-data.R
# Event-study plotting and estimation
Rscript --vanilla code/003d-event-study.R

# Time-series plots of visits / smoke metrics
Rscript --vanilla code/004a-plot-time-series.R
# Map CBG smoke exposure (shapefile + legend + hist)
Rscript --vanilla code/004b-map-smoke-exposure.R
# Summary statistics and tables for paper
Rscript --vanilla code/004c-summarize-data.R
# Build regression LaTeX tables (via fixest etable)
Rscript --vanilla code/004d-build-reg-tables.R