# prov-seminar-DS

[![DOI](https://zenodo.org/badge/1167865968.svg)](https://doi.org/10.5281/zenodo.18805227)

## Overview
This repository contains 
- `prov.png`: A provenance diagram for the survey study (Practical Exercise 2).
- `A Global Impact AI DV`: A simple DV app made with rshiny (Practical Exercise 1).
  
## License
This project is licensed under the Apache License 2.0 – see the [LICENSE](LICENSE) file.

## Citation
If you use this software, please cite:
> Roopha Sharon, "Global AI Content Impact DV", v1.0.0, DOI: 10.5281/zenodo.18805228

## Dataset Requirements
The application expects a CSV file located at:
`data/Global_AI_Content_Impact_Dataset.csv`

> Important: The app loads the CSV using a **relative path**, so the `data/` folder must be present when running locally and when deploying.

## How to Run Locally (RStudio)
1. Open RStudio and set the working directory to the folder containing `app.R`.
2. Install required packages (one-time setup).
3. Run the application.

### Install dependencies
```r
install.packages(c(
  "shiny", "shinydashboard", "dplyr", "tidyr",
  "ggplot2", "plotly", "leaflet", "sf",
  "DT", "networkD3", "cluster", "umap",
  "viridis", "rnaturalearth", "rnaturalearthdata",
  "countrycode"
))
8) shinyapps.io Link

App URL: https://roophasharon.shinyapps.io/GlobalAIContentImpactDV/
