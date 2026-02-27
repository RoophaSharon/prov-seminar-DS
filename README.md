# odkg-seminar-DS
# Global AI Impact (Data Visualization)

[![DOI](https://zenodo.org/badge/1167865968.svg)](https://doi.org/10.5281/zenodo.18805227)

## Overview
This repository contains an interactive R Shiny dashboard developed for the **Data Visualization** 
The dashboard supports analysis of the *Global AI Content Impact Dataset* through four research questions (RQ1???RQ4) using coordinated views, interactive filtering, and multiple visualization idioms.

## Research Questions
- **RQ1 ??? Spatial Spillover:** Do countries near strict-regulation countries show different patterns in adoption and trust?
- **RQ2 ??? Temporal Dynamics:** How do Trust Gap hotspots evolve over time across regulation regimes and industries?
- **RQ3 ??? Tool Ecosystem:** What tool???country/industry ecosystems emerge, and which tools act as bridges across regimes?
- **RQ4 ??? Impact Profiles:** What stable multivariate ???impact profiles??? emerge from clustering across impact metrics?

## Features (What the App Provides)
- Global interactive filters (year range, countries, industries, regulation status)
- **RQ1:** Choropleth map + neighbor comparison (boxplot) + scatter + details table
- **RQ2:** Time-series trend + hotspot detection + hotspot map + industry facets + table
- **RQ3:** Tool ecosystem network (networkD3) + tool performance scatter + prevalence chart + bridge table
- **RQ4:** Clustering (k-means / hierarchical) + parallel coordinates + UMAP embedding + cluster map + centroids + exemplars
- Data Explorer with filtering and CSV download

## Folder Structure (Required)
Place files exactly as follows:

DV_Project/
app.R
README.md
data/
Global_AI_Content_Impact_Dataset.csv
## Dataset Requirements
The application expects a CSV file located at:

`data/Global_AI_Content_Impact_Dataset.csv`

The dataset should contain columns corresponding to:
- Country, Year, Industry
- Adoption, Content Volume, Job Loss, Revenue Increase, Collaboration, Trust, Market Share
- Top AI Tools Used, Regulation Status

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
