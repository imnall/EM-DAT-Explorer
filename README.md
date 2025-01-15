# EM-DAT Explorer

Interactive dashboard analyzing global natural disasters (1980-2024) using EM-DAT database.

## Quick Start
```r
install.packages(c("shiny", "shinythemes", "shinyWidgets", "leaflet", "plotly", "DT", "dplyr", "sf", "rnaturalearth"))
shiny::runApp()
```

## Features
- 📊 Real-time KPIs and data visualization
- 🌍 Interactive world map with disaster impact zones
- 📈 Temporal analysis and trend tracking
- 🔍 Multi-criteria filtering

## Structure
```
├── app.R
├── server.R
├── ui.R
├── www/
│   └── style.css
├── data/
│   └── data_cleaned_final.rds
└── data_preprocessing.R
```

## Data Source
Based on [EM-DAT](https://www.emdat.be) (The International Disaster Database) from the Centre for Research on the Epidemiology of Disasters (CRED). 

[Access EM-DAT Database](https://public.emdat.be)

## Developed by
W.O.A.H.

---
**Note**: This project uses EM-DAT data for educational and research purposes. Visit [EM-DAT Documentation](https://www.emdat.be/explanatory-notes) for database details.
