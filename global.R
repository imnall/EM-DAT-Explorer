# global.R
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(viridis)

# Chargement des données
df_analysis <- readRDS("data/data_cleaned_final.rds")

# Transformation des variables
df_analysis <- df_analysis %>%
  mutate(across(where(is.factor), as.character))

# Correction de l'encodage UTF-8
df_analysis$country <- iconv(df_analysis$country, from = "UTF-8", to = "UTF-8", sub = "")
