# ====================================================
# 0. CHARGEMENT DES BIBLIOTHÈQUES --------------------
# ====================================================

library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)


# ====================================================
# 1. CHARGEMENT ET SÉLECTION DES DONNÉES -------------
# ====================================================

# Définir le chemin du fichier
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Master 2/Projet ML/data")

# Importation avec les paramètres spécifiques
df <- read_delim(
  "data.csv",
  delim = ";",
  col_names = TRUE,
  col_types = cols(.default = "c"),
  locale = locale(encoding = "UTF-8"),
  guess_max = 10000
)

# Vérification des noms des colonnes
print(colnames(df))
cat("✅ Nombre de colonnes importées :", ncol(df), "\n")


# Sélection des variables pertinentes
df <- df %>%
  select(
    # Identification
    DisNo.,
    
    # Classification
    'Disaster Group', 'Disaster Subgroup', 'Disaster Type', 'Disaster Subtype',
    
    # Détails de l'Événement
    'Event Name',
    
    # Géographie
    ISO, Country, Subregion, Region, Location,
    
    # Types Associés
    'Associated Types',
    
    # Caractéristiques Techniques
    Magnitude, 'Magnitude Scale',
    
    # Coordonnées Géographiques
    Latitude, Longitude,
    
    # Temporalité
    'Start Year', 'Start Month', 'Start Day', 'End Year', 'End Month', 'End Day',
    
    # Impacts Humains
    'Total Deaths', 'No. Injured', 'No. Affected', 'No. Homeless', 'Total Affected',
    
    # Impacts Financiers
    "Reconstruction Costs ('000 US$)", "Reconstruction Costs, Adjusted ('000 US$)",
    "Insured Damage ('000 US$)", "Insured Damage, Adjusted ('000 US$)",
    "Total Damage ('000 US$)", "Total Damage, Adjusted ('000 US$)",
    
    # Économie
    CPI
  )

# Validation de la sélection
cat("✅ Nombre final de colonnes après sélection :", ncol(df), "\n")
print(colnames(df))

# ====================================================
# 2. RENOMMAGE DES VARIABLES -------------------------
# ====================================================

df <- df %>%
  rename(
    # Identification
    disaster_id = DisNo.,
    
    # Classification
    disaster_group = `Disaster Group`,
    disaster_subgroup = `Disaster Subgroup`,
    disaster_type = `Disaster Type`,
    disaster_subtype = `Disaster Subtype`,
    
    # Détails de l'Événement
    event_name = `Event Name`,
    
    # Géographie
    iso_code = ISO,
    country = Country,
    subregion = Subregion,
    region = Region,
    location = Location,
    
    # Types Associés
    associated_types = `Associated Types`,
    
    # Caractéristiques Techniques
    magnitude = Magnitude,
    magnitude_scale = `Magnitude Scale`,
    
    # Coordonnées Géographiques
    latitude = Latitude,
    longitude = Longitude,
    
    # Temporalité
    start_year = `Start Year`,
    start_month = `Start Month`,
    start_day = `Start Day`,
    end_year = `End Year`,
    end_month = `End Month`,
    end_day = `End Day`,
    
    # Impacts Humains
    total_deaths = `Total Deaths`,
    number_injured = `No. Injured`,
    number_affected = `No. Affected`,
    number_homeless = `No. Homeless`,
    total_affected = `Total Affected`,
    
    # Impacts Financiers
    reconstruction_costs = `Reconstruction Costs ('000 US$)`,
    reconstruction_costs_adjusted = `Reconstruction Costs, Adjusted ('000 US$)`,
    insured_damage = `Insured Damage ('000 US$)`,
    insured_damage_adjusted = `Insured Damage, Adjusted ('000 US$)`,
    total_damage = `Total Damage ('000 US$)`,
    total_damage_adjusted = `Total Damage, Adjusted ('000 US$)`,
    
    # Économie
    cpi = CPI
  )


# ====================================================
# 3. CONVERSION DES TYPES DE VARIABLES ---------------
# ====================================================

df <- df %>%
  mutate(
    # Identification
    disaster_id = as.character(disaster_id),
    
    # Classification (catégorielles)
    disaster_group = as.factor(disaster_group),
    disaster_subgroup = as.factor(disaster_subgroup),
    disaster_type = as.factor(disaster_type),
    disaster_subtype = as.factor(disaster_subtype),
    event_name = as.character(event_name),
    
    # Géographie (catégorielles)
    iso_code = as.factor(iso_code),
    country = as.factor(country),
    subregion = as.factor(subregion),
    region = as.factor(region),
    location = as.character(location),
    associated_types = as.character(associated_types),
    
    # Caractéristiques Techniques (numériques)
    magnitude = as.numeric(magnitude),
    magnitude_scale = as.character(magnitude_scale),
    
    # Coordonnées Géographiques (numériques)
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    
    # Temporalité (entiers)
    start_year = as.integer(start_year),
    start_month = as.integer(start_month),
    start_day = as.integer(start_day),
    end_year = as.integer(end_year),
    end_month = as.integer(end_month),
    end_day = as.integer(end_day),
    
    # Impacts Humains (entiers)
    total_deaths = as.integer(total_deaths),
    number_injured = as.integer(number_injured),
    number_affected = as.integer(number_affected),
    number_homeless = as.integer(number_homeless),
    total_affected = as.integer(total_affected),
    
    # Impacts Financiers (numériques)
    reconstruction_costs = as.numeric(reconstruction_costs),
    reconstruction_costs_adjusted = as.numeric(reconstruction_costs_adjusted),
    insured_damage = as.numeric(insured_damage),
    insured_damage_adjusted = as.numeric(insured_damage_adjusted),
    total_damage = as.numeric(total_damage),
    total_damage_adjusted = as.numeric(total_damage_adjusted),
    
    # Économie (numérique)
    cpi = as.numeric(cpi)
  )

# ====================================================
# 4. GESTION DES VALEURS MANQUANTES ------------------
# ====================================================

# Compter les valeurs manquantes
colSums(is.na(df))

# Calcul du pourcentage de valeurs manquantes
missing_percentage <- colSums(is.na(df)) / nrow(df) * 100

# Afficher les variables avec leur pourcentage de valeurs manquantes
missing_percentage_sorted <- sort(missing_percentage, decreasing = TRUE)
print(missing_percentage_sorted)

# Supprimer les variables avec plus de 50% de valeurs manquantes
df <- df %>%
  select(where(~ mean(is.na(.)) <= 0.5))

# Validation des colonnes restantes
cat("✅ Nombre total de colonnes après suppression :", ncol(df), "\n")
print(colnames(df))

# Remplacer les NA dans location par "Inconnu"
df <- df %>%
  mutate(location = ifelse(is.na(location), "Inconnu", location))

# Remplacer les NA dans magnitude_scale par "Non applicable" pour les sous-types identifiés
df <- df %>%
  mutate(
    magnitude_scale = ifelse(
      is.na(magnitude_scale) & disaster_subtype %in% c(
        "Bacterial disease", "Landslide (wet)", "Ash fall", "Avalanche (wet)",
        "Mudslide", "Locust infestation", "Landslide (dry)", "Grasshopper infestation",
        "Lava flow", "Volcanic activity (General)", "Infestation (General)", "Rockfall (dry)", 
        "Glacial lake outburst flood", "Avalanche (dry)", "Rockfall (wet)", "Pyroclastic flow", 
        "Worms infestation", "Animal incident", "Collision", "Sudden Subsidence (dry)", "Sudden Subsidence (wet)",
        "Lahar"
      ),
      "Non applicable",
      magnitude_scale
    )
  )


# Certaines catastrophes n'ont pas de mesure quantitative pertinente pour une échelle de magnitude.

# Vérifier les NA restants après traitement
colSums(is.na(df))

# ====================================================
# 5. TRAITEMENT DES DATES ET DURÉES ------------------
# ====================================================

# Vérification des valeurs manquantes restantes
colSums(is.na(df))

# Création de la variable 'year' à partir de start_year, sinon end_year
df <- df %>%
  mutate(
    year = ifelse(!is.na(start_year), start_year, end_year)
  )

# Vérification des valeurs manquantes dans 'year'
sum(is.na(df$year))

# Création de start_date et end_date au format d/m/y
df <- df %>%
  mutate(
    start_date = ifelse(
      !is.na(start_year) & !is.na(start_month) & !is.na(start_day),
      paste0(sprintf("%02d", start_day), "/", sprintf("%02d", start_month), "/", start_year),
      NA
    ),
    end_date = ifelse(
      !is.na(end_year) & !is.na(end_month) & !is.na(end_day),
      paste0(sprintf("%02d", end_day), "/", sprintf("%02d", end_month), "/", end_year),
      NA
    ),
    start_date = dmy(start_date),
    end_date = dmy(end_date),
    event_duration = ifelse(
      !is.na(start_date) & !is.na(end_date),
      as.numeric(difftime(end_date, start_date, units = "days")),
      NA
    )
  )

# Suppression des variables redondantes
df <- df %>%
  select(
    -start_day, -start_month, -start_year,
    -end_day, -end_month, -end_year
  )

# ====================================================
# 6. SUPPRESSION DES VARIABLES INUTILES---------------
# ====================================================

df <- df %>%
  select(
    -disaster_id,     # Identifiant unique non nécessaire pour l'analyse
    -disaster_group,  # Toujours "Natural", pas d'information supplémentaire       # Redondant avec la variable 'country'
    -number_affected  # Redondant avec 'total_affected'
  )

# ====================================================
# 7. SAUVEGARDE DES DONNÉES NETTOYÉES ----------------
# ====================================================

# 📦 Charger les bibliothèques nécessaires
library(dplyr)

# 🔍 Filtrage des années supérieures à 1990
df_filtered <- df %>%
  filter(year > 2000)

# 📊 Afficher les premières lignes du nouveau dataframe
print(head(df_filtered))

# 🔍 Calcul du pourcentage de valeurs manquantes par variable
missing_values <- data.frame(
  Variable = names(df_filtered),
  Pourcentage_Manquant = round(colMeans(is.na(df_filtered)) * 100, 2)
)

# 📊 Affichage en table dans la console
print(missing_values, row.names = FALSE)


# Définir le répertoire de sortie
# setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Master 2/Projet ML/output")
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Master 2/Projet ML/EM-DAT-Explorer")

# Créer le dossier s'il n'existe pas
if (!dir.exists(getwd())) {
  dir.create(getwd(), recursive = TRUE)
}

# Sauvegarde des données nettoyées au format RDS et CSV
saveRDS(df, "data_cleaned_final.rds")
write_csv(df, "data_cleaned_final.csv")

# Message de confirmation
cat("✅ Les données nettoyées ont été sauvegardées avec succès.\n")
