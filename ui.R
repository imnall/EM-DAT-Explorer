# ====================================================
# 🎨 UI.R - Interface Utilisateur du Tableau de Bord
# ====================================================

# 📦 CHARGEMENT DES LIBRAIRIES -----------------------
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(leaflet)
# ====================================================
# 📊 INTERFACE UTILISATEUR ---------------------------
shinyUI(
  fluidPage(
    theme = shinytheme("flatly"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    # 🏷️ En-tête
    titlePanel(
      title = "🌍 EM-DAT Explorer",
      windowTitle = "EM-DAT Explorer"
    ),
    
    # 🚀 Filtres Horizontaux
    fluidRow(
      column(2, pickerInput("region", "🌍 Région", choices = c("Toutes", sort(unique(df_analysis$region))), selected = "Toutes", options = list(`live-search` = TRUE))),
      column(2, pickerInput("country", "🏳️ Pays", choices = c("Tous", sort(unique(df_analysis$country))), selected = "Tous", options = list(`live-search` = TRUE))),
      column(2, pickerInput("disaster_subgroup", "🌪 Groupe de Catastrophe", choices = c("Tous", sort(unique(df_analysis$disaster_subgroup))), selected = "Tous", options = list(`live-search` = TRUE))),
      column(2, pickerInput("disaster_type", "🔥 Type de Catastrophe", choices = c("Tous", sort(unique(df_analysis$disaster_type))), selected = "Tous", options = list(`live-search` = TRUE))),
      column(2, pickerInput("year", "📅 Année", choices = c("Toutes", sort(unique(na.omit(df_analysis$year)))), selected = "Toutes",   multiple = TRUE, options = list(`live-search` = TRUE))),
      column(2, div(style = "margin-top: 25px;", actionButton("reset_filters", "🔄 Réinitialiser", class = "btn-primary btn-block")))
    ),
    hr(),
    # 📊 Indicateurs Clés (KPIs)
    fluidRow(
      column(
        2,
        h4("📊 Indicateurs Clés", style = "font-weight: bold; text-align: center;"),
        hr(),
        div(
          class = "kpi-box",
          wellPanel(
            h5("📈 Total des Catastrophes", style = "font-weight: bold; text-align: center;"),
            h3(textOutput("total_disasters"), style = "text-align: center;"),
            icon("exclamation-triangle", style = "font-size: 3em; color: #0078D7; display: block; margin: auto;")
          )
        ),
        div(
          class = "kpi-box",
          wellPanel(
            h5("💀 Total des Décès", style = "font-weight: bold; text-align: center;"),
            h3(textOutput("total_deaths"), style = "text-align: center;"),
            icon("heart-broken", style = "font-size: 3em; color: #D9534F; display: block; margin: auto;")
          )
          
        ),
        div(
          class = "kpi-box",
          wellPanel(
            h5("👥 Total Personnes Affectées", style = "font-weight: bold; text-align: center;"),
            h3(textOutput("total_affected"), style = "text-align: center;"),
            icon("users", style = "font-size: 3em; color: #5BC0DE; display: block; margin: auto;")
          )
        ),
        div(
          class = "kpi-box",
          wellPanel(
            h5("⏳ Durée Moyenne", style = "font-weight: bold; text-align: center;"),
            h3(textOutput("avg_duration"), style = "text-align: center;"),
            icon("clock", style = "font-size: 3em; color: #5BC0DE; display: block; margin: auto;")
          )
        )
      ),
      
      # 🎯 Section principale avec graphiques et tableaux
      column(
        10,
        tabsetPanel(
          type = "tabs",
          
          # 📊 Onglet Aperçu Global
          tabPanel(
            "📊 Aperçu Global",
            fluidRow(
              column(
                6,
                h4("🌍 Top 5 Pays par Décès et Nombre de Catastrophes"),
                div(
                  class = "styled-table overview-table",
                  DT::dataTableOutput("top5_countries")
                )
              ),
              column(
                6,
                h4("🏆 Top 5 Catastrophes par Décès et Personnes Affectées"),
                div(
                  class = "styled-table overview-table", 
                  DT::dataTableOutput("top5_disasters")
                )
              )
            ),
            fluidRow(
              column(
                6,
                div(class = "plot-box",
                    h4("📊 Distribution des Décès par Type de Catastrophe"),
                    plotlyOutput("plot_overview", height = "300px")
                )
              ),
              column(
                6,
                div(class = "plot-box",
                    h4("📊 Évolution de la Durée Moyenne des Catastrophes Naturelles"),
                    plotlyOutput("plot_extra", height = "300px")
                )
              ),
              
            )
          ),
          
          # 📆 Onglet Analyse Temporelle
          tabPanel(
            "📆 Analyse Temporelle",
            fluidRow(
              column(6,
                     div(class = "plot-box",
                         h4("📈 Évolution du Nombre d'Événements"),
                         plotlyOutput("plot_time_events", height = "250px")
                     )
              ),
              column(6,
                     div(class = "plot-box",
                         h4("💀 Évolution du Nombre de Décès"),
                         plotlyOutput("plot_time_deaths", height = "250px")
                     )
              )
            ),
            fluidRow(
              column(12,
                     sliderInput("time_range",
                                 "Période d'analyse temporelle",
                                 min = 1980,
                                 max = 2024,
                                 value = c(1980, 2024),
                                 step = 1,
                                 sep = "",
                                 width = "100%"
                     )
              )
            ),
            fluidRow(
              column(12,
                     div(class = "styled-table details-table",
                         style = "height: calc(100vh - 650px); overflow-y: auto; margin-top: -20px;",
                         h4("📋 Détails des Catastrophes"),
                         DT::dataTableOutput("details_table")
                     )
              )
            )
          ),
          
          # 🌍 Onglet Analyse Régionale
          # Dans UI.R, onglet Analyse Régionale
          tabPanel(
            "🌍 Analyse Régionale",
            tags$div(
              style = "position: relative; height: calc(100vh - 250px);",  # Conteneur principal avec hauteur fixe
              
              # Conteneur de la carte
              tags$div(
                style = "position: absolute; top: 0; width: 100%; height: 500px;",
                div(class = "map-box",
                    leafletOutput("region_map", height = "500px")
                )
              ),
              
              # Conteneur du slider
              tags$div(
                style = "position: absolute; bottom: 0; width: 100%;",
                div(class = "slider-box",
                    sliderInput("year_range",
                                "Période d'analyse",
                                min = 1980,
                                max = 2024,
                                value = c(1980, 2024),
                                step = 1,
                                sep = "",
                                width = "100%"
                    )
                )
              )
            )
          ),
          
          tabPanel(
            "📋 Données",
            div(class = "styled-table details-table",
                DT::dataTableOutput("full_data_table")
            )
          ),

          # ℹ️ Onglet À Propos
          tabPanel(
            "ℹ️ À Propos",
            fluidRow(
              column(12,
                     h4("🌟 Fonctionnalités principales"),
                     tags$ul(
                       tags$li("🔍 Filtrage multi-critères des données"),
                       tags$li("📊 Visualisations interactives"),
                       tags$li("🌍 Analyse géographique"),
                       tags$li("📈 Suivi temporel"),
                       tags$li("📋 Vue détaillée des données")
                     ),
                     h4("📚 Guide d'utilisation"),
                     tags$ul(
                       tags$li("Utilisez les filtres en haut pour affiner l'analyse"),
                       tags$li("Naviguez entre les onglets pour différentes perspectives"),
                       tags$li("Interagissez avec les graphiques pour plus de détails")
                     ),
                     h4("ℹ️ À propos des données"),
                     p("Base EM-DAT : Emergency Events Database du Centre de Recherche sur l'Épidémiologie des Désastres (CRED)"),
                     p("Période couverte : 1980-2024"),
                     tags$a(href="https://www.emdat.be", "Accéder au site EM-DAT", target="_blank"),
                     hr(),
                     p("💻 Développé par : W.O.A.H."),
                     p("📊 Version : 1.0")
              )
            )
          )
        )
      )
    ),
    
    # 📄 Pied de page
    fluidRow(
      column(
        12,
        p("📊 Dashboard interactif | 🌐 Source : EM-DAT (1980-2024) | ✍️ Réalisé par W.O.A.H.]",
          style = "text-align: center; font-size: 0.8em; color: grey;")
      )
    )
  )
)