# ====================================================
# 🛠️ SERVER.R - Logique Serveur du Tableau de Bord-----------------------------
# ====================================================

# 📦 CHARGEMENT DES LIBRAIRIES
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(DT) # Pour les tables interactives
library(plotly)

# 🗃️ IMPORTATION DE LA BASE DE DONNÉES ---------------
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Master 2/Projet ML/EM-DAT-Explorer")

df_analysis <- readRDS("data_cleaned_final.rds")

# 🔄 Transformation des variables

# Transformer les facteurs (factor) en chaînes de caractères (character)
df_analysis <- df_analysis %>%
  mutate(across(where(is.factor), as.character))

# Correction de l'encodage UTF-8
df_analysis$country <- iconv(df_analysis$country, from = "UTF-8", to = "UTF-8", sub = "")

# ====================================================
# 🔄 LOGIQUE SERVEUR------------------------------------------------------------
# ====================================================
shinyServer(function(input, output, session) {
  
  # ====================================================
  # 🔍 1. FILTRES DYNAMIQUES----------------------------------------------------
  # ====================================================
  observeEvent(input$reset_filters, {
    updatePickerInput(session, "region", selected = "Toutes")
    updatePickerInput(session, "disaster_subgroup", selected = "Tous")
    updatePickerInput(session, "disaster_type", selected = "Tous")
    updatePickerInput(session, "country", selected = "Tous")
    updatePickerInput(session, "year", selected = "Toutes")
    updateSliderInput(session, "time_range", value = c(1980, 2024))
    updateSliderInput(session, "year_range", value = c(1980, 2024))
  })
  
  # ====================================================
  # 📊 2. FILTRAGE DES DONNÉES--------------------------------------------------
  # ====================================================
  filtered_data <- reactive({
    data <- df_analysis
    
    if (input$region != "Toutes") {
      data <- data %>% filter(region == input$region)
    }
    if (input$disaster_subgroup != "Tous") {
      data <- data %>% filter(disaster_subgroup == input$disaster_subgroup)
    }
    if (input$disaster_type != "Tous") {
      data <- data %>% filter(disaster_type == input$disaster_type)
    }
    if (input$country != "Tous") {
      data <- data %>% filter(country == input$country)
    }
    if (!("Toutes" %in% input$year)) {
      data <- data %>% filter(year %in% input$year)
    }
    
    return(data)
  })
  
  # ====================================================
  # 📊 3. INDICATEURS CLÉS (KPI)------------------------------------------------
  # ====================================================
  output$total_disasters <- renderText({
    filtered_data() %>%
      filter(year >= input$time_range[1], year <= input$time_range[2]) %>%
      nrow() %>%
      format(big.mark = " ")
  })
  
  output$total_deaths <- renderText({
    filtered_data() %>%
      filter(year >= input$time_range[1], year <= input$time_range[2]) %>%  
      summarise(total = sum(total_deaths, na.rm = TRUE)) %>%
      pull(total) %>%
      {if(is.na(.)) "0" else format(., big.mark = " ")}
  })
  
  output$avg_duration <- renderText({
    filtered_data() %>%
      filter(year >= input$time_range[1], year <= input$time_range[2]) %>%
      summarise(avg = mean(event_duration, na.rm = TRUE)) %>%
      pull(avg) %>%
      {if(is.na(.)) "0" else round(.)}
  })
  
  output$total_affected <- renderText({
    filtered_data() %>%
      filter(year >= input$time_range[1], year <= input$time_range[2]) %>%
      summarise(total = sum(total_affected, na.rm = TRUE)) %>%
      pull(total) %>%
      {if(is.na(.)) "0" else format(., big.mark = " ")}
  })
  
  # ====================================================
  # 📊 4. GRAPHIQUE - ANALYSE DES CATASTROPHES PAR SOUS-GROUPE------------------
  # ====================================================
  library(plotly)
  
  output$plot_overview <- renderPlotly({
   data <- filtered_data() %>%
     group_by(disaster_subgroup) %>%
     summarise(
       total_deaths = sum(total_deaths, na.rm = TRUE),
       avg_duration = mean(event_duration, na.rm = TRUE)
     )
   
   p <- ggplot(data, aes(x = reorder(disaster_subgroup, -total_deaths))) +
     geom_bar(aes(y = total_deaths, fill = disaster_subgroup, 
                  text = paste("Décès:", formatC(total_deaths, format="f", big.mark=" ", digits=0))), 
              stat = "identity") +
     scale_y_continuous(
       name = "Nombre total de décès",
       sec.axis = sec_axis(~./5000, name = "Durée moyenne (jours)"),
       labels = scales::comma_format(big.mark = " ")
     ) +
     scale_fill_brewer(palette = "Set2") +
     labs(
       x = "Sous-Groupe",
       fill = "Type de catastrophe"
     ) +
     theme_minimal() +
     theme(
       text = element_text(color = "white"),
       axis.text = element_text(color = "white"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       axis.title = element_text(color = "white"),
       legend.position = "none",
       panel.grid = element_line(color = "rgba(255,255,255,0.1)")
     ) +
     labs(x = "Sous-Groupe", y = "Nombre de décès")
   
   ggplotly(p, tooltip = "text") %>%
     layout(
       plot_bgcolor = '#2C2C3C',
       paper_bgcolor = '#2C2C3C',
       font = list(color = 'white'),
       margin = list(t = 30, r = 30, b = 80, l = 80)
     )
})
  
  # ====================================================
  # 🏆 5. TABLEAU INTERACTIF - TOP 5 TYPES DE CATASTROPHES PAR DÉCÈS------------
  # ====================================================
  output$top5_disasters <- DT::renderDT({
    data <- filtered_data() %>%
      group_by(disaster_type) %>%
      summarise(
        `Nombre Total de Décès` = sum(total_deaths, na.rm = TRUE),
        `Personnes Affectées` = sum(total_affected, na.rm = TRUE)
      ) %>%
      arrange(desc(`Nombre Total de Décès`)) %>%
      head(5)
    
    DT::datatable(
      data,
      colnames = c("Type de Catastrophe", "Nombre Total de Décès", "Personnes Affectées"),
      options = list(
        pageLength = 5,
        dom = 't',
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(
        c('Nombre Total de Décès', 'Personnes Affectées'),
        currency = "",
        digits = 0,
        mark = " "
      )
  })
  
  # ====================================================
  # 🌍 6. TABLEAU INTERACTIF - TOP 5 PAYS LES PLUS IMPACTÉS---------------------
  # ====================================================
  output$top5_countries <- DT::renderDT({
    data <- filtered_data() %>%
      group_by(country) %>%
      summarise(
        `Nombre Total de Décès` = sum(total_deaths, na.rm = TRUE),
        `Nombre de Catastrophes` = n()
      ) %>%
      arrange(desc(`Nombre Total de Décès`)) %>% 
      head(5)
    
    DT::datatable(
      data,
      colnames = c("Pays", "Nombre Total de Décès", "Nombre de Catastrophes"),
      options = list(
        pageLength = 5,
        dom = 't',
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(
        c('Nombre Total de Décès', 'Nombre de Catastrophes'),
        currency = "",
        digits = 0,
        mark = " "
      )
  })
  
  # ====================================================
  # 📊 7. VISUALISATION COMPLÉMENTAIRE------------------------------------------
  # ====================================================
  output$plot_extra <- renderPlotly({
    data <- filtered_data() %>%
      group_by(disaster_subgroup) %>%
      summarise(
        total_deaths = sum(total_deaths, na.rm = TRUE),
        avg_duration = mean(event_duration, na.rm = TRUE),
        min_duration = min(event_duration, na.rm = TRUE),
        max_duration = max(event_duration, na.rm = TRUE),
        median_duration = median(event_duration, na.rm = TRUE)
      )
    
    p <- ggplot(data, aes(x = reorder(disaster_subgroup, -total_deaths))) +
      geom_line(aes(y = avg_duration, group = 1), 
                color = "#e63946", size = 1.2) +
      geom_point(aes(y = avg_duration, 
                     text = paste("Durée moyenne:", round(avg_duration, 1), "jours",
                                  "<br>Médiane:", round(median_duration, 1), "jours",
                                  "<br>Min:", round(min_duration, 1), "jours",
                                  "<br>Max:", round(max_duration, 1), "jours")), 
                 color = "#e63946", size = 3) +
      scale_y_continuous(
        name = "Durée (jours)",
        labels = scales::comma_format(big.mark = " ")
      ) +
      labs(
        x = "Sous-Groupe"
      ) +
      theme_minimal() +
      theme(
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(color = "white"),
        legend.position = "none",
        panel.grid = element_line(color = "rgba(255,255,255,0.1)")
      ) +
      labs(x = "Sous-Groupe", y = "Nombre de décès")
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        plot_bgcolor = '#2C2C3C',
        paper_bgcolor = '#2C2C3C',
        font = list(color = 'white'),
        margin = list(t = 30, r = 30, b = 80, l = 80)
      )
  })
  
  # ====================================================
  # 📊 8. VISUALISATION Temporelle ------------------------------------------
  # ====================================================
  # Graphique des événements ---------------------------
  output$plot_time_events <- renderPlotly({
    data_time <- filtered_data() %>%
      filter(year >= input$time_range[1], year <= input$time_range[2]) %>%
      group_by(year) %>%
      summarise(nb_events = n())
    
    plot_ly(data_time, x = ~year, y = ~nb_events) %>%
      add_lines(
        line = list(color = "#1976D2", width = 3),
        hovertemplate = "Année: %{x}<br>Événements: %{y}<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "Année", gridcolor = 'rgba(255,255,255,0.1)'),
        yaxis = list(title = "Nombre d'événements", gridcolor = 'rgba(255,255,255,0.1)'),
        plot_bgcolor = '#2C2C3C',
        paper_bgcolor = '#2C2C3C',
        font = list(color = 'white')
      )
  })
  
  # Graphique des décès ---------------------------
  output$plot_time_deaths <- renderPlotly({
    data_time <- filtered_data() %>%
      filter(year >= input$time_range[1], year <= input$time_range[2]) %>%
      group_by(year) %>%
      summarise(total_deaths = sum(total_deaths, na.rm = TRUE))
    
    plot_ly(data_time, x = ~year, y = ~total_deaths) %>%
      add_lines(
        line = list(color = "#e63946", width = 3),
        hovertemplate = "Année: %{x}<br>Décès: %{y}<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "Année", gridcolor = 'rgba(255,255,255,0.1)'),
        yaxis = list(title = "Nombre de décès", gridcolor = 'rgba(255,255,255,0.1)'),
        plot_bgcolor = '#2C2C3C',
        paper_bgcolor = '#2C2C3C',
        font = list(color = 'white')
      )
  })
  
  #Table Analyse Temporelle ---------------------------
  output$details_table <- DT::renderDT({
    data <- filtered_data() %>%
      filter(year >= input$time_range[1], year <= input$time_range[2]) %>%
      mutate(
        `Année` = year,
        `Pays` = country, 
        `Type de Catastrophe` = disaster_type,
        `Nombre de Décès` = ifelse(is.na(total_deaths), 0, total_deaths),
        `Personnes Affectées` = ifelse(is.na(total_affected), 0, total_affected)
      ) %>%
      select(`Année`, `Pays`, `Type de Catastrophe`, `Nombre de Décès`, `Personnes Affectées`)
    
    DT::datatable(
      data,
      options = list(
        pageLength = 5,
        dom = 't',
        autoWidth = TRUE,
        scrollY = "calc(100vh - 750px)",
        scrollCollapse = TRUE,
        paging = FALSE
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(
        c('Nombre de Décès', 'Personnes Affectées'),
        currency = "", 
        digits = 0,
        mark = " "
      )
  })
  
  # ====================================================
  # 🌍 9. VISUALISATION Régionale ------------------------------------------
  # ====================================================
  library(sf)
  library(rnaturalearth)
  library(leaflet)
  library(viridis)
  library(dplyr)
  library(scales)
  
  output$region_map <- renderLeaflet({
    data <- filtered_data() %>%
      filter(
        !is.na(country),
        year >= input$year_range[1],
        year <= input$year_range[2]
      ) %>%
      group_by(country) %>%
      summarise(
        total_disasters = n(),
        total_deaths = sum(total_deaths, na.rm = TRUE),
        total_affected = sum(total_affected, na.rm = TRUE),
        total_impact = total_deaths + total_affected,
        # Trouve la catastrophe la plus impactante
        main_disaster = first(disaster_type[which.max(total_deaths + total_affected)])
      )
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world_data <- world %>%
      left_join(data, by = c("name" = "country"))
    
    death_points <- st_centroid(world_data) %>%
      filter(!is.na(total_impact), total_impact > 100000)
    
    pal <- colorNumeric("viridis", domain = world_data$total_disasters, na.color = "#808080")
    
    map <- leaflet(world_data) %>%
      addTiles() %>%
      setView(0, 20, 2) %>%
      addPolygons(
        fillColor = ~pal(total_disasters),
        weight = 1,
        color = "#333333",
        fillOpacity = 0.7
      )
    
    if(nrow(death_points) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = death_points,
          radius = ~sqrt(total_impact/100000),
          color = "red",
          fillOpacity = 0.6,
          stroke = TRUE,
          weight = 1,
          label = ~paste0(
            name, "<br>",
            "Période: ", input$year_range[1], "-", input$year_range[2], "<br>",
            "Catastrophe principale: ", main_disaster, "<br>",
            "Décès: ", formatC(total_deaths, format="f", big.mark=" ", digits=0), "<br>",
            "Personnes affectées: ", formatC(total_affected, format="f", big.mark=" ", digits=0),
            "<br>Impact total: ", formatC(total_impact, format="f", big.mark=" ", digits=0)
          ) %>% lapply(htmltools::HTML)
        )
    }
    
    map %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~total_disasters,
        title = "Nombre de catastrophes",
        na.label = "Indisponible"
      ) %>%
      addLegend(
        position = "bottomleft",
        colors = "red",
        labels = "Impact total (Décès + Affectés)",
        opacity = 0.6,
        title = "Impact humain"
      )
  })
  
  # ====================================================
  # 📄 10. VISUALISATION DATATABLE ------------------------------------------
  # ====================================================
  output$full_data_table <- DT::renderDT({
    DT::datatable(
      filtered_data() %>%
        select(
          `Année` = year,
          `Pays` = country,
          `Région` = region,
          `Groupe de CAT` = disaster_subgroup,
          `Type de CAT` = disaster_type,
          `Sous-type de CAT` = disaster_subtype,
          `Décès` = total_deaths,
          `Personnes Affectées` = total_affected,
          `Durée (jours)` = event_duration
        ),
      options = list(
        pageLength = 15,
        scrollY = "calc(100vh - 300px)",
        scrollX = FALSE,
        scrollCollapse = TRUE,
        scroller = TRUE,
        dom = 'lrtip',
        order = list(1, 'desc'),
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    ) %>%
      formatCurrency(
        c('Décès', 'Personnes Affectées'),
        currency = "",
        digits = 0,
        mark = " "
      )
  })
  
})