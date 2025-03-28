library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(glue)

# Chargement des données
nba_games <- read_csv("nba_games_2023_2024.csv") %>%
  mutate(Date = as.Date(Date))

player_stats <- read_csv("nba_player_stats_2023_2024.csv") %>%
  filter(MP >= 15) %>%
  mutate(
    PPG = PTS,
    APG = AST,
    RPG = TRB,
    `FG%` = `FG%` * 100,
    Position = case_when(
      Pos == "PG" ~ "Meneur",
      Pos == "SG" ~ "Arrière",
      Pos == "SF" ~ "Ailier",
      Pos == "PF" ~ "Ailier fort",
      Pos == "C" ~ "Pivot",
      TRUE ~ Pos
    )
  )

# Fonction pour calculer le classement
calculate_standings <- function(games_df) {
  all_teams <- unique(c(games_df$Home, games_df$Visitor))
  
  standings <- tibble(
    Team = all_teams,
    Wins = sapply(all_teams, function(x) sum(games_df$Winner == x)),
    Losses = sapply(all_teams, function(x) sum(games_df$Home == x | games_df$Visitor == x) - sum(games_df$Winner == x)),
    Home_Wins = sapply(all_teams, function(x) sum(games_df$Home == x & games_df$Winner == x)),
    Away_Wins = sapply(all_teams, function(x) sum(games_df$Visitor == x & games_df$Winner == x))
  ) %>%
    mutate(
      Win_Pct = Wins / (Wins + Losses),
      Games_Played = Wins + Losses
    ) %>%
    arrange(desc(Win_Pct))
  
  return(standings)
}

standings <- calculate_standings(nba_games)

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "NBA 2023-2024 - Tableau de Bord"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Guide", tabName = "guide", icon = icon("info-circle")),
      menuItem("Statistiques Équipes", tabName = "teams", icon = icon("users")),
      menuItem("Analyse Joueurs", tabName = "players", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      # Onglet Guide
      tabItem(
        tabName = "guide",
        fluidRow(
          box(
            title = "NBA Dashboard 2023-2024",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            tags$div(
              tags$div(
                style = "display: flex; align-items: center; margin-bottom: 20px;",
                tags$img(src = "nba_logo.png", height = "80px"),
                tags$h2("Guide d'Utilisation", style = "margin-left: 20px; margin-bottom: 0;")
              ),
              tags$div(
                style = "text-align: left;",
                tags$p("Utilisez le menu de gauche pour naviguer entre les différentes sections."),
                tags$h3("Statistiques Équipes"),
                tags$p("Analyse des performances collectives avec :"),
                tags$ul(
                  tags$li("Classement général"),
                  tags$li("Détail par équipe"),
                  tags$li("Performances à domicile/extérieur")
                ),
                tags$h3("Analyse Joueurs"),
                tags$p("Comparaison des performances individuelles avec :"),
                tags$ul(
                  tags$li("Top marqueurs"),
                  tags$li("Top défenseurs"),
                  tags$li("Filtres avancés")
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Glossaire",
            width = 12,
            tags$div(
              style = "text-align: left;",
              tags$h3("Termes techniques"),
              tags$h4("Pour les équipes :"),
              tags$ul(
                tags$li(tags$b("Win% :"), "Pourcentage de victoires"),
                tags$li(tags$b("PtsSc :"), "Points marqués en moyenne"),
                tags$li(tags$b("PtsAl :"), "Points encaissés en moyenne")
              ),
              tags$h4("Pour les joueurs :"),
              tags$ul(
                tags$li(tags$b("PPG :"), "Points par match"),
                tags$li(tags$b("APG :"), "Passes décisives par match"),
                tags$li(tags$b("RPG :"), "Rebonds par match"),
                tags$li(tags$b("STL :"), "Interceptions par match"),
                tags$li(tags$b("BLK :"), "Contres par match"),
                tags$li(tags$b("FG% :"), "Pourcentage de réussite aux tirs"),
                tags$li(tags$b("MP :"), "Minutes jouées par match")
              ),
              tags$h4("Positions :"),
              tags$ul(
                tags$li(tags$b("Meneur (PG) :"), "Organisateur du jeu"),
                tags$li(tags$b("Arrière (SG) :"), "Scoreur"),
                tags$li(tags$b("Ailier (SF) :"), "Polyvalent"),
                tags$li(tags$b("Ailier fort (PF) :"), "Physique"),
                tags$li(tags$b("Pivot (C) :"), "Défenseur")
              )
            )
          )
        )
      ),
      
      # Onglet Équipes
      tabItem(
        tabName = "teams",
        h2("Statistiques des Équipes"),
        fluidRow(
          box(
            title = "Sélection d'Équipe",
            selectInput("selected_team", "Choisir une équipe:", 
                        choices = sort(unique(nba_games$Home))),
            width = 12
          )
        ),
        fluidRow(
          valueBoxOutput("team_wins"),
          valueBoxOutput("team_losses"),
          valueBoxOutput("team_win_pct")
        ),
        fluidRow(
          box(
            title = "Détail des Résultats",
            DTOutput("team_results_table"),
            width = 6
          ),
          box(
            title = "Performance par Lieu",
            plotlyOutput("team_home_away_plot"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Classement Général",
            DTOutput("full_standings_table"),
            width = 12
          )
        )
      ),
      
      # Onglet Joueurs
      tabItem(
        tabName = "players",
        h2("Analyse des Performances par Joueur"),
        fluidRow(
          box(
            title = "Filtres",
            selectInput("player_team", "Équipe:", 
                        choices = c("Toutes", unique(player_stats$Team))),
            selectInput("player_pos", "Position:", 
                        choices = c("Toutes", "Meneur", "Arrière", "Ailier", "Ailier fort", "Pivot")),
            sliderInput("player_min", "Minutes min. par match:",
                        min = 15, max = 40, value = 20),
            width = 3
          ),
          box(
            title = "Top Performeurs Offensifs",
            plotlyOutput("player_offense_plot"),
            width = 9
          )
        ),
        fluidRow(
          box(
            title = "Top Performeurs Défensifs",
            plotlyOutput("player_defense_plot"),
            width = 9,
            offset = 3
          )
        ),
        fluidRow(
          box(
            title = "Statistiques Complètes",
            DTOutput("players_table"),
            width = 12
          )
        )
      )
    )
  )
)

# Serveur
server <- function(input, output) {
  
  # Section Équipes
  selected_team_stats <- reactive({
    standings %>% filter(Team == input$selected_team)
  })
  
  output$team_wins <- renderValueBox({
    wins <- selected_team_stats()$Wins
    valueBox(wins, "Victoires", icon = icon("trophy"), color = "green")
  })
  
  output$team_losses <- renderValueBox({
    losses <- selected_team_stats()$Losses
    valueBox(losses, "Défaites", icon = icon("times-circle"), color = "red")
  })
  
  output$team_win_pct <- renderValueBox({
    pct <- selected_team_stats()$Win_Pct
    valueBox(
      scales::percent(pct, accuracy = 0.1), 
      "Pourcentage de Victoires", 
      icon = icon("percent"), 
      color = "blue"
    )
  })
  
  output$team_results_table <- renderDT({
    team_games <- nba_games %>%
      filter(Home == input$selected_team | Visitor == input$selected_team) %>%
      arrange(desc(Date)) %>%
      mutate(
        Opponent = ifelse(Home == input$selected_team, Visitor, Home),
        Location = ifelse(Home == input$selected_team, "Domicile", "Extérieur"),
        Result = ifelse(Winner == input$selected_team, "Victoire", "Défaite"),
        Score = ifelse(Home == input$selected_team, 
                       paste(PTS_Home, "-", PTS_Visitor),
                       paste(PTS_Visitor, "-", PTS_Home))
      ) %>%
      select(Date, Opponent, Location, Result, Score)
    
    datatable(
      team_games,
      options = list(pageLength = 5),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Result',
        backgroundColor = styleEqual(c("Victoire", "Défaite"), c('#d4edda', '#f8d7da'))
      )
  })
  
  output$team_home_away_plot <- renderPlotly({
    team_data <- standings %>% filter(Team == input$selected_team)
    
    plot_ly(
      x = c("Domicile", "Extérieur"),
      y = c(team_data$Home_Wins, team_data$Away_Wins),
      type = 'bar',
      name = "Victoires"
    ) %>%
      layout(
        title = paste("Victoires par lieu -", input$selected_team),
        yaxis = list(title = "Nombre de victoires")
      )
  })
  
  output$full_standings_table <- renderDT({
    datatable(
      standings,
      options = list(pageLength = 10),
      rownames = FALSE
    ) %>%
      formatPercentage('Win_Pct', 1)
  })
  
  # Section Joueurs
  filtered_players <- reactive({
    players <- player_stats
    if (input$player_team != "Toutes") {
      players <- players %>% filter(Team == input$player_team)
    }
    if (input$player_pos != "Toutes") {
      players <- players %>% filter(Position == input$player_pos)
    }
    players %>% filter(MP >= input$player_min)
  })
  
  output$player_offense_plot <- renderPlotly({
    top_players <- filtered_players() %>%
      arrange(desc(PPG)) %>%
      head(15)
    
    plot_ly(
      top_players,
      x = ~reorder(Player, -PPG),
      y = ~PPG,
      type = 'bar',
      color = ~Position,
      text = ~paste(Team, "|", Position, "|", MP, "min/match")
    ) %>%
      layout(
        title = "Top 15 Marqueurs (Points par match)",
        xaxis = list(title = ""),
        yaxis = list(title = "Points par match")
      )
  })
  
  output$player_defense_plot <- renderPlotly({
    top_defenders <- filtered_players() %>%
      mutate(Def_Score = STL + BLK + DRB) %>%
      arrange(desc(Def_Score)) %>%
      head(15)
    
    plot_ly(
      top_defenders,
      x = ~reorder(Player, -Def_Score),
      y = ~Def_Score,
      type = 'bar',
      color = ~Position,
      text = ~paste("Int:", STL, "| Ctr:", BLK, "| RebDef:", DRB)
    ) %>%
      layout(
        title = "Top 15 Défenseurs (Score = Intercep. + Contres + Rebonds Def)",
        xaxis = list(title = ""),
        yaxis = list(title = "Score défensif")
      )
  })
  
  output$players_table <- renderDT({
    datatable(
      filtered_players() %>% select(Player, Team, Position, PPG, APG, RPG, STL, BLK, `FG%`, MP),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      colnames = c('Joueur', 'Équipe', 'Poste', 'Pts/Match', 'Passes/Match', 'Rebonds/Match', 'Intercep.', 'Contres', 'FG%', 'Min/Match'),
      rownames = FALSE
    ) %>%
      formatRound(c('PPG', 'APG', 'RPG', 'STL', 'BLK', 'MP'), 1) %>%
      formatRound('FG%', 1)
  })
}

shinyApp(ui, server)