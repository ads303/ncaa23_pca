library(tidyverse)
library(tidymodels)
library(shiny)
library(FactoMineR)
library(shinydashboard)
library(rsconnect)

# Read in the data

setwd("~/Documents/Projects/pc_ncaa23_tourney/")
data <- read.csv("ncaa_data.csv")

# Define logistic regression model
model <- glm(data$AdjEM ~ data$AdjO + data$AdjD + data$AdjT + data$Luck + data$OppO + data$OppD + data$`AdjEM.1` + data$NCSOSEM)

# Select relevant variables
vars <- c("AdjEM", "AdjO", "AdjD", "AdjT", "Luck", "AdjEM.1", "OppO", "OppD", "NCSOSEM")
data <- data %>% select(Team, record, all_of(vars))

# Scale variables
data_scaled <- data %>% 
  select(-Team, -record) %>% 
  scale()

# Run PCA
pca <- PCA(data_scaled, scale.unit = FALSE, ncp = length(vars))

# Extract scores of first principal component
pc_scores <- pca$ind$coord[,1]

# Add scores to original data
data <- data %>% mutate(pc_score = pc_scores)

# Rank teams by PC score
data <- data %>% mutate(Rank = rank(-pc_score, ties.method = "min"))

# Display top 10 teams by PC score
top_teams <- head(data %>% arrange(desc(pc_score)), 10)
ggplot(top_teams, aes(x = reorder(Team, -pc_score), y = pc_score)) +
  geom_col() +
  xlab("") +
  ylab("PC score") +
  ggtitle("Top 10 NCAA teams by PC score")




ui <- dashboardPage(
  dashboardHeader(title = "NCAA '23 Tourney Chances"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Performance Prediction", tabName = "performance_prediction")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "performance_prediction",
              fluidRow(
                box(
                  title = "PC-based Tourney Run Rankings",
                  status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  plotOutput("team_performance_plot")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  # Plot team PC scores
  output$team_performance_plot <- renderPlot({
    ggplot(data, aes(x = reorder(Team, pc_score), y = pc_score)) +
      geom_bar(stat = "identity", fill = "#0072B2") +
      theme(axis.text.x = element_text(family="Tahoma", face="bold", angle = 90, hjust = 1, vjust = 0.5)) +
      labs(x = "Team", y = "PC Score") +
      ggtitle("Team Performance Rankings")
  })
  
  selected_team <- reactive({
    subset(df, Team == input$team)
  })

}

shinyApp(ui, server)


#rsconnect::deployApp("~/Documents/Projects/pc_ncaa23_tourney/")
