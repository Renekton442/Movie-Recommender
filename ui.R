# ui.R
# modified from Derek's code
library(shiny)
library(shinydashboard)

source('system1_function.R')
source('system2_function.R')


genre_list = c("", "Action", "Adventure", "Animation",
               "Children's", "Comedy", "Crime", "Documentary",
               "Drama", "Fantasy", "Film-Noir", "Horror",
               "Musical", "Mystery", "Romance", "Sci-Fi",
               "Thriller", "War", "Western")

# UI for app
shinyUI(
  dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Movie Recommender"),
    dashboardSidebar(disable = TRUE),
    
    dashboardBody(
      fluidRow(
        box(width = 12, title = "Genre Recommendation", 
            status = "info", solidHeader = TRUE, collapsible = TRUE,
            selectInput(inputId = "genre_select_menu", 
                        label = "Select a Genre:",
                        choices = genre_list,
                        width = '250px',
                        selected = ""),
            tableOutput("genre_selection")
        )
      ),
      
      fluidRow(
        box(width = 12, title = "Step 1: Rate as many movies as possible", 
            status = "info", solidHeader = TRUE, collapsible = TRUE,
            uiOutput('ratings'))
      ),
      
      fluidRow(
        box(
          width = 12, status = "info", solidHeader = TRUE,
          title = "Step 2: Discover movies you might like",
          br(),
          actionButton("btn", "Check recommendations", class = "btn-warning"),
          br(),
          tableOutput("results")
        )
      )
    )
  )
)