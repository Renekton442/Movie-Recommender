# server.R

library(dplyr)
library(data.table)
library(Matrix)
library(plyr)
library(recommenderlab)
library(shiny)
library(ShinyRatingInput)

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data

movies = readLines("movies.dat")
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

movies$image_url = sapply(movies$MovieID, function(x) paste0("https://liangfgithub.github.io/MovieImages/", x, '.jpg?raw=true'))

# server 
shinyServer(function(input, output) { 
  output$genre_selection <- renderUI({ # modified from Derek's code
    selected_genre = input$genre_select_menu
    if(selected_genre == ""){
      h3("Select a genre for recommendations")}
    else{
      selected_genre = gsub("'", ".", selected_genre)
      selected_genre = gsub("-", ".", selected_genre)
      
      num_rows = 2
      num_movies = 5
      rec_results = as.integer(system1(selected_genre)) 
      
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          current_movieID = rec_results[(i-1)*num_movies + j]
          img_url = paste0("https://liangfgithub.github.io/MovieImages/", current_movieID, ".jpg?raw=true")
          
          box(width = 2, status = "success", solidHeader = FALSE, 
              
              div(style = "text-align:center", img(src = img_url, height = 150)),
              div(style = "text-align:center; color: darkred; font-size: 18px",
                  movies$Title[movies$MovieID == current_movieID])
          )
        }))) 
      }) 
    }#else
  })#genre_selection
  
  #display 
  output$ratings <- renderUI({
    num_rows = 5
    num_movies = 5
    
    mIDs_for_rating = as.integer(names(sort(table(rate$MovieID, useNA = "no"), decreasing = T)[1:30]))
    
    fluidRow(
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          current_movieID = mIDs_for_rating[(i-1)*num_movies + j]
          img_url = paste0("https://liangfgithub.github.io/MovieImages/", current_movieID, ".jpg?raw=true")
          
          list(box(width = 2,
                   div(style = "text-align:center", img(src = img_url, height = 150)),
                   div(style = "text-align:center; color: black; font-size: 18px", 
                       movies$Title[movies$MovieID == current_movieID]),
                   div(style = "text-align:center; font-size: 100%; color: gold;", 
                       ratingInput(paste0("select_", current_movieID), label = "", dataStop = 5))  #
          ))
        })))
      })
    )#fluidRow 1
  })#output$ratings
  
  
  retrieve_ratings <- eventReactive(input$btn, {
    # get the user's rating data
    value_list <- reactiveValuesToList(input)
    user_ratings <- get_user_ratings(value_list)
    return(system2(user_ratings))
  }) 
  
  output$results <- renderUI({
    num_rows = 2
    num_movies  = 5
    rec_results = as.integer(retrieve_ratings()) #returns a simple list
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        current_movieID = rec_results[(i-1)*num_movies + j]
        img_url = paste0("https://liangfgithub.github.io/MovieImages/", current_movieID, ".jpg?raw=true")
        
        box(width = 2, status = "success", solidHeader = TRUE, 
            title = paste0("Rank ", ((i-1)*num_movies + j)),
            
            div(style = "text-align:center", img(src = img_url, height = 150)),
            div(style = "text-align:center; color: darkred; font-size: 18px",
                movies$Title[movies$MovieID == current_movieID])
            
        )
      }))) 
    }) 
  }) # output$results renderUI function
})#shinyServer