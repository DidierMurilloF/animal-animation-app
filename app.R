#' @title Animal Animation Shiny App
#' @author Didier Murillo,
#' \email{didier.statistician@gmail.com}

# load libraries
library(shiny)
library(shinyjs)

# Define the client side
ui <- fluidPage(
  # Enable shinyjs
  shinyjs::useShinyjs(),
  includeCSS("www/style.css"),
  br(),
  div(
    class = "container",
    fluidRow(
      column(width = 6,  tags$h2("Welcome to the Animal Animation Shiny App!")),
      column(
        width = 4,  
        tags$img(
          src="https://raw.githubusercontent.com/rstudio/hex-stickers/main/SVG/shiny.svg", 
          id = "shiny_img", 
          style = "width: 100px; cursor: pointer;"
        )
      ),
      column(
        width = 2, 
        uiOutput("heart_shiny")
      )
    ),
    actionButton(inputId = "add_animals", label = "Add Animals", class = "my-btn-class"),
    br(),
    conditionalPanel(
      condition = "JS(output.animal_list.length > 0)",
      uiOutput("animal_list")
    )
  )
)

# Define the server side
server <- function(input, output) {
  
  # Enable shinyjs
  shinyjs::useShinyjs()
  
  # Define the list of animals
  animals <- c("cow", "cat", "dog", "gator", "horse")
  
  # Define a reactive value to store the list of selected animals
  animal_list <- reactiveVal(character(0))
  
  # Define a reactive value to track the number of times the "Add Animals" button has been clicked
  button_count <- reactiveVal(0)
  
  random_animals <- sample(animals)
  
  # Add a new animal to the list each time the "Add Animals" button is clicked
  observeEvent(input$add_animals, {
    button_count(button_count() + 1)
    #animal <- sample(animals, 1)
    if (button_count() <= 5) {
      animal_list(c(animal_list(), random_animals[button_count()]))
      print(animal_list())
    } else {
      showModal(modalDialog(
        "No more animals!",
        easyClose = TRUE
      ))
    }
  })
  
  # Define a reactive values to track the number of times each animal is clicked
  click_count <- reactiveValues(cow = 0, cat = 0, dog = 0, gator = 0, horse = 0)
  
  # Add a click event to each animal image using shinyjs::onclick
  lapply(animals, function(animal) {
      shinyjs::onclick(animal, {
        click_count[[animal]] <- click_count[[animal]] + 1
      })
  })
  
  # Output the list of animals as a series of img tags
  output$animal_list <- renderUI({
    if (length(animal_list()) == 0) {
      tags$div()
    } else {
      animal_tags <- lapply(animal_list(), function(animal) {
        heart_width <- 10 + 15 * click_count[[animal]]
        style_img_heart <- paste0("width: ", heart_width, "px;", " margin-left: -150px; margin-top: 40px")
        style_img_animal <-  "height: 150px; margin-top: 10px; cursor: pointer;"
        fluidRow(
          column(
            width = 6, 
            tags$img(
              src = paste0(animal, ".svg"), 
              style = style_img_animal, id = animal
            )
          ),
          column(
            width = 6,  
            tags$img(
              src = "heart.svg", 
              style = style_img_heart, 
              id = paste0(animal, "_heart")
            )
          )
        )
      })
      do.call(tags$div, animal_tags)
    }
  })
  
  #--------- Shiny img -----------#
  # Define a reactive expression to track clicks on the shiny image
  click_count_shiny <- reactiveVal(0)

  # Add a click event to the shiny image using shinyjs::onclick
  shinyjs::onclick("shiny_img", click_count_shiny(click_count_shiny() + 1))

  output$heart_shiny <- renderUI({
    style <- paste0("width: ", 1 + 20 * click_count_shiny(), "px;", " margin-left: -120px; margin-top: 1px")
    tags$img(src = "heart.svg", id = "love",  style = style)
  })
}

shinyApp(ui, server)
