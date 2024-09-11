library(shiny)
library(bslib)

# List of exercises by body part
exercises <- list(
    Chest = c("Bench Press", "Push-Ups", "Dumbbell Flyes", "Incline Bench Press"),
    Back = c("Pull-Ups", "Bent-Over Rows", "Lat Pulldowns", "Deadlifts"),
    Legs = c("Squats", "Lunges", "Leg Press", "Calf Raises"),
    Shoulders = c("Overhead Press", "Lateral Raises", "Front Raises", "Shrugs"),
    Arms = c("Bicep Curls", "Tricep Extensions", "Hammer Curls", "Dips"),
    Core = c("Plank", "Crunches", "Russian Twists", "Leg Raises")
)

# List of finishers by body part
finishers <- list(
    Chest = c("Push-Up Burnout", "Chest Dips to Failure"),
    Back = c("Pull-Up Burnout", "Renegade Rows"),
    Legs = c("Bodyweight Squat Burnout", "Jump Lunges"),
    Shoulders = c("Lateral Raise Burnout", "Pike Push-Ups"),
    Arms = c("Tricep Pushdown Burnout", "21s Bicep Curls"),
    Core = c("Plank Hold", "Flutter Kicks")
)

# Custom theme with color grading (using system fonts)
custom_theme <- bs_theme(
    bg = "#f8f9fa",
    fg = "#212529",
    primary = "#007bff",
    secondary = "#6c757d",
    success = "#28a745",
    info = "#17a2b8",
    warning = "#ffc107",
    danger = "#dc3545",
    base_font = "Arial, sans-serif",
    heading_font = "Helvetica, sans-serif",
    font_scale = 0.9
)

ui <- page_fluid(
    theme = custom_theme,
    tags$head(
        tags$style(HTML("
            .card { transition: all 0.3s ease-in-out; }
            .card:hover { transform: translateY(-5px); box-shadow: 0 4px 8px rgba(0,0,0,0.2); }
        "))
    ),
    titlePanel("Fit4Me - Random Workout Generator"),
    fluidRow(
        column(4,
               card(
                   card_header("Workout Settings"),
                   radioButtons("workout_type", "Workout Type",
                                choices = c("Single Body", "Double Body", "Mix Body", "Core"),
                                selected = "Single Body"),
                   conditionalPanel(
                       condition = "input.workout_type == 'Single Body'",
                       selectInput("single_body_part", "Select Body Part", choices = names(exercises))
                   ),
                   conditionalPanel(
                       condition = "input.workout_type == 'Double Body'",
                       selectInput("double_body_part1", "Select First Body Part", choices = names(exercises)),
                       selectInput("double_body_part2", "Select Second Body Part", choices = names(exercises))
                   ),
                   numericInput("workout_minutes", "Workout Duration (minutes)", value = 30, min = 10, max = 120),
                   numericInput("num_sets", "Number of Sets", value = 3, min = 1, max = 5),
                   numericInput("num_exercises", "Number of Exercises", value = 5, min = 1, max = 10),
                   checkboxInput("add_finisher", "Add Finisher", value = TRUE),
                   actionButton("generate", "Generate Workout", class = "btn-primary btn-lg btn-block mt-3")
               )
        ),
        column(8,
               card(
                   card_header("Your Workout Plan"),
                   tableOutput("workout_plan")
               )
        )
    )
)

server <- function(input, output, session) {
    workout_plan <- reactiveVal(NULL)
    
    generate_workout <- function(exercises, num_exercises, body_part, add_finisher) {
        workout <- data.frame(
            Exercise = sample(exercises, num_exercises, replace = TRUE),
            `Body Part` = rep(body_part, num_exercises),
            Reps = sample(8:15, num_exercises, replace = TRUE),
            stringsAsFactors = FALSE
        )
        
        if (add_finisher) {
            finisher <- data.frame(
                Exercise = sample(finishers[[body_part]], 1),
                `Body Part` = body_part,
                Reps = "To Failure",
                stringsAsFactors = FALSE
            )
            workout <- rbind(workout, finisher)
        }
        
        workout
    }
    
    observeEvent(input$generate, {
        workout_minutes <- input$workout_minutes
        num_sets <- input$num_sets
        num_exercises <- input$num_exercises
        add_finisher <- input$add_finisher
        
        workout <- switch(input$workout_type,
                          "Single Body" = {
                              body_part <- input$single_body_part
                              workout <- generate_workout(exercises[[body_part]], num_exercises, body_part, add_finisher)
                              workout$Sets <- c(rep(num_sets, num_exercises), if(add_finisher) 1 else NULL)
                              workout$`Time (min)` <- c(rep(round(workout_minutes / num_exercises), num_exercises), if(add_finisher) 5 else NULL)
                              workout
                          },
                          "Double Body" = {
                              body_parts <- c(input$double_body_part1, input$double_body_part2)
                              workout1 <- generate_workout(exercises[[body_parts[1]]], num_exercises %/% 2, body_parts[1], FALSE)
                              workout2 <- generate_workout(exercises[[body_parts[2]]], num_exercises - num_exercises %/% 2, body_parts[2], FALSE)
                              workout <- rbind(workout1, workout2)
                              if (add_finisher) {
                                  finisher1 <- generate_workout(exercises[[body_parts[1]]], 1, body_parts[1], TRUE)
                                  finisher2 <- generate_workout(exercises[[body_parts[2]]], 1, body_parts[2], TRUE)
                                  workout <- rbind(workout, finisher1[nrow(finisher1),], finisher2[nrow(finisher2),])
                              }
                              workout$Sets <- c(rep(num_sets, num_exercises), if(add_finisher) c(1, 1) else NULL)
                              workout$`Time (min)` <- c(rep(round(workout_minutes / num_exercises), num_exercises), if(add_finisher) c(5, 5) else NULL)
                              workout
                          },
                          "Mix Body" = {
                              all_exercises <- unlist(exercises)
                              all_body_parts <- rep(names(exercises), sapply(exercises, length))
                              indices <- sample(seq_along(all_exercises), num_exercises, replace = TRUE)
                              workout <- data.frame(
                                  Exercise = all_exercises[indices],
                                  `Body Part` = all_body_parts[indices],
                                  Reps = sample(8:15, num_exercises, replace = TRUE),
                                  stringsAsFactors = FALSE
                              )
                              workout$Sets <- rep(num_sets, num_exercises)
                              workout$`Time (min)` <- round(workout_minutes / num_exercises)
                              
                              if (add_finisher) {
                                  finisher_body_part <- sample(names(exercises), 1)
                                  finisher <- generate_workout(exercises[[finisher_body_part]], 1, finisher_body_part, TRUE)
                                  finisher$Sets <- 1
                                  finisher$`Time (min)` <- 5
                                  workout <- rbind(workout, finisher[nrow(finisher),])
                              }
                              workout
                          },
                          "Core" = {
                              workout <- generate_workout(exercises$Core, num_exercises, "Core", add_finisher)
                              workout$Sets <- c(rep(num_sets, num_exercises), if(add_finisher) 1 else NULL)
                              workout$`Time (min)` <- c(rep(round(workout_minutes / num_exercises), num_exercises), if(add_finisher) 5 else NULL)
                              workout
                          }
        )
        
        workout_plan(workout)
    })
    
    output$workout_plan <- renderTable({
        req(workout_plan())
        workout_plan()
    }, striped = FALSE, hover = TRUE, bordered = TRUE, class = "table-primary")
}

shinyApp(ui, server)
