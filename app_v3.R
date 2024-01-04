#shiny app for club assignment
#V1
#Amanda Reneau
#12/31/2023



library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)
library(tidyverse)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(DT)
library(janitor)
library(RColorBrewer)

# UI Definition
ui <- fluidPage(
  # Custom CSS to change the look and feel of the app
  tags$head(
    tags$style(HTML("
      .shiny-output-error { color: red; }
      .shiny-output-error:before { content: 'Error: '; }
      
      .well { background-color: #9DC3E7; border-radius: 5px; } 
      h1, h2, h3 { color: #1F5373; }
    "))
  ),
  # Adding an image
  
  titlePanel("Club Assignment Dashboard", windowTitle = "Club Assignment App"),
  sidebarLayout(
    sidebarPanel(
      # Adding a text description
      tags$img(src = "SSA_logo.jpg", height = 300, width = 600),
      tags$p("Directions: Select a .csv file that contains the columns First_Name, Last_Name, Grade, First_Choice, Second_Choice, Third_Choice, preference. (The preference column must only have the numbers 0 and 1. Students with a 1 will be given priority assignment.) Input Club Capacities, select grade, and then click Run. If the model failes to produce results, increase the value for not_assigned"),
      fileInput("datafile", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      uiOutput("club_capacity_inputs"),
      selectInput("grade", "Select grade", choices = NULL),
      actionButton("run", "Run Assignment"),
     
    ),
   
    mainPanel(
      # Create a tabset panel for the plots
      tabsetPanel(
        tabPanel("First Choices", plotOutput("plot1")),
        tabPanel("Second Choices", plotOutput("plot2")),
        tabPanel("Third Choices", plotOutput("plot3"))
      ),
      plotOutput("plot4"),
      plotOutput("plot"),
      DTOutput("table"),
      downloadButton("downloadData", "Download CSV")
      #tableOutput("choice_summary")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Reactive expression to store and process the uploaded data
  data <- reactive({
    req(input$datafile)
    df <- read.csv(input$datafile$datapath)
    
    df<- df %>%
      clean_names() %>% 
      mutate(fourth_choice= "not_assigned")
    
    # Trim whitespace from club names
    df$first_choice <- trimws(df$first_choice)
    df$second_choice <- trimws(df$second_choice)
    df$third_choice <- trimws(df$third_choice)
    df$grade <- as.character(df$grade)
 
    return(df)
  })
  
  # Observe the uploaded data to update UI elements
  observe({
    df <- data()
    # The palette with grey:
    cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    updateSelectInput(session, "grade", choices = unique(df$grade))
    
    # Dynamically create club capacity inputs
    club_names <- unique(unlist(df[, c("first_choice", "second_choice", "third_choice", 'fourth_choice')]))
    output$club_capacity_inputs <- renderUI({
      lapply(seq_along(club_names), function(i) {
        numericInput(paste0("capacity_", club_names[i]), 
                     label = paste("Capacity for", club_names[i]), 
                     value = 10)
      })
    })
    output$plot1 <- renderPlot({
    # Explore data
    ggplot(data = df) +
      geom_bar(mapping = aes(x = first_choice, fill = grade), position = "dodge") +
      labs(title = "First Club Choice by Grade", y = "Number of Students", x = "First Choice Club")+
        theme(text = element_text(size = 24),  # Increase text size
              axis.title = element_text(size = 24),  # Increase axis title size
              plot.title = element_text(size = 26, hjust = 0.5)) + # Increase plot title size and center it
              scale_fill_manual(values = cbp1)
  })
    output$plot2 <- renderPlot({
      # Explore data
      ggplot(data = df) +
        geom_bar(mapping = aes(x = second_choice, fill = grade), position = "dodge") +
        labs(title = "Second Choice Club by Grade", y = "Number of Students", x = "Second Choice Club")+
        theme(text = element_text(size = 24),  # Increase text size
              axis.title = element_text(size = 24),  # Increase axis title size
              plot.title = element_text(size = 26, hjust = 0.5)) + # Increase plot title size and center it
        scale_fill_manual(values = cbp1)
    }) 
    output$plot3 <- renderPlot({
      # Explore data
      ggplot(data = df) +
        geom_bar(mapping = aes(x = third_choice, fill = grade), position = "dodge") +
        labs(title = "Third Choice Club by Grade", y = "Number of Students", x = "Third Choice Club")+
        theme(text = element_text(size = 24),  # Increase text size
              axis.title = element_text(size = 24),  # Increase axis title size
              plot.title = element_text(size = 26, hjust = 0.5)) + # Increase plot title size and center it
        scale_fill_manual(values = cbp1)
    })    
    
    output$plot4 <- renderPlot({
      # Explore data
      df<-df %>% 
        filter(preference == 1)
      ggplot(data = df) +
        geom_bar(mapping = aes(x = first_choice, fill = grade), position = "dodge") +
        labs(title = "Prefered Students Only, First Choice Club by Grade", y = "Number of Students", x = "First Choice Club")+
        theme(text = element_text(size = 24),  # Increase text size
              axis.title = element_text(size = 24),  # Increase axis title size
              plot.title = element_text(size = 26, hjust = 0.5)) + # Increase plot title size and center it
        scale_fill_manual(values = cbp1)
    })
  })
  
  # Function to get club capacities from input
  get_club_capacities <- reactive({
    df <- data()
    club_names <- unique(unlist(df[, c("first_choice", "second_choice", "third_choice", "fourth_choice")]))
    sapply(club_names, function(name) input[[paste0("capacity_", name)]])
  })
  
  # Observe event for the 'Run Assignment' button
  observeEvent(input$run, {
    req(data())
    df <- data()
    club_names <- unique(unlist(df[, c("first_choice", "second_choice", "third_choice", "fourth_choice")]))
    club_capacities <- get_club_capacities()
    club_info <- data.frame(Club_Name = club_names, Capacity = club_capacities)
    
    # Initialize a list to store solutions for each grade
    assignment_solutions <- list()
    
    
    
    for (grade in unique(df$grade)) {
      # Filter students by grade
      df_grade <- df[df$grade == grade, ]
      
      # Update club capacities based on preferences
      for (club in club_names) {
        num_pref_students <- sum(df_grade$preference == 1 & df_grade$first_choice == club)
        club_info$Capacity[club_info$Club_Name == club] <- 
          club_info$Capacity[club_info$Club_Name == club] - num_pref_students
      }
      
      # Create cost matrix for this grade
      n_students_grade <- nrow(df_grade)
      cost_matrix_grade <- matrix(100, nrow = n_students_grade, ncol = length(club_names))
      colnames(cost_matrix_grade) <- club_names
      rownames(cost_matrix_grade) <- paste(df_grade$first_name, df_grade$last_name)
      

      # Adjust the cost matrix
      for (i in 1:n_students_grade) {
        for (j in 1:length(club_names)) {
          if (df_grade$preference[i] == 1 && df_grade$first_choice[i] == club_names[j]) {
            cost_matrix_grade[i, j] <- 0  # Very low cost for preferred first choice
          } else if (df_grade$first_choice[i] == club_names[j]) {
            cost_matrix_grade[i, j] <- 1
          } else if (df_grade$second_choice[i] == club_names[j]) {
            cost_matrix_grade[i, j] <- 2
          } else if (df_grade$third_choice[i] == club_names[j]) {
            cost_matrix_grade[i, j] <- 3
          } else if (df_grade$fourth_choice[i] == club_names[j]) {
            cost_matrix_grade[i, j] <- 10
          } else {
            cost_matrix_grade[i, j] <- 1000  # Assign a high cost to unrequested clubs
          }
        }
      }
      
      
      # Set up and solve the model
      model <- MIPModel() %>%
        add_variable(x[i, j], i = 1:n_students_grade, j = 1:length(club_names), type = "binary") %>%
        set_objective(sum_expr(cost_matrix_grade[i, j] * x[i, j], i = 1:n_students_grade, j = 1:length(club_names)), "min") %>%
        add_constraint(sum_expr(x[i, j], j = 1:length(club_names)) == 1, i = 1:n_students_grade) %>%
        add_constraint(sum_expr(x[i, j], i = 1:n_students_grade) <= club_info$Capacity[j], j = 1:length(club_names))
      
      # Solve the model
      result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
      
      # After solving the model
      solution <- get_solution(result, x[i, j])
      solution$student <- rownames(cost_matrix_grade)[solution$i]
      
      # Map indices to club names
      solution$club_name <- club_names[solution$j]
      
      # Reshape the solution using club names as column headers
      assignment_solution_grade <- dcast(solution, student ~ club_name, value.var = "value")
      assignment_solution_grade$grade <- grade  # Add grade column
      
      # Store the solution in the list with the grade as the key
      assignment_solutions[[as.character(grade)]] <- assignment_solution_grade
    }
    
    # Combine all grades into one dataframe for plotting
    all_grades_assignments <- do.call(rbind, assignment_solutions)
    
    # Requests per grade
    requests_per_grade <- df %>%
      group_by(grade, first_choice) %>%
      summarise(Requests = n(), .groups = "drop") %>%
      ungroup() %>%
      rename(Club = first_choice)
    
    # Assignments per grade
    assignments_per_grade <- all_grades_assignments %>%
      pivot_longer(cols = -c(student, grade), names_to = "Club", values_to = "Assigned") %>%
      filter(Assigned == 1) %>%
      group_by(grade, Club) %>%
      summarise(Assignments = n()) %>%
      ungroup()
    
    # Merge requests and assignments data
    combined_data_per_grade <- merge(requests_per_grade, assignments_per_grade, by = c("grade", "Club"), all = TRUE)
    
    # Reshape data for plotting
    combined_data_long_per_grade <- pivot_longer(combined_data_per_grade, cols = c(Requests, Assignments), names_to = "Type", values_to = "Count")
    # Debug: Check combined data for grades 1 and 2
    combined_data_debug <- combined_data_per_grade %>% filter(grade %in% c(1, 2))
    print(combined_data_debug)
    # Plot
    output$plot <- renderPlot({
      specific_grade_data <- combined_data_long_per_grade %>%
        filter(grade == input$grade)
      ggplot(specific_grade_data, aes(x = Club, y = Count, fill = Type)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        scale_fill_manual(values = c("#56B4E9", "#E69F00")) + # Custom colors
        labs(title = paste("Club Requests and First Choice Assignments for grade", input$grade),
             x = "Club",
             y = "Count",
             fill = "Metric") +
        theme(text = element_text(size = 24),  # Increase text size
              axis.title = element_text(size = 24),  # Increase axis title size
              plot.title = element_text(size = 26, hjust = 0.5))  # Increase plot title size and center it
    })
    # Transform assignment solutions into a student-club assignment data frame
    student_club_assignments <- lapply(assignment_solutions, function(assignment_df) {
      melted_assignment <- melt(assignment_df, id.vars = c("student", "grade"))
      
      # Filter only for assigned clubs and include the grade column
      assigned_clubs <- melted_assignment[melted_assignment$value == 1, c("student", "grade", "variable")]
      colnames(assigned_clubs) <- c("Student", "grade", "Assigned_Club")
      return(assigned_clubs)
    })
    
    # Combine all grades into one dataframe
    all_student_club_assignments <- do.call(rbind, student_club_assignments)
    
    

    output$table <- DT::renderDataTable({
      # Filter the data based on the selected grade
      filtered_data <- all_student_club_assignments %>%
        filter(grade == input$grade)
      datatable(filtered_data)
    })
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("filtered-data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        # Repeat the data filtering process
        filtered_data <- all_student_club_assignments %>%
          filter(grade == input$grade)
        
        # Write the filtered data to a CSV file
        write.csv(filtered_data, file, row.names = FALSE)
      }
    )
    # # Create a table to display the results
    # output$assignment_results <- renderDataTable({
    # df <- data()
    # merged_data <- merge(solution, df, by = "student")
    # 
    # # Add a column to indicate if the assigned club was the first choice
    # merged_data$first_choice_Match <- merged_data$assigned_club == merged_data$first_choice
    # 
    # 
    #   datatable(merged_data[, c("student", "assigned_club", "first_choice", "first_choice_Match")])
    # })
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
