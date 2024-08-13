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
  
  # Adding a title and window title
  titlePanel("Club Assignment Dashboard", windowTitle = "Club Assignment App"),
  
  # Layout with sidebar and main panel
  sidebarLayout(
    sidebarPanel(
      # Adding a text description
      tags$p("Directions: Select a .csv file that contains the columns First_Name, Last_Name, Grade, First_Choice, Second_Choice, Third_Choice, and Preference. The 'Preference' column should only contain the numbers 0 and 1, with students marked as 1 receiving priority for club assignments. To use the app, input the club capacities, select the grade level, and click 'Run.' If the 'Not Assigned' value is set to zero, any student who doesn't get placed in one of their top three choices will be randomly assigned to a club. However, if 'Not Assigned' is set to a high number, students will not be placed in a club they didn't request."),
      fileInput("datafile", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      uiOutput("club_capacity_inputs"),
      actionButton("run", "Run Assignment")
    ),
    
    mainPanel(
      # First tabset panel for the overview of student choices
      tabsetPanel(
        tabPanel("Overview of Student Choices",
                 tabsetPanel(
                   tabPanel("First Choices", plotOutput("plot1", height = "800px", width = "100%")),
                   tabPanel("Second Choices", plotOutput("plot2", height = "800px", width = "100%")),
                   tabPanel("Third Choices", plotOutput("plot3", height = "800px", width = "100%"))
                 ),
                 plotOutput("plot4", height = "600px", width = "100%")
        ),
        
        # Second tabset panel for the assignment results
        tabPanel("Assignment Results",
                 plotOutput("plot", height = "800px", width = "100%"),
                 selectInput("grade", "Select grade", choices = NULL),
                 DTOutput("table"),
                 downloadButton("downloadData", "Download CSV")
        )
      )
    )
  )
)


# Server Logic
server <- function(input, output, session) {
  # Reactive expression to store and process the uploaded data
  data <- reactive({
    req(input$datafile)
    df <- read.csv(input$datafile$datapath)
    
    df <- df %>%
      clean_names() %>% 
      mutate(fourth_choice = "not_assigned",
             preference = if_else(is.na(preference) | preference == "", 0, preference))
    
      
    
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
    
    # Use a predefined palette and extend it to 22 colors to show each club
    cbp22 <- colorRampPalette(brewer.pal(8, "Set3"))(22)
    
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
              scale_fill_manual(values = cbp1)+
        coord_flip()  # Flip the coordinates
  })
    output$plot2 <- renderPlot({
      # Explore data
      ggplot(data = df) +
        geom_bar(mapping = aes(x = second_choice, fill = grade), position = "dodge") +
        labs(title = "Second Choice Club by Grade", y = "Number of Students", x = "Second Choice Club")+
        theme(text = element_text(size = 24),  # Increase text size
              axis.title = element_text(size = 24),  # Increase axis title size
              plot.title = element_text(size = 26, hjust = 0.5)) + # Increase plot title size and center it
        scale_fill_manual(values = cbp1)+
        coord_flip()  # Flip the coordinates
    }) 
    output$plot3 <- renderPlot({
      # Explore data
      ggplot(data = df) +
        geom_bar(mapping = aes(x = third_choice, fill = grade), position = "dodge") +
        labs(title = "Third Choice Club by Grade", y = "Number of Students", x = "Third Choice Club")+
        theme(text = element_text(size = 24),  # Increase text size
              axis.title = element_text(size = 24),  # Increase axis title size
              plot.title = element_text(size = 26, hjust = 0.5)) + # Increase plot title size and center it
        scale_fill_manual(values = cbp1)+
        coord_flip()  # Flip the coordinates
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
        scale_fill_manual(values = cbp1)+
        coord_flip()  # Flip the coordinates
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
            cost_matrix_grade[i, j] <- 4
          } else if (df_grade$third_choice[i] == club_names[j]) {
            cost_matrix_grade[i, j] <- 8
          } else if (df_grade$fourth_choice[i] == club_names[j]) {#students will not be placed into a club that they did not request. 
            cost_matrix_grade[i, j] <- 20
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
       # add_constraint(sum_expr(x[i, j] * (df_grade$first_choice == club_names[j]), i = 1:n_students_grade) >= club_info$Capacity[j] - sum(df_grade$first_choice == club_names[j]), j = 1:length(club_names)) %>%
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
              plot.title = element_text(size = 26, hjust = 0.5))+  # Increase plot title size and center it
             coord_flip()  # Flip the coordinates
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
    original <- data() %>%
      mutate(Student = paste0(first_name, " ", last_name))
    # Combine the data frames by matching on 'Student' and 'grade'
    combined_df <- original %>%
      left_join(all_student_club_assignments, by = c("Student", "grade"))
    

    output$table <- DT::renderDataTable({
      # Filter the data based on the selected grade
      filtered_data <- combined_df %>%
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
