# DEMOGRAPHICS TABLE
demographics_table <- reactive({
  all_metadata <- selected_data()[["metadata"]]
  avg_samples <- mean(table(all_metadata$patient))
  sample_count <- length(all_metadata$Run)
  metadata <- dplyr::distinct(all_metadata, patient, .keep_all = TRUE)
  data <- data.frame(
    Statistic = c("Total number of donors", 
                  "Total number of recipients", 
                  "Number of samples collected",
                  "Average number of samples collected per study participant",
                  "Number of patients who responded",
                  "Number of patients who did not respond"),
    Value = c(
      sum(metadata$donor_or_recipient == "Donor", na.rm = TRUE),
      sum(metadata$donor_or_recipient == "Recipient", na.rm = TRUE),
      sample_count,
      round(avg_samples, 2),
      sum(metadata$donor_or_recipient == "Recipient" & metadata$response == "R", na.rm = TRUE),
      sum(metadata$donor_or_recipient == "Recipient" & metadata$response == "NR", na.rm = TRUE)
    )
  )
  return(data)
})
output$demographics_table_real <- renderTable({
  req(selected_data())  # Stops execution if no dataset is selected
  demographics_table()
})
output$demographics_table <- renderUI({
  if (is.null(selected_data())) {
    return(tags$p("⚠️ Please select a dataset to view demographics.", 
                  style = "color: red; font-weight: bold; margin-bottom: 20px;"))
  }
  tableOutput("demographics_table_real")  # Placeholder for real table
})

# STUDY DESIGN TABLE
output$study_design_table_real <- renderTable({
  req(selected_data())  # Ensure a dataset is selected
  study_designs <- list(
    "Routy"  = c("No", "Oral capsule", "Healthy"),
    "Davar"  = c("Yes", "Colonoscopy", "Previous patient"),
    "Baruch" = c("Yes", "Combination of oral capsule and colonoscopy", "Previous patient")
  )
  selected_studies <- input$clicked_cards  # Can be multiple values
  # Filter only the studies that exist in study_designs
  matching_studies <- selected_studies[selected_studies %in% names(study_designs)]
  # Create a dataframe that combines all selected studies
  study_info <- do.call(rbind, lapply(matching_studies, function(study) {
    data.frame(
      Study = study,
      Protocol = c("Antibiotics Used", "Method of FMT Administration", "Donor Status"),
      Design = study_designs[[study]],
      stringsAsFactors = FALSE
    )
  }))
  return(study_info)
}, rownames = FALSE)
output$study_design_table <- renderUI({
  if (is.null(selected_data())) {
    return(tags$p("⚠ Please select a dataset to view study design information.", 
                  style = "color: red; font-weight: bold; margin-bottom: 20px;"))
  }
  tableOutput("study_design_table_real")
})

# METADATA SUMMARY PLOT
# Initialize variables
plot_shown <- reactiveVal(FALSE)
invalid_text <- reactiveVal(NULL)
# Filtered data based on user inputs (moved this part to a reactive expression)
m_filtered_data <- reactive({
  req(selected_data())  # Ensure dataset is selected
  metadata <- dplyr::distinct(selected_data()[["metadata"]], patient, .keep_all = TRUE)
  # Filter metadata based on group choice (Donors or Recipients)
  if (input$group_choice == "Donors") {
    metadata <- metadata[metadata$donor_or_recipient == "Donor", ]
  } else if (input$group_choice == "Recipients") {
    metadata <- metadata[metadata$donor_or_recipient == "Recipient", ]
  }
  return(metadata)
})
observeEvent(input$plot_bttn, {
  req(selected_data(), input$metadata_x_axis, input$metadata_y_axis, input$metadata_fill_colour)
  data <- m_filtered_data()
  x_var <- input$metadata_x_axis
  y_var <- input$metadata_y_axis
  fill_var <- input$metadata_fill_colour
  # Convert "None" selections to NULL
  if (x_var == "None") x_var <- NULL
  if (y_var == "None") y_var <- NULL
  if (fill_var == "None") fill_var <- NULL
  # Reset error message
  invalid_text(NULL)
  # Generate plot based on selected plot type
  if (input$plot_type == "boxplot") {
    if (!is.null(x_var) && !is.null(y_var) && is.null(fill_var)) {
      summary_plot <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
        geom_boxplot(outlier.shape = NA, fill = "lightblue", color = "black") + 
        geom_jitter(width = 0.2, height = 0) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(hjust = 1)
        ) +
        labs(x = x_var, y = y_var)
      
    
      } else if (!is.null(fill_var) && !is.null(y_var)) {
      summary_plot <- ggplot(data, aes(
        x = interaction(!!sym(fill_var), !!sym(x_var), sep = " | "),
        y = !!sym(y_var)
      )) +
        geom_boxplot(outlier.shape = NA, fill = "lightblue", color = "black") + 
        geom_jitter(width = 0.2, height = 0) +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(hjust = 1)
        ) +
        labs(x = "Groups", y = y_var)
    
      } else {
      invalid_text("⚠ Boxplot requires both X and Y variables.")
    }
  } else if (input$plot_type == "barplot") {
    if (!is.null(x_var)) {
      if (!is.null(y_var)) {
        summary_plot <- ggplot(data, aes_string(x = x_var, y = y_var, fill = fill_var)) +
          geom_bar(stat = "identity", position = position_dodge()) +
          theme_minimal()
      } else {
        summary_plot <- ggplot(data, aes_string(x = x_var, fill = fill_var)) +
          geom_bar(position = position_dodge()) +
          theme_minimal()
      }
    } else {
      invalid_text("⚠ Barplot requires an X variable.")
    }
  }
  # Render the plot if no errors
  if (is.null(invalid_text())) {
    output$plot <- renderPlotly({
      ggplotly(summary_plot)
    })
    plot_shown(TRUE)
  } else {
    plot_shown(FALSE)
  }
})
observeEvent(selected_data(), {
  plot_shown(FALSE)
  invalid_text(NULL)
  output$plot <- renderPlotly(NULL)
})
# Render UI to display plot or messages
output$plot_ui <- renderUI({
  if (!is.null(invalid_text())) {
    return(tags$p(invalid_text(), style = "color: red; font-weight: bold; margin-bottom: 20px;"))
  }
  if (plot_shown()) {
    return(plotlyOutput("plot"))
  } else {
    show_dataset_message()
  }
})