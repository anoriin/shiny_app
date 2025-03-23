# Track whether the user has requested a plot
plot_requested8 <- reactiveVal(FALSE)
plot_requested9 <- reactiveVal(FALSE)
plot_requested10 <- reactiveVal(FALSE)
plot_requested11 <- reactiveVal(FALSE)
plot_requested12 <- reactiveVal(FALSE)
plot_requested13 <- reactiveVal(FALSE)
plot_requested14 <- reactiveVal(FALSE)
table_requested1 <- reactiveVal(FALSE)
table_requested2 <- reactiveVal(FALSE)
table_requested3 <- reactiveVal(FALSE)

observeEvent(selected_data(), {
  plot_requested8(FALSE)
  plot_requested9(FALSE)
  plot_requested10(FALSE)
  plot_requested11(FALSE)
  plot_requested12(FALSE)
  plot_requested13(FALSE)
  plot_requested14(FALSE)
  table_requested1(FALSE)
  table_requested2(FALSE)
  table_requested3(FALSE)
  
  # Explicitly remove plots by setting output to NULL
  output$genesabu_plot <- renderPlotly(NULL)
  output$genesabu_table <- renderDataTable(NULL)
  output$genesfc_plot <- renderPlotly(NULL)
  output$genesfc_table <- renderDataTable(NULL)
  output$genes_diffabundance_table <- renderDataTable(NULL)
  
  output$pwabu_plot <- renderPlotly(NULL)
  output$pwabu_table <- renderDataTable(NULL)
  output$pw_diffabundance_table <- renderDataTable(NULL)
  output$pwfc_plot <- renderPlotly(NULL)
  output$pwfc_table <- renderDataTable(NULL)
  output$pwcov_plot <- renderPlot(NULL)
  output$pwcov_table <- renderDataTable(NULL)
  
  output$enzymesabu_plot <- renderPlotly(NULL)
  output$enzymesabu_table <- renderDataTable(NULL)
  output$ec_diffabundance_table <- renderDataTable(NULL)
  output$enzymesfc_plot <- renderPlotly(NULL)
  output$enzymesfc_table <- renderDataTable(NULL)
})

# Dataset selection messages
output$warning8 <- renderUI({ if (!plot_requested8()) show_dataset_message() })
output$warning9 <- renderUI({ if (!plot_requested9()) show_dataset_message() })
output$warning10 <- renderUI({ if (!plot_requested10()) show_dataset_message() })
output$warning11 <- renderUI({ if (!plot_requested11()) show_dataset_message() })
output$warning12 <- renderUI({ if (!plot_requested12()) show_dataset_message() })
output$warning13 <- renderUI({ if (!plot_requested13()) show_dataset_message() })
output$warning14 <- renderUI({ if (!plot_requested14()) show_dataset_message() })
output$warning_genes_diffabundance <- renderUI({ if (!table_requested1()) show_dataset_message() })
output$warning_pw_diffabundance <- renderUI({ if (!table_requested2()) show_dataset_message() })
output$warning_ec_diffabundance <- renderUI({ if (!table_requested3()) show_dataset_message() })

# Function to filter data based on subset selection
filter_data <- function(input_subset) {
  reactive({
    req(selected_data())  # Ensure the data is loaded
    if (input_subset() == "Donors") {
      return(memoised_remove_samples(selected_data(), "Recipient"))
    } else if (input_subset() == "Recipients") {
      return(memoised_remove_samples(selected_data(), "Donor"))
    } else {
      return(selected_data())  # No filtering
    }
  })
}

# Function to dynamically update selection input based on selected dataset
update_dynamic_select <- function(input_id, data_key) {
  observe({
    req(selected_data())  # Ensure the data is loaded
    choices <- colnames(selected_data()[[data_key]])
    updateSelectizeInput(session, input_id, choices = choices, server = TRUE)
  })
}

# Function to dynamically update metadata variable selection
update_metadata_selection <- function(input_subset, input_split_by) {
  observe({
    req(input_subset())  # Ensure input_subset is available
    output_choices <- case_when(
      input_subset() == "All" ~ variable_mapping[names(variable_mapping) != "Age"],
      input_subset() == "Donors" ~ variable_mapping[names(variable_mapping) == "Sex"],
      input_subset() == "Recipients" ~ variable_mapping[names(variable_mapping) != "Age"]
    )
    updateSelectInput(session, input_split_by, choices = output_choices)
  })
}

# Function to dynamically update second metadata variable selection
update_metadata_selection2 <- function(input_subset, input_split_by2) {
  observe({
    req(input_subset())  # Ensure input_subset is available
    output_choices <- switch(
      input_subset(),
      "All" = c("None", variable_mapping[names(variable_mapping) != "Age"]),
      "Donors" = c("None", variable_mapping[names(variable_mapping) == "Sex"]),
      "Recipients" = c("None", variable_mapping[names(variable_mapping) != "Age"])
    )
    updateSelectInput(session, input_split_by2, choices = output_choices)
  })
}

# Function to generate and render abundance plots
generate_abundance_plot <- function(input_trigger, input_subset, data_key, input_selected, input_split_by, input_split_by2, output_plot, output_table) {
  observeEvent(input_trigger(), {
    if (output_plot == "genesabu_plot") { plot_requested8(TRUE) }
    if (output_plot == "pwabu_plot") { plot_requested10(TRUE) }
    if (output_plot == "enzymesabu_plot") { plot_requested13(TRUE) }
    req(selected_data(), input_selected(), input_split_by(), input_split_by2())  # Ensure necessary inputs are available
    selected_data <- filter_data(input_subset)()  # Get subsetted data
    split_by <- input_split_by()
    split_by2 <- input_split_by2()
    selected_items <- input_selected()
    # Generate the abundance plot
    abundance_result <- memoised_generate_abuplot(selected_data, data_key, selected_items, split_by, split_by2)
    abundance_plot <- abundance_result[[1]]
    abundance_table <- abundance_result[[2]]
    # Render the plot
    output[[output_plot]] <- renderPlotly({
      ggplotly(abundance_plot)
    })
    # Render the p-value table
    output[[output_table]] <- renderDataTable({
      req(abundance_table)
      abundance_table <- as.data.frame(abundance_table)
      datatable(abundance_table, options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}

# Function to generate and render fold change plots and tables
generate_fc_plot <- function(input_trigger, data_key, input_selected, input_filter, output_plot, output_table) {
  observeEvent(input_trigger(), {
    if (output_plot == "genesfc_plot") { plot_requested9(TRUE) }
    if (output_plot == "pwfc_plot") { plot_requested11(TRUE) }
    if (output_plot == "enzymesfc_plot") { plot_requested14(TRUE) }
    req(selected_data(), input_selected(), input_filter())  # Ensure necessary inputs are available
    selected_data <- selected_data()
    selected_fc <- input_selected() 
    responder_filter <- input_filter()
    fc_result <- memoised_generate_fcplot(selected_data, data_key, selected_fc, responder_filter)  # Generate FC plot
    fc_plot <- fc_result[[1]]
    fc_table <- fc_result[[2]]
    # Render the FC plot
    output[[output_plot]] <- renderPlotly({
      ggplotly(fc_plot)
    })
    # Render FC table
    output[[output_table]] <- renderDataTable({
      req(fc_table)
      fc_table <- as.data.frame(fc_table)
      datatable(fc_table, options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}
# Function to generate and render coverage pie plots
generate_coverage_plot <- function(input_trigger, input_subset, data_key, input_selected, output_plot, output_table) {
  observeEvent(input_trigger(), {
    plot_requested12(TRUE)
    req(filter_data(input_subset)(), input_selected())  # Ensure necessary inputs are available
    selected_data <- filter_data(input_subset)()  # Get filtered data
    selected_pathways <- input_selected()
    # Convert from wide to long format
    long_data <- selected_data[[data_key]] %>%
      pivot_longer(cols = all_of(selected_pathways), names_to = "pathway", values_to = "coverage")
    # Categorize coverage values
    long_data <- long_data %>%
      mutate(Category = case_when(
        coverage >= 0.95 ~ "High Coverage",
        coverage <= 0.05 ~ "Low Coverage",
        TRUE ~ "Intermediate Coverage"
      ))
    # Count occurrences of each category per pathway
    category_counts <- long_data %>%
      group_by(pathway, Category) %>%
      summarise(count = n(), .groups = "drop")
    # Generate pie charts
    pwcov_pieplot <- ggplot(category_counts, aes(x = "", y = count, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      facet_wrap(~pathway, scales = "free") +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 10, face = "bold")
      )
    # Render plot
    output[[output_plot]] <- renderPlot({
      pwcov_pieplot
    })
    # Compute summary statistics
    summary_stats <- long_data %>%
      group_by(pathway) %>%
      summarise(Mean = mean(coverage, na.rm = TRUE), Median = median(coverage, na.rm = TRUE), .groups = "drop")
    # Render summary statistics table
    output[[output_table]] <- renderDataTable({
      datatable(summary_stats, options = list(pageLength = 5, scrollX = TRUE))
    })
  })
}

generate_diffabundance_table <- function(input_trigger, data_key, covariates, output_table) {
  observeEvent(input_trigger(), {
    if (output_table == "genes_diffabundance_table") { table_requested1(TRUE) }
    if (output_table == "pw_diffabundance_table") { table_requested2(TRUE) }
    if (output_table == "ec_diffabundance_table") { table_requested3(TRUE) }
    data <- memoised_remove_samples(selected_data(), "Donor")
    data$metadata[["Response + Timepoint"]] <- 
      interaction(data$metadata$response, data$metadata$timepoint)
    metadata <- data$metadata[, covariates()]
    metadata <- as.data.frame(metadata)
    rownames(metadata) <- data$metadata$Run
    fit_data <- Maaslin2(
      data[[data_key]], as.data.frame(metadata),
      output = 'output',
      standardize = FALSE, cores = 4
    )
    output$output_table <- renderDataTable({
      fit_data$results
    })
  })
}

# ------ APPLY TO ALL DATA TYPES (GENES, PATHWAYS, ENZYMES) ------

# GENE FAMILIES
update_dynamic_select("selected_genes_abu", "genefamilies")
update_metadata_selection(reactive(input$genes_subset_data), "genes_split_by")
update_metadata_selection2(reactive(input$genes_subset_data), "genes_split_by2")
generate_abundance_plot(reactive(input$generate_genesabuplot), reactive(input$genes_subset_data), "genefamilies", reactive(input$selected_genes_abu), reactive(input$genes_split_by), reactive(input$genes_split_by2), "genesabu_plot", "genesabu_table")
update_dynamic_select("selected_genes_fc", "genefamilies")
generate_fc_plot(reactive(input$generate_genesfcplot), "genefamilies", reactive(input$selected_genes_fc), reactive(input$genes_responder_filter), "genesfc_plot", "genesfc_table")
generate_diffabundance_table(reactive(input$generate_genes_diffabundance_table), "genefamilies", reactive(input$genes_covariates), "genes_diffabundance_table")

# PATHWAYS
update_dynamic_select("selected_pathways_abu", "pathabundance")
update_metadata_selection(reactive(input$pathways_subset_data), "pathways_split_by")
update_metadata_selection2(reactive(input$pathways_subset_data), "pathways_split_by2")
generate_abundance_plot(reactive(input$generate_pwabuplot), reactive(input$pathways_subset_data), "pathabundance", reactive(input$selected_pathways_abu), reactive(input$pathways_split_by), reactive(input$pathways_split_by2), "pwabu_plot", "pwabu_table")
update_dynamic_select("selected_pathways_fc", "pathabundance")
generate_fc_plot(reactive(input$generate_pwfcplot), "pathabundance", reactive(input$selected_pathways_fc), reactive(input$pathways_responder_filter), "pwfc_plot", "pwfc_table")
generate_diffabundance_table(reactive(input$generate_pw_diffabundance_table), "pathabundance", reactive(input$pw_covariates), "pw_diffabundance_table")
# PATHWAY COVERAGE
update_dynamic_select("selected_pathways_cov", "pathcoverage")
generate_coverage_plot(reactive(input$generate_pwcovplot), reactive(input$pathwayscov_subset_data), "pathcoverage", reactive(input$selected_pathways_cov), "pwcov_plot", "pwcov_table")

# ENZYMES
update_dynamic_select("selected_enzymes_abu", "enzyme_commissions")
update_metadata_selection(reactive(input$enzymes_subset_data), "enzymes_split_by")
update_metadata_selection2(reactive(input$enzymes_subset_data), "enzymes_split_by2")
generate_abundance_plot(reactive(input$generate_enzymesabuplot), reactive(input$enzymes_subset_data), "enzyme_commissions", reactive(input$selected_enzymes_abu), reactive(input$enzymes_split_by), reactive(input$enzymes_split_by2), "enzymesabu_plot", "enzymesabu_table")
update_dynamic_select("selected_enzymes_fc", "enzyme_commissions")
generate_fc_plot(reactive(input$generate_enzymesfcplot), "enzyme_commissions", reactive(input$selected_enzymes_fc), reactive(input$enzymes_responder_filter), "enzymesfc_plot", "enzymesfc_table")
generate_diffabundance_table(reactive(input$generate_ec_diffabundance_table), "enzyme_commissions", reactive(input$ec_covariates), "ec_diffabundance_table")

