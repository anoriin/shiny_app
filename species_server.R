plot_requested_ss1 <- reactiveVal(FALSE)
plot_requested_ss2 <- reactiveVal(FALSE)
plot_requested_ad <- reactiveVal(FALSE)
plot_requested_bd <- reactiveVal(FALSE)
plot_requested_lefse <- reactiveVal(FALSE)

observeEvent(selected_data(), {
  plot_requested_ss1(FALSE)
  plot_requested_ss2(FALSE)
  plot_requested_ad(FALSE)
  plot_requested_bd(FALSE)
  plot_requested_lefse(FALSE)
  
  # Explicitly remove plots by setting output to NULL
  output$top_taxa_plot <- renderPlotly(NULL)
  output$relabu_plot <- renderPlotly(NULL)
  
  output$a_diversity_plot <- renderPlotly(NULL)
  output$wilcoxon_tests <- renderTable(NULL)
  
  output$b_diversity_plot <- renderPlotly(NULL)
  output$permanova_tests <- renderTable(NULL)
 
  output$lefse_plot <- renderPlotly(NULL)
  output$lefse_table <- renderDataTable(NULL)
})

# Dataset selection messages
output$warning3 <- renderUI({ if (!plot_requested_ss1()) show_dataset_message() })
output$warning4 <- renderUI({ if (!plot_requested_ss2()) show_dataset_message() })
output$warning5 <- renderUI({ if (!plot_requested_ad()) show_dataset_message() })
output$warning6 <- renderUI({ if (!plot_requested_bd()) show_dataset_message() })
output$warning7 <- renderUI({ if (!plot_requested_lefse()) show_dataset_message() })

# SUMMARY STATISTICS
# Barplot 1
# Change metadata variable selection dynamically 
species_choices <- reactive({
  req(input$species_subset_data)
  if (input$species_subset_data == "All") {
    c("None", variable_mapping[names(variable_mapping) != "Age"])
  } else if (input$species_subset_data == "Donors") {
    c("None", variable_mapping[names(variable_mapping) == "Sex"])
  } else if (input$species_subset_data == "Recipients") {
    c("None", variable_mapping[names(variable_mapping) != "Age"])
  }
})
observeEvent(input$species_subset_data, {
  updateSelectInput(session, "ss_split_by", choices = species_choices())
})
species_filtered_data <- reactive({
  req(selected_data())
  if (input$species_subset_data == "Donors") {
    remove_samples(selected_data(), "Recipient")
  } else if (input$species_subset_data == "Recipients") {
    remove_samples(selected_data(), "Donor")
  } else {
    selected_data()
  }
})
observeEvent(input$generate_summary1, {
  req(species_filtered_data(), input$taxa_level, input$top_taxa, input$ss_split_by)
  plot_requested_ss1(TRUE)
  data <- species_filtered_data()
  metadata <- data$metadata
  taxa_level <- input$taxa_level
  top_taxa <- input$top_taxa
  ss_split_by <- input$ss_split_by
  # Aggregate taxa
  taxa_data <- aggregate_taxa(data$abundance, taxa_level)
  # Compute top taxa (overall mean abundance, before grouping)
  top_taxa_values <- colMeans(taxa_data, na.rm = TRUE) %>%
    sort(decreasing = TRUE) %>%
    head(top_taxa)
  # Subset taxa_data to only include the top taxa
  taxa_data_top <- as.data.frame(taxa_data[, names(top_taxa_values), drop = FALSE])
  if (ss_split_by == "None") {
    # No grouping, calculate overall mean
    taxa_long <- taxa_data_top %>%
      pivot_longer(cols = everything(), names_to = "Taxa", values_to = "Abundance") %>%
      group_by(Taxa) %>%
      summarize(Mean_Abundance = mean(Abundance, na.rm = TRUE), .groups = "drop") %>%
      mutate(Taxa = factor(Taxa, levels = names(top_taxa_values)))  # Ensure correct order
    # Plot without fill (single group)
    top_taxa_barplot <- ggplot(taxa_long, aes(x = Taxa, y = Mean_Abundance)) +
      geom_bar(stat = "identity", fill = "lightblue") +  # Single color since no groups
      theme_minimal() +
      labs(title = "Top Taxa by Mean Relative Abundance",
           x = "Taxa",
           y = "Mean Relative Abundance (/100)") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  } else {
    # Use grouping variable from metadata
    taxa_data_top <- cbind(taxa_data_top, Group = metadata[[ss_split_by]])
    # Convert to long format for ggplot
    taxa_long <- taxa_data_top %>%
      pivot_longer(-Group, names_to = "Taxa", values_to = "Abundance") %>%
      group_by(Taxa, Group) %>%
      summarize(Mean_Abundance = mean(Abundance, na.rm = TRUE), .groups = "drop") %>%
      mutate(Taxa = factor(Taxa, levels = names(top_taxa_values)))  # Ensure correct order
    # Plot with fill (grouped bars)
    top_taxa_barplot <- ggplot(taxa_long, aes(x = Taxa, y = Mean_Abundance, fill = Group)) +
      geom_bar(stat = "identity", position = "dodge") +  # Separate bars by group
      theme_minimal() +
      labs(title = "Top Taxa by Mean Relative Abundance (Grouped)",
           x = "Taxa",
           y = "Mean Relative Abundance (/100)") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }
  output$top_taxa_plot <- renderPlotly({
    ggplotly(top_taxa_barplot)
  })
})
# Barplot 2
# Server logic for dynamically updating the selected taxa based on the selected taxonomic level
observe({
  req(input$taxa_level2)  # Ensure taxa level is selected
  if (!is.null(selected_data())) {
    data <- selected_data()  # Load the data
    # Aggregate taxa data based on the selected taxonomic level
    taxa_data <- aggregate_taxa(data$abundance, input$taxa_level2)
    # Update the selectize input choices with the columns of the aggregated taxa data
    updateSelectizeInput(session, "selected_taxa", choices = colnames(taxa_data), server = TRUE) 
  }
})
observeEvent(input$generate_summary2, {
  req(selected_data(), input$taxa_level2, input$species2_subset_data, input$selected_taxa, input$split_by, input$split_by2)
  plot_requested_ss2(TRUE)
  data <- filter_data(reactive(input$species2_subset_data))()
  taxa_level2 <- input$taxa_level2
  selected_taxa <- input$selected_taxa
  split_by <- input$split_by
  split_by2 <- input$split_by2
  # Aggregate taxa
  taxa_data <- aggregate_taxa(data$abundance, taxa_level2)
  # Subset taxa data to include only the selected taxa
  taxa_data <- taxa_data[, colnames(taxa_data) %in% selected_taxa, drop = FALSE]
  # Create relative abundance barplot
  relabu_result <- generate_abuplot(
    selected_data = list(abundance = taxa_data, metadata = data$metadata),
    "abundance",
    selected_taxa,
    split_by,
    split_by2
  )
  relabu_barplot <- relabu_result[[1]]
  relabu_table <- relabu_result[[2]]
  output$relabu_plot <- renderPlotly({
    ggplotly(relabu_barplot)
  })
  output$relabu_table <- renderDataTable({
    req(relabu_table)
    relabu_table <- as.data.frame(relabu_table)
    datatable(relabu_table, options = list(pageLength = 10, scrollX = TRUE))
  })
})

# ALPHA DIVERSITY PLOT
observeEvent(input$refresh_alpha, {
  req(input$adiversity_taxa_level, input$ad_subset_data, input$x_axis, input$diversity_index, input$colour_by)
  plot_requested_ad(TRUE)
  adiversity_data <- filter_data(reactive(input$ad_subset_data))()
  adiversity_taxa_level <- input$adiversity_taxa_level
  x_axis <- input$x_axis
  diversity_index <- input$diversity_index
  colour_by <- input$colour_by
  adjustment_method <- input$adjustment_method
  adiversity_taxa_data <- aggregate_taxa(adiversity_data$abundance, adiversity_taxa_level)
  shannon <- diversity(adiversity_taxa_data, index = "shannon")
  simpson <- diversity(adiversity_taxa_data, index = "simpson")
  invsimpson <- diversity(adiversity_taxa_data, index = "invsimpson")
  observed <- observed <- rowSums(adiversity_taxa_data > 0)
  diversity_results <- data.frame(
    Sample = adiversity_data$metadata$Run,
    Shannon = shannon,
    Simpson = simpson,
    InvSimpson = invsimpson,
    Observed = observed
  )
  row.names(diversity_results) <- diversity_results$Sample
  diversity_results$Sample <- NULL
  if (colour_by != "None") {
    aes_params <- aes(
      x = .data[[x_axis]],
      y = diversity_results[[diversity_index]],
      color = .data[[colour_by]],
      group = .data[[colour_by]]
    )
  } else {
    aes_params <- aes(
      x = .data[[x_axis]],
      y = diversity_results[[diversity_index]]
    )
  }
  alpha_diversity_plot <- ggplot(adiversity_data$metadata, aes_params) +
                                  geom_point(na.rm=TRUE, position=position_dodge(width=0.2), size = 1.5) +
                                  labs(
                                    x = input$x_axis,
                                    y = paste(input$diversity_index, "Diversity Index"),
                                    color = if (colour_by != "None") input$colour_by else NULL                                  ) +
                                  theme_minimal() +
                                  theme(
                                    axis.title.x = element_text(size = 14),
                                    axis.title.y = element_text(size = 14)
                                  )
  output$a_diversity_plot <- renderPlotly({
    ggplotly(alpha_diversity_plot)
  })
  output$wilcoxon_tests <- renderTable({
    result <- pairwise.wilcox.test(
      diversity_results[[diversity_index]],
      adiversity_data$metadata[[x_axis]],
      p.adjust.method = adjustment_method
    )
    p_value_df <- as.data.frame(as.table(result$p.value))
    data.frame(
      Comparison = paste(p_value_df$Var1, p_value_df$Var2, sep = " vs "),
      AdjustedP_Value = p_value_df$Freq
    )
  })
})

# BETA DIVERSITY
observeEvent(input$refresh_beta, {
  req(input$bdiversity_taxa_level, input$bd_subset_data, input$method, input$k, input$colour_by2, input$shape_by)
  plot_requested_bd(TRUE)
  bdiversity_data <- filter_data(reactive(input$bd_subset_data))()
  method <- input$method
  k <- input$k
  colour_by2 <- input$colour_by2
  shape_by <- input$shape_by
  bdiversity_taxa_level <- input$bdiversity_taxa_level
  taxa_data <- aggregate_taxa(bdiversity_data$abundance, bdiversity_taxa_level)
  distance_matrix <- vegdist(taxa_data, method = method)
  metaMDS_result <- metaMDS(distance_matrix, distance = method, k = k, trymax = 100)
  beta_diversity_data <- data.frame(PC1 = metaMDS_result$points[, 1], PC2 = metaMDS_result$points[, 2])
  metadata <- bdiversity_data$metadata
  if (shape_by != "None") {
    aes_params <- aes(
      x = beta_diversity_data$PC1,
      y = beta_diversity_data$PC2,
      color = .data[[colour_by2]],
      shape = .data[[shape_by]]
    )
  } else {
    aes_params <- aes(
      x = beta_diversity_data$PC1,
      y = beta_diversity_data$PC2,
      color = .data[[colour_by2]]
    )
  }
  beta_diversity_plot <- ggplot(metadata, aes_params) +
    geom_point(na.rm = TRUE, size = 2, alpha = 0.7) +
    stat_ellipse() + 
    geom_text(aes(label = patient), nudge_y = 0.1, size = 2) +  # Add text labels above points
    labs(x = "Axis 1", y = "Axis 2") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    )
  output$b_diversity_plot <- renderPlotly({
    ggplotly(beta_diversity_plot)
  })
  adonis_formula <- as.formula(paste("distance_matrix ~", colour_by2))
  permanova_result <- adonis2(adonis_formula, data = metadata, permutations = 999)
  permanova_result_table <- as.data.frame(permanova_result)
  output$permanova_tests <- renderTable({
    permanova_result_table
  })
})

# LEfSe
no_donor_data <- reactive({
  remove_samples(selected_data(), "Donor")
})
observeEvent(input$run, {
  req(no_donor_data(), input$lefse_taxa_level, input$class, input$subclass, input$kw_alpha, input$wilcoxon_alpha, input$lda)
  plot_requested_lefse(TRUE)
  data <- no_donor_data()
  class <- input$class
  subclass <- input$subclass
  kw_alpha <- input$kw_alpha
  wilcoxon_alpha <- input$wilcoxon_alpha
  lda <- input$lda
  lefse_taxa_level <- input$lefse_taxa_level
  taxa_data <- aggregate_taxa(data$abundance, lefse_taxa_level)
  otumat <- t(taxa_data)
  SE <- SummarizedExperiment(assays = list(counts = otumat), colData = data.frame(data$metadata))
  lefse_data <- relativeAb(SE)
  if (subclass != "None") {
    res <- lefser(
      lefse_data,
      kruskal.threshold = kw_alpha,
      wilcox.threshold = wilcoxon_alpha,
      lda.threshold = lda,
      groupCol = class,
      blockCol = subclass
    )
  } else if (subclass == "None") {
    res <- lefser(
      lefse_data,
      kruskal.threshold = kw_alpha,
      lda.threshold = lda,
      groupCol = class
    )
  }
  lefse_barplot <- lefserPlot(res)
  output$lefse_plot <- renderPlotly({
    lefse_barplot
  })
  # Create a table of results (assuming res is a data frame or tibble)
  lefse_table <- res %>%
    select(Feature = Names, Score = scores)
  output$lefse_table <- renderDataTable({
    lefse_table
  })
})
