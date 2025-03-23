# Mapping of taxa choices to strings in abundance table
taxa_choices <- list(
  "Kingdom"="k__", 
  "Phylum"="p__", 
  "Class"="c__", 
  "Order"="o__", 
  "Family"="f__", 
  "Genus"="g__", 
  "Species"="s__", 
  "SGBs"="t__")

# Mapping of user-friendly variable names to metadata column names
variable_mapping <- list(
  "Sex" = "sex",
  "Age" = "age",
  "S point" = "s_point",
  "Donor or Recipient" = "donor_or_recipient",
  "Pre- or Post-FMT" = "timepoint",
  "Response" = "response"
)

# Custom card CSS
my_card <- function(header = NULL, body = NULL, footer = NULL) {
  div(class = "custom-card",
      if (!is.null(header)) div(class = "custom-card-header", header),
      div(class = "custom-card-body", body),
      if (!is.null(footer)) div(class = "custom-card-footer", footer)
  )
}

# Function to remove donor or recipient samples
remove_samples <- function(database, samples_to_remove) {
  required_components <- c("metadata", "abundance", "enzyme_commissions", "genefamilies", 
                           "pathcoverage", "pathabundance")
  remove_indices <- which(database$metadata[["donor_or_recipient"]] == samples_to_remove)
  for (component in required_components) {
    database[[component]] <- database[[component]][-remove_indices, , drop = FALSE]
  }
  return(database)
}

# Functions to extract taxonomic level to analyze
extract_taxonomic_level <- function(taxa_strings, level_prefix) {
  sapply(taxa_strings, function(taxa) {
    match <- regmatches(taxa, regexpr(paste0(level_prefix, "[^|]*"), taxa))
    if (length(match) > 0) {
      sub(paste0(level_prefix), "", match)
    } else {
      NA
    }
  })
}

aggregate_taxa <- function(abundance_matrix, level_prefix) {
  # Extract the taxonomic levels from the column names
  taxa_strings <- colnames(abundance_matrix)
  taxonomic_levels <- extract_taxonomic_level(taxa_strings, level_prefix)
  # Remove taxa with NA taxonomic levels
  valid_indices <- !is.na(taxonomic_levels)
  abundance_matrix <- abundance_matrix[, valid_indices, drop = FALSE]  # Keep valid columns
  taxonomic_levels <- taxonomic_levels[valid_indices]
  # Aggregate abundance data by taxonomic level
  unique_levels <- unique(taxonomic_levels)
  # Initialize an empty matrix for the aggregated results
  aggregated_matrix <- matrix(0, nrow = nrow(abundance_matrix), ncol = length(unique_levels))
  # Loop over each unique taxa and sum the abundance values
  for (i in seq_along(unique_levels)) {
    taxa <- unique_levels[i]
    taxa_indices <- which(taxonomic_levels == taxa)
    aggregated_matrix[, i] <- rowSums(abundance_matrix[, taxa_indices, drop = FALSE], na.rm = TRUE)
  }
  # Set the column names and row names (samples)
  colnames(aggregated_matrix) <- unique_levels
  rownames(aggregated_matrix) <- rownames(abundance_matrix)
  return(aggregated_matrix)
}

# Function to generate relative abundance barplots with facet_wrap
generate_abuplot <- function(selected_data, feature, selected_features, split_by, split_by2) {
  data <- selected_data[[feature]][, selected_features, drop = FALSE]
  plot_data <- cbind(Sample = rownames(data), data)
  plot_data <- merge(plot_data, selected_data$metadata, by.x = "Sample", by.y = "Run")
  # Ensure numeric conversion for selected features
  plot_data[selected_features] <- sapply(plot_data[selected_features], as.numeric)
  # Reshape for plotting
  plot_data_long <- plot_data %>%
    pivot_longer(
      cols = all_of(selected_features),  # Columns corresponding to features
      names_to = "Feature",
      values_to = "Abundance"
    )
  # Dynamically define x-axis grouping
  if (split_by2 != "None" && split_by != split_by2) {
    plot_data_long <- plot_data_long %>%
      mutate(Group = interaction(!!sym(split_by2), !!sym(split_by), sep = " | "))
  } else {
    plot_data_long <- plot_data_long %>%
      mutate(Group = !!sym(split_by))
  }
  # Ensure split_by2 is a valid fill variable
  plot_data_long <- plot_data_long %>%
    mutate(FillGroup = if (split_by2 != "None") !!sym(split_by2) else "None")
  # Perform pairwise Wilcoxon test with BH correction
  pvalue_data <- plot_data_long %>%
    group_by(Feature) %>%
    summarise(
      PairwiseP = list(as.data.frame(as.table(pairwise.wilcox.test(Abundance, Group, p.adjust.method = "BH", exact = FALSE)$p.value)))
    ) %>%
    unnest(PairwiseP) %>%
    mutate(Comparison = paste(Var1, "vs.", Var2)) %>%
    rename(AdjustedPValue = Freq) %>%
    dplyr::select(Feature, Comparison, AdjustedPValue) %>%
    mutate(AdjustedPValue = round(AdjustedPValue, 4)) %>%
    arrange(AdjustedPValue)
  # Create significance labels based on p-value thresholds
  pvalue_data <- pvalue_data %>%
    mutate(Significance = case_when(
      AdjustedPValue < 0.001 ~ "***",
      AdjustedPValue < 0.01 ~ "**",
      AdjustedPValue < 0.05 ~ "*",
      TRUE ~ ""
    ))
  # Generate the barplot with facet_wrap
  barplot <- ggplot(plot_data_long, aes(x = !!sym(split_by), y = Abundance, fill = FillGroup)) +
    stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.8), width = 0.6) +  
    geom_point(
      position = if (split_by2 != "None") position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8) else position_jitter(width = 0.2),
      shape = 16, colour = "black"
    ) +
    labs(
      x = "Group",
      y = "Relative Abundance (CPM)",
      fill = if (split_by2 != "None") split_by2 else NULL
    ) +
    theme(
      axis.text.y = element_text(size = 8),  # Adjust y-axis text size for readability
      axis.text.x = element_text(size = 10),  # Adjust x-axis text size
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    theme_minimal() +
    facet_wrap(~Feature, scales = "free_y") + # Add facet wrap for multiple features
    scale_fill_brewer(palette = "Dark2")
  return(list(barplot, pvalue_data))
}

# Function to generate barplots with fold changes (functional profiling)
generate_fcplot <- function(selected_data, feature, selected_features, responder_filter) {
  # Subset data based on responder filter
  metadata <- selected_data$metadata
  if (responder_filter == "All Recipients") {
    metadata <- metadata %>% filter(donor_or_recipient == "Recipient")
  } else if (responder_filter != "All Recipients") {
    metadata <- metadata %>% filter(response == responder_filter)
  }
  # Extract abundance data
  features <- selected_data[[feature]][, selected_features, drop = FALSE]
  fc_data <- cbind(Sample = rownames(features), features)
  fc_data <- merge(fc_data, metadata, by.x = "Sample", by.y = "Run")
  # Ensure numeric conversion for selected features
  fc_data[selected_features] <- sapply(fc_data[selected_features], as.numeric)
  # Calculate log2 fold changes and p-values
  fc_data <- fc_data %>%
    pivot_longer(
      cols = all_of(selected_features),
      names_to = "Feature",
      values_to = "Abundance"
    ) %>%
    group_by(Feature) %>%
    summarise(
      Log2FoldChange = log2(
        # add a small pseudocount to the mean abundances to avoid division by 0
        (mean(Abundance[timepoint == "Post_FMT"], na.rm = TRUE) + 1e-3) /
          (mean(Abundance[timepoint == "Pre_FMT"], na.rm = TRUE) + 1e-3)
      ),
      PValue = wilcox.test(
        Abundance[timepoint == "Pre_FMT"],
        Abundance[timepoint == "Post_FMT"]
      )$p.value
    ) %>%
    # Round the values to 4 decimal places
    mutate(
      Log2FoldChange = ifelse(is.na(Log2FoldChange), 0, Log2FoldChange),
      Log2FoldChange = round(Log2FoldChange, 4),
      PValue = round(PValue, 4),
      Significance = case_when(
        PValue < 0.001 ~ "***",
        PValue < 0.01 ~ "**",
        PValue < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
  # Generate the fold change plot
  fc_plot <- ggplot(fc_data, aes(x = Feature, y = Log2FoldChange)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = Significance), hjust = -3) +
    coord_flip() + 
    geom_hline(yintercept=0) +
    theme_minimal() +
    labs(
      y = "Log2 Fold Change",
      x = NULL,
      caption = "Asterisk (*) indicates p < 0.05"
    ) +
    theme(
      axis.text.x = element_text(hjust = 1),
      axis.text.y = element_text(size = 10)
    )
  fc_table <- as.data.frame(fc_data) %>%
    select(Feature, Log2FoldChange, PValue) %>%
    arrange(PValue) 
  return(list(fc_plot, fc_table))
}

# Memoised versions
memoised_remove_samples <- memoise(remove_samples)
memoised_aggregate_taxa <- memoise(aggregate_taxa)
memoised_generate_abuplot <- memoise(generate_abuplot)
memoised_generate_fcplot <- memoise(generate_fcplot)