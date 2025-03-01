# Image rendering
output$map_img1 <- renderImage({ list(src = "docs/www/mimic.png", width = "100%", height = "200px") }, deleteFile = FALSE)
output$map_img2 <- renderImage({ list(src = "docs/www/pittsburgh.png", width = "100%", height = "200px") }, deleteFile = FALSE)
output$map_img3 <- renderImage({ list(src = "docs/www/baruch.png", width = "100%", height = "200px") }, deleteFile = FALSE)

# Available dataset files
available_files <- c(
  "processed_baruch_database.RData",
  "processed_baruch_davar_database.RData",
  "processed_davar_database.RData",
  "processed_baruch_routy_database.RData",
  "processed_baruch_davar_routy_database.RData",
  "processed_routy_database.RData",
  "processed_davar_routy_database.RData"
)

# Reactive value to store selected datasets
selected_cards <- reactiveVal(NULL)  # Start as an empty vector

observe({
  runjs('
    $(document).on("click", ".clickable-card", function() {
      var card_id = $(this).attr("id");  
      $(this).toggleClass("selected");
      // Ensure current_selection is always an array
      var current_selection = Shiny.shinyapp.$inputValues["clicked_cards"];
      if (!Array.isArray(current_selection)) {
        current_selection = [];
      }
      if (current_selection.includes(card_id)) {
        current_selection = current_selection.filter(id => id !== card_id); 
      } else {
        current_selection.push(card_id);
      }
      // Explicitly send "none" when all cards are deselected
      Shiny.setInputValue("clicked_cards", current_selection.length > 0 ? current_selection : "none", {priority: "event"});
    });
  ')
})

# Observe changes in selected cards
observeEvent(input$clicked_cards, {
  new_selection <- input$clicked_cards
  if (identical(new_selection, "none")) { 
    selected_cards(NULL)
  } else {
    selected_cards(new_selection)  # Update with selected items
  }
})

# Determine the best matching file based on selected cards
get_best_matching_file <- function(selected) {
  selected <- sort(tolower(selected))  # Sort to ensure alphabetical order
  pattern <- paste0("processed_", paste(selected, collapse = "_"), "_database.RData")
  # Find the most specific available file
  best_match <- available_files[grepl(pattern, available_files)]
  if (length(best_match) > 0) {
    return(paste0("data/", best_match))  # Return full path to matching file
  } else {
    return(NULL)  # No matching file found
  }
}

# Load the best matching dataset
selected_data <- reactive({
  if (length(selected_cards()) == 0) {
    return(NULL)  # No selection, return NULL
  }
  file_path <- get_best_matching_file(selected_cards())
  validate(need(!is.null(file_path) && file.exists(file_path), "No matching dataset found."))  # Validate that the dataset exists
  # Load the dataset from the file
  loaded_env <- new.env()
  load(file_path, envir = loaded_env)
  dataset_name <- ls(loaded_env)[1]  # Get the name of the dataset
  dataset <- get(dataset_name, envir = loaded_env)  # Extract the dataset
  return(dataset)  # Return the loaded dataset
})

# Dataset selection message
show_dataset_message <- function() {
  if (is.null(selected_data())) {
    return(tags$p("⚠ Please select at least one dataset to proceed.", 
                  style = "color: red; font-weight: bold; margin-bottom: 20px;"))
  } else {
    return(tagList(
      tags$p(paste("You have selected the following dataset(s):", paste(selected_cards(), collapse = ", ")),
             style = "color: red; font-weight: bold;"),
      tags$p("Please proceed with plotting.", 
             style = "color: red; font-weight: bold; margin-bottom: 20px;")
    ))
  }
}

output$warning1 <- renderUI({ show_dataset_message() })