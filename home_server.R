# Image rendering
output$map_img1 <- renderImage({ list(src = file.path(getwd(), "www/mimic.png"), width = "100%", height = "200px") }, deleteFile = FALSE)
output$map_img2 <- renderImage({ list(src = file.path(getwd(), "www/pittsburgh.png"), width = "100%", height = "200px") }, deleteFile = FALSE)
output$map_img3 <- renderImage({ list(src = file.path(getwd(), "www/baruch.png"), width = "100%", height = "200px") }, deleteFile = FALSE)

# Define the local dataset directory
data_dir <- file.path(getwd(), "data")

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

# Function to check if dataset file exists
get_existing_file_path <- function(file_name) {
  file_path <- file.path(data_dir, file_name)
  if (file.exists(file_path)) {
    return(file_path)
  } else {
    return(NULL)
  }
}

# Reactive value to store selected datasets
selected_cards <- reactiveVal(NULL)

observe({
  runjs('
    $(document).on("click", ".clickable-card", function() {
      var card_id = $(this).attr("id");  
      $(this).toggleClass("selected");
      var current_selection = Shiny.shinyapp.$inputValues["clicked_cards"];
      if (!Array.isArray(current_selection)) {
        current_selection = [];
      }
      if (current_selection.includes(card_id)) {
        current_selection = current_selection.filter(id => id !== card_id); 
      } else {
        current_selection.push(card_id);
      }
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
    selected_cards(new_selection)
  }
})

# Function to determine the best matching file
get_best_matching_file <- function(selected) {
  selected <- sort(tolower(selected))
  pattern <- paste0("processed_", paste(selected, collapse = "_"), "_database.RData")
  best_match <- available_files[grepl(pattern, available_files)]
  if (length(best_match) > 0) {
    file_path <- get_existing_file_path(best_match)
    return(file_path)
  } else {
    return(NULL)
  }
}

# Load the best matching dataset
selected_data <- reactive({
  if (length(selected_cards()) == 0) {
    return(NULL)
  }
  file_path <- get_best_matching_file(selected_cards())
  validate(need(!is.null(file_path) && file.exists(file_path), "No matching dataset found."))
  loaded_env <- new.env()
  load(file_path, envir = loaded_env)
  dataset_name <- ls(loaded_env)[1]
  dataset <- get(dataset_name, envir = loaded_env)
  return(dataset)
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
