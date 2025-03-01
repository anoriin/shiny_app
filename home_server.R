# Image rendering
output$map_img1 <- renderImage({ list(src = "www/mimic.png", width = "100%", height = "200px") }, deleteFile = FALSE)
output$map_img2 <- renderImage({ list(src = "www/pittsburgh.png", width = "100%", height = "200px") }, deleteFile = FALSE)
output$map_img3 <- renderImage({ list(src = "www/baruch.png", width = "100%", height = "200px") }, deleteFile = FALSE)

# Set your Google Drive folder ID
folder_id <- "11t-wgGpliwkwEDB79QaTHGiJYokrcwPJ"

# Function to get file ID from Google Drive
get_drive_file_id <- function(file_name) {
  files <- drive_ls(as_id(folder_id))  # List files in the folder
  match <- files[files$name == file_name, ]  # Find matching file
  if (nrow(match) == 1) {
    return(match$id)  # Return file ID
  } else {
    return(NULL)  # File not found
  }
}

# Function to download dataset if missing
download_if_needed <- function(file_name) {
  file_path <- paste0("data/", file_name)  # Local storage path
  if (!file.exists(file_path)) {
    file_id <- get_drive_file_id(file_name)
    if (!is.null(file_id)) {
      drive_download(as_id(file_id), path = file_path, overwrite = TRUE)
      message(paste("Downloaded", file_name, "from Google Drive"))
    } else {
      message(paste("Error: File", file_name, "not found in Google Drive"))
    }
  }
  return(file_path)
}

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

# Function to determine the best matching file
get_best_matching_file <- function(selected) {
  selected <- sort(tolower(selected))
  pattern <- paste0("processed_", paste(selected, collapse = "_"), "_database.RData")
  best_match <- available_files[grepl(pattern, available_files)]
  if (length(best_match) > 0) {
    file_path <- download_if_needed(best_match)  # Download if missing
    return(file_path)
  } else {
    return(NULL)
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