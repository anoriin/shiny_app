# === IMAGE RENDERING ===
output$map_img1 <- renderImage({ list(src = "www/mimic.png", width = "100%", height = "200px") }, deleteFile = FALSE)
output$map_img2 <- renderImage({ list(src = "www/pittsburgh.png", width = "100%", height = "200px") }, deleteFile = FALSE)
output$map_img3 <- renderImage({ list(src = "www/baruch.png", width = "100%", height = "200px") }, deleteFile = FALSE)

# === DATASET SETUP ===
data_dir <- file.path(getwd(), "data")
dir.create(data_dir, showWarnings = FALSE)  # Ensure the directory exists

file_links <- list(
  "processed_baruch_database.RData"         = "1-7AEh855SrgBoBY-AmOLybrF1cDRkk3G", 
  "processed_baruch_davar_database.RData"   = "1nZ7IgNEdl1rP-cQ7kgsAZZ5iqj9VrPYL",
  "processed_davar_database.RData"          = "1XJDrsUfjxdjT7HX-f1uWw6fLDAQlSVxE",
  "processed_baruch_routy_database.RData"   = "18CjQJ_1F4VOY4MEPHIETxPcpi9HMYh8P",
  "processed_baruch_davar_routy_database.RData" = "1uDAeRuhH8mslBpMUQaDPqUmJPdg82Ee3",
  "processed_routy_database.RData"          = "1UKApBMD1WsENx2KjgmID0PNGYx_58TYB",
  "processed_davar_routy_database.RData"    = "1cAiDeokBuJIPFHpb1C8Rr8ycx4uBcVbu"
)

# === MEMOISED FILE PATH FETCH ===
get_existing_file_path <- function(file_name) {
  file_path <- file.path(data_dir, file_name)
  
  if (!file.exists(file_path)) {
    message(paste("Downloading:", file_name))
    drive_download(as_id(file_links[[file_name]]), path = file_path, overwrite = TRUE)
  }
  
  if (file.exists(file_path)) {
    return(file_path)
  } else {
    return(NULL)
  }
}

# Memoised version
memoised_get_existing_file_path <- memoise(get_existing_file_path)

# === DATASET SELECTION ===
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

observeEvent(input$clicked_cards, {
  new_selection <- input$clicked_cards
  if (identical(new_selection, "none")) { 
    selected_cards(NULL)
  } else {
    selected_cards(new_selection)
  }
})

# === BEST MATCHING FILE FINDER ===
get_best_matching_file <- function(selected) {
  selected <- sort(tolower(selected))
  pattern <- paste0("processed_", paste(selected, collapse = "_"), "_database.RData")
  best_match <- names(file_links)[grepl(pattern, names(file_links))]
  
  if (length(best_match) > 0) {
    file_path <- memoised_get_existing_file_path(best_match)
    return(file_path)
  } else {
    return(NULL)
  }
}

# === MEMOISED DATASET LOADING ===
load_dataset_from_file <- function(file_path) {
  loaded_env <- new.env()
  load(file_path, envir = loaded_env)
  dataset_name <- ls(loaded_env)[1]
  dataset <- get(dataset_name, envir = loaded_env)
  dataset$metadata$timepoint <- factor(dataset$metadata$timepoint, ordered = FALSE)
  dataset$metadata$timepoint <- relevel(dataset$metadata$timepoint, ref = "Pre_FMT")
  return(dataset)
}

memoised_load_dataset_from_file <- memoise(load_dataset_from_file)

# === REACTIVE DATA LOADING ===
selected_data <- reactive({
  if (length(selected_cards()) == 0) {
    return(NULL)
  }
  file_path <- get_best_matching_file(selected_cards())
  validate(need(!is.null(file_path) && file.exists(file_path), "No matching dataset found."))
  memoised_load_dataset_from_file(file_path)
})

# === USER FEEDBACK ===
show_dataset_message <- function() {
  if (is.null(selected_data())) {
    return(tags$p("⚠ Please select at least one dataset to proceed.", 
                  style = "color: red; font-weight: bold; margin-bottom: 20px;"))
  } else {
    return(tagList(
      tags$p(paste("You have selected the following dataset(s):", paste(selected_cards(), collapse = ", ")),
             style = "color: red; font-weight: bold;"),
      tags$p("Please proceed.", 
             style = "color: red; font-weight: bold; margin-bottom: 20px;")
    ))
  }
}

output$warning1 <- renderUI({ show_dataset_message() })