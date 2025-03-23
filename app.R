library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(ggsignif)
library(DT)
library(tidyr)
library(dplyr)
library(vegan)
library(SummarizedExperiment)
library(lefser)
library(plotly)
library(googledrive)
library(Maaslin2)
library(memoise)

source("helpers.R", local = TRUE)$value

ui <- navbarPage(
  title = "FMT Microbiome Explorer",
  position = "fixed-top",
  collapsible = TRUE,
  inverse = TRUE,
  fluid = TRUE,
  tags$head(
    tags$style("
    body {
      padding-top: 80px; /* Push body content down */
    }
    .clickable-card {
      cursor: pointer;
      border: 2px solid transparent;
      border-radius: 10px;
      box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);
      padding: 15px;
      text-align: center;
      transition: all 0.3s ease-in-out;
      margin-bottom: 20px;
    }
    .clickable-card-header {
      font-weight: bold;
      background-color: #f8f9fa;
      border-bottom: 1px solid #ddd;
      padding: 10px;
      border-top-left-radius: 10px;
      border-top-right-radius: 10px;
    }
    .clickable-card img {
      width: 100%;
      height: auto;
      object-fit: contain;
      border-radius: 10px;
    }
    .clickable-card.selected {
      border: 3px solid red !important;
      box-shadow: 0 0 15px rgba(255, 0, 0, 0.6) !important;
    }
    .custom-card {
      border: 1px solid #ddd;
      border-radius: 10px;
      box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);
      padding: 10px;
      overflow: hidden;
      height: 100%;
    }
    .custom-card-header {
      font-weight: bold;
      background-color: #f8f9fa;
      border-bottom: 1px solid #ddd;
      padding: 10px;
      border-top-left-radius: 10px;
      border-top-right-radius: 10px;
    }
    .custom-card-body {
      height: 100%;
    }
    .shiny-image-output {
      height: auto !important;  /* Override Shiny's default height */
    }
  ")
  ),
  source("home_ui.R", local = TRUE)$value,
  source("metadata_ui.R", local = TRUE)$value,
  source("taxonomic_ui.R", local = TRUE)$value,
  source("functional_ui.R", local = TRUE)$value
)

server <- function(input, output, session) {
  source("home_server.R", local = TRUE)$value
  source("metadata_server.R", local = TRUE)$value
  source("taxonomic_server.R", local = TRUE)$value
  source("functional_server.R", local = TRUE)$value
}

shinyApp(ui = ui, server = server)