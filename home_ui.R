tabPanel("Home",
         fluidPage(
           useShinyjs(),
           h2("Welcome to the FMT Microbiome Explorer"),
           h3("Instructions"),
           h5("1. Select the dataset(s) you would like to work with. Multiple selections will concatenate datasets."),
           h5("2. Create plots and run statistical tests."),
           h5("3. Download results."),
           br(),
           h4("Select the dataset(s) you would like to work with:"),
           fluidRow(
             column(4,
                    div(id = "Routy", class = "clickable-card",
                        div(class = "clickable-card-header", "Routy"),
                        imageOutput("map_img1")
                    )
             ),
             column(4,
                    div(id = "Davar", class = "clickable-card",
                        div(class = "clickable-card-header", "Davar"),
                        imageOutput("map_img2")
                    )
             ),
             column(4,
                    div(id = "Baruch", class = "clickable-card",
                        div(class = "clickable-card-header", "Baruch"),
                        imageOutput("map_img3")
                    )
             )
           ),
           uiOutput("warning1"),
           # Information about FMT
           my_card(
             header = "What is Fecal Microbiota Transplantation (FMT)?",
             body = list(
               tags$div("FMT is the process of transferring fecal bacteria and other microbes from a healthy individual into another individual."),
               tags$div("In the context of cancer treatment, FMT aims to reshape the microbial landscape in cancer patients to enhance their response to immunotherapy.")
             )
           )
         )
)