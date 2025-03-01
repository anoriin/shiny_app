tabPanel("Metadata",
         titlePanel("Metadata"),
         fluidRow(
           column(
             6,
             card(
               h3("Summary Statistics"),
               uiOutput("demographics_table")
             )
           ),
           column(
             6,
             card(
               h3("Study Design"),
               uiOutput("study_design_table")
             )
           )
         ),
         fluidRow(
           column(
             12,
             card(
               h3("Metadata Summary Plots")
             )
           )
         ),
         sidebarLayout(
           sidebarPanel(
             selectInput(
               "plot_type",
               "Choose a type of plot:",
               choices = c("boxplot", "barplot")
             ),
             selectInput(
               "group_choice",
               "Subset data:",
               choices = c("All", "Donors", "Recipients")
             ),
             selectInput(
               "metadata_x_axis",
               "Choose x-axis variable:",
               choices = c("None", variable_mapping[names(variable_mapping) != "Age"])
             ),
             selectInput(
               "metadata_y_axis",
               "Choose y-axis variable:",
               choices = c("None", variable_mapping[names(variable_mapping) == "Age"])
             ),
             selectInput(
               "metadata_fill_colour",
               "Choose second grouping variable:",
               choices = c("None", variable_mapping[names(variable_mapping) != "Age"])
             ),
             actionButton("plot_bttn", "Plot")
           ),
           mainPanel(
             uiOutput("plot_ui")  # Plot area (conditionally rendered)
           )
         )
)