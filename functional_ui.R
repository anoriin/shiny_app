tabPanel("Functional Profiling",
         titlePanel("Functional Profiling"),
         tabsetPanel(
           tabPanel("Gene Families",
                    h3("Gene Family Abundance"),
                    card(
                      tags$div("UniRef90 is a protein sequence database that clusters sequences with at least 90% identity into gene families."), 
                      tags$div("These gene families represent groups of related proteins that likely perform similar functions and share a common evolutionary origin."),
                      tags$div("In metagenomics, UniRef90 gene families are extracted by mapping predicted protein sequences from microbial genomes to the UniRef90 database, and normalizing their abundance values to counts per million (CPM)."),
                      tags$div("The resulting gene families provide insights into the functional composition of the microbial community in a sample.")
                    ),
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                    card(
                      p("Use the following tool to visualize the relative abundance of specific gene families across different groups.")
                    ),
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "genes_subset_data",
                            "Subset data:",
                            choices = c("All", "Donors", "Recipients")
                          ),
                          selectInput(
                            "genes_split_by",
                            "Select grouping variable:",
                            choices = variable_mapping[names(variable_mapping) != "Age"]
                          ),
                          selectInput(
                            "genes_split_by2",
                            "Select second grouping variable:",
                            choices = c("None")
                          ),
                          selectizeInput(
                            "selected_genes_abu",
                            "Select gene families to display:",
                            choices = NULL,  # Choices will be populated dynamically
                            multiple = TRUE, # Allow multiple pathway selection
                            options = list(placeholder = "Select one or more gene families")
                          ),
                          actionButton("generate_genesabuplot", "Plot")
                        ),
                        mainPanel(
                          uiOutput("warning8"),
                          plotlyOutput("genesabu_plot")
                        )
                      )
                    ),
                    h3("Gene Family Fold Changes"),
                    card(
                      tags$div("Use the following tool to visualize changes in the abundance of specific gene families pre-FMT (at baseline) vs. post-FMT in specific patient groups (no donors)."), 
                      tags$div("P-values were calculated using the wilcoxon test.")
                    ),
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "genes_responder_filter",
                            "Subset by responder status:",
                            choices = c("All Recipients", "Responder"="R", "Non-Responder"="NR")
                          ),
                          selectizeInput(
                            "selected_genes_fc",
                            "Select gene families:",
                            choices = NULL,  # Choices will be populated dynamically
                            multiple = TRUE,
                            options = list(placeholder = "Select one or more genes")
                          ),
                          actionButton("generate_genesfcplot", "Plot")
                        ),
                        mainPanel(
                          uiOutput("warning9"),
                          plotlyOutput("genesfc_plot"),
                          dataTableOutput("genesfc_table")
                        )
                      )
                    )
                  ),
           tabPanel("Pathways",
                    h3("Pathway Abundance"),
                    card(
                      tags$div("Pathway abundance represents how active or prevalent specific biological pathways are within a microbial community based on the genes present in the sample."),
                      tags$div("These values give us insight into the community's functional capabilities, such as how it processes nutrients or responds to its environment."),
                      tags$div("Each pathway abundance value is normalized to counts per million (CPM).")
                    ),
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                    card(
                      p("Use the following tool to visualize the relative abundance of specific pathways across different groups.")
                    ),
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "pathways_subset_data",
                            "Subset data:",
                            choices = c("All", "Donors", "Recipients")
                          ),
                          selectInput(
                            "pathways_split_by",
                            "Select grouping variable:",
                            choices = variable_mapping[names(variable_mapping) != "Age"]
                          ),
                          selectInput(
                            "pathways_split_by2",
                            "Select second grouping variable:",
                            choices = c("None")
                          ),
                          selectizeInput(
                            "selected_pathways_abu",
                            "Select pathways to display:",
                            choices = NULL,  # Choices will be populated dynamically
                            multiple = TRUE, # Allow multiple pathway selection
                            options = list(placeholder = "Select one or more pathways")
                          ),
                          actionButton("generate_pwabuplot", "Plot")
                        ),
                        mainPanel(
                          uiOutput("warning10"),
                          plotlyOutput("pwabu_plot")
                        )
                      )
                    ),
                    h3("Pathway Abundance Fold Changes"),
                    card(
                      tags$div("Use the following too to visualize changes in the abundance of specific pathways pre-FMT (at baseline) vs. post-FMT in specific patient groups (no donors)."),
                      tags$div("P-values were calculated using the wilcoxon test.")
                    ),
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "pathways_responder_filter",
                            "Subset by responder status:",
                            choices = c("All Recipients", "Responder"="R", "Non-Responder"="NR")
                          ),
                          selectizeInput(
                            "selected_pathways_fc",
                            "Select pathways:",
                            choices = NULL,  # Choices will be populated dynamically
                            multiple = TRUE,
                            options = list(placeholder = "Select one or more pathways")
                          ),
                          actionButton("generate_pwfcplot", "Plot")
                        ),
                        mainPanel(
                          uiOutput("warning11"),
                          plotlyOutput("pwfc_plot"),
                          dataTableOutput("pwfc_table")
                        )
                      )
                    ),
                    h3("Pathway Coverage"),
                    card(
                      tags$div("Use the following tool to visualize the distribution of pathway coverage values using pie charts. Categories include high coverage (≥ 0.95), low coverage (≤ 0.05), and intermediate values (between 0.05 and 0.95).")                    
                      ),
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "pathwayscov_subset_data",
                            "Subset data:",
                            choices = c("All", "Donors", "Recipients")
                          ),
                          selectizeInput(
                            "selected_pathways_cov",
                            "Select pathways:",
                            choices = NULL,  # Choices will be populated dynamically
                            multiple = TRUE,
                            options = list(placeholder = "Select one or more pathways")
                          ),
                          actionButton("generate_pwcovplot", "Plot")
                        ),
                        mainPanel(
                          uiOutput("warning12"),
                          plotOutput("pwcov_plot"),
                          dataTableOutput("pwcov_table")
                        )
                      )
                    )
           ),
           tabPanel("Enzymes",
                    h3("Enzyme Abundance"),
                    card(
                      tags$div("Enzyme Commission (EC) numbers classify enzymes based on the type of reaction they catalyze. They are used to annotate and categorize enzymes in metagenomic datasets."),
                      tags$div("EC abundance values represent how active or prevalent specific enzyme-driven reactions are within a microbial community based on the genes present in the sample."),
                      tags$div("All EC abundance values were normalized to counts per million (CPM).")
                    ),
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                    card(
                      p("Use the following tool to visualize the relative abundance of specific enzymes across different groups.")
                    ),
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "enzymes_subset_data",
                            "Subset data:",
                            choices = c("All", "Donors", "Recipients")
                          ),
                          selectInput(
                            "enzymes_split_by",
                            "Select grouping variable:",
                            choices = variable_mapping[names(variable_mapping) != "Age"]
                          ),
                          selectInput(
                            "enzymes_split_by2",
                            "Select second grouping variable:",
                            choices = c("None")
                          ),
                          selectizeInput(
                            "selected_enzymes_abu",
                            "Select enzymes to display:",
                            choices = NULL,  # Choices will be populated dynamically
                            multiple = TRUE, # Allow multiple pathway selection
                            options = list(placeholder = "Select one or more enzymes")
                          ),
                          actionButton("generate_enzymesabuplot", "Plot")
                        ),
                        mainPanel(
                          uiOutput("warning13"),
                          plotlyOutput("enzymesabu_plot")
                        )
                      )
                    ),
                    h3("Enzyme Fold Changes"),
                    card(
                      tags$div("Use the following tool to visualize changes in the abundance of specific enzymes pre-FMT (at baseline) vs. post-FMT in specific patient groups (no donors)."), 
                      tags$div("P-values were calculated using the wilcoxon test.")
                    ),
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "enzymes_responder_filter",
                            "Subset by responder status:",
                            choices = c("All Recipients", "Responder"="R", "Non-Responder"="NR")
                          ),
                          selectizeInput(
                            "selected_enzymes_fc",
                            "Select enzyme commissions:",
                            choices = NULL,  # Choices will be populated dynamically
                            multiple = TRUE,
                            options = list(placeholder = "Select one or more enzymes")
                          ),
                          actionButton("generate_enzymesfcplot", "Plot")
                        ),
                        mainPanel(
                          uiOutput("warning14"),
                          plotlyOutput("enzymesfc_plot"),
                          dataTableOutput("enzymesfc_table")
                        )
                      )
                    )
                 )
         )
)