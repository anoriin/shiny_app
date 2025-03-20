tabPanel("Functional Profiling",
         titlePanel("Functional Profiling"),
         tabsetPanel(
           tabPanel("Gene Families",
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
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
                    h3("Gene Family Relative Abundance"),
                    card(
                      tags$div("Use the following tool to visualize the relative abundance of specific gene families across different groups."),
                      tags$div("P-values were calculated using the wilcoxon test with BH correction, where applicable.")
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
                          plotlyOutput("genesabu_plot"),
                          dataTableOutput("genesabu_table")
                        )
                      )
                    ),
                    h3("Gene Family Differential Abundance"),
                    card(
                      tags$div("Use this tool to identify significant gene families associated with metadata variables."),
                      tags$div("This analysis is performed using Maaslin2 (Multivariable Association Discovery in Population-scale Meta-omics Studies), which identifies associations between microbial community composition and metadata using multivariable linear models."),
                      tags$div("Note: Donor data has been removed from this analysis.")
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
                            "genes_covariates",
                            "Choose covariates:",
                            choices = c(variable_mapping[names(variable_mapping) %in% c("Response", 
                                                                                        "Sex", 
                                                                                        "Pre- or Post-FMT", 
                                                                                        "Age", 
                                                                                        "S point")], 
                                        "Response + Timepoint"),
                            multiple = TRUE
                          ),
                          actionButton("generate_genes_diffabundance_table", label = "Run")
                        ),
                        mainPanel(
                          uiOutput("warning_genes_diffabundance"),
                          dataTableOutput("genes_diffabundance_table")
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
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
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
                    h3("Pathway Relative Abundance"),
                    card(
                      tags$div("Use the following tool to visualize the relative abundance of specific pathways across different groups."),
                      tags$div("P-values were calculated using the wilcoxon test with BH correction, where applicable.")
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
                          plotlyOutput("pwabu_plot"),
                          dataTableOutput("pwabu_table")
                        )
                      )
                    ),
                    h3("Pathway Differential Abundance"),
                    card(
                      tags$div("Use this tool to identify significant pathways associated with metadata variables."),
                      tags$div("This analysis is performed using Maaslin2 (Multivariable Association Discovery in Population-scale Meta-omics Studies), which identifies associations between microbial community composition and metadata using multivariable linear models."),
                      tags$div("Note: Donor data has been removed from this analysis.")
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
                            "pw_covariates",
                            "Choose covariates:",
                            choices = c(variable_mapping[names(variable_mapping) %in% c("Response", 
                                                                                        "Sex", 
                                                                                        "Pre- or Post-FMT", 
                                                                                        "Age", 
                                                                                        "S point")], 
                                        "Response + Timepoint"),
                            multiple = TRUE
                          ),
                          actionButton("generate_pw_diffabundance_table", label = "Run")
                        ),
                        mainPanel(
                          uiOutput("warning_pw_diffabundance"),
                          dataTableOutput("pw_diffabundance_table")
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
                    tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
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
                    h3("Enzyme Relative Abundance"),
                    card(
                      tags$div("Use the following tool to visualize the relative abundance of specific enzymes across different groups."),
                      tags$div("P-values were calculated using the wilcoxon test with BH correction, where applicable.")
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
                          plotlyOutput("enzymesabu_plot"),
                          dataTableOutput("enzymesabu_table")
                        )
                      )
                    ),
                    h3("Enzyme Differential Abundance"),
                    card(
                      tags$div("Use this tool to identify significant enzyme commissions associated with metadata variables."),
                      tags$div("This analysis is performed using Maaslin2 (Multivariable Association Discovery in Population-scale Meta-omics Studies), which identifies associations between microbial community composition and metadata using multivariable linear models."),
                      tags$div("Note: Donor data has been removed from this analysis.")
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
                            "ec_covariates",
                            "Choose covariates:",
                            choices = c(variable_mapping[names(variable_mapping) %in% c("Response", 
                                                                                        "Sex", 
                                                                                        "Pre- or Post-FMT", 
                                                                                        "Age", 
                                                                                        "S point")], 
                                        "Response + Timepoint"),
                            multiple = TRUE
                          ),
                          actionButton("generate_ec_diffabundance_table", label = "Run")
                        ),
                        mainPanel(
                          uiOutput("warning_ec_diffabundance"),
                          dataTableOutput("ec_diffabundance_table")
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