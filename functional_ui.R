tabPanel("Functional Profiling",
         titlePanel("Functional Profiling"),
         tabsetPanel(
           tabPanel("Gene Families",
                    tags$style(HTML(".card { margin-bottom: 20px; }")),
                    card(
                      tags$div("UniRef90 is a protein sequence database that clusters sequences sharing at least 90% identity into gene families."),
                      tags$div("These families represent related proteins that likely perform similar functions and share a common evolutionary origin."),
                      tags$div("In metagenomics, UniRef90 gene families are identified by mapping predicted protein sequences from microbial genomes to the UniRef90 database, and normalizing abundances to counts per million (CPM)."),
                      tags$div("These gene families provide insight into the functional composition of microbial communities.")
                    ),
                    
                    h3("Gene Family Relative Abundance"),
                    card(
                      tags$div("Visualize the relative abundance of specific gene families across different groups."),
                      tags$div("P-values were calculated using the Wilcoxon test with Benjamini-Hochberg (BH) correction, where applicable.")
                    ),
                    
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("genes_subset_data", "Subset data:", choices = c("All", "Donors", "Recipients")),
                          selectInput("genes_split_by", "Select a grouping variable:", choices = variable_mapping[names(variable_mapping) != "Age"]),
                          selectInput("genes_split_by2", "Select a second grouping variable:", choices = c("None")),
                          selectizeInput("selected_genes_abu", "Select gene families to plot:", choices = NULL, multiple = TRUE, options = list(placeholder = "Select one or more gene families")),
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
                      tags$div("Identify gene families significantly associated with metadata variables using Maaslin2 (Multivariable Association Discovery in Population-scale Meta-omics Studies)."),
                      tags$div("This method uses multivariable linear models to link microbial community composition to metadata."),
                      tags$div("Note: Donor samples were excluded from this analysis.")
                    ),
                    
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput("genes_covariates", "Select model covariates:", choices = c(variable_mapping[names(variable_mapping) %in% c("Response", "Sex", "Pre- or Post-FMT", "Age", "S point")], "Response + Timepoint"), multiple = TRUE, 
                                      options = list(placeholder = "Select one or more covariates")),
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
                      tags$div("Visualize fold changes in gene family abundance pre- vs. post-FMT in specific patient subgroups."),
                      tags$div("Note: Donor samples were excluded. P-values calculated using the Wilcoxon test.")
                    ),
                    
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("genes_responder_filter", "Subset by responder status:", choices = c("All Recipients", "Responder" = "R", "Non-Responder" = "NR")),
                          selectizeInput("selected_genes_fc", "Select gene families to plot:", choices = NULL, multiple = TRUE, options = list(placeholder = "Select one or more genes")),
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
                    tags$style(HTML(".card { margin-bottom: 20px; }")),
                    card(
                      tags$div("Pathway abundance reflects the activity or prevalence of specific biological pathways based on gene presence in microbial communities."),
                      tags$div("These values provide insight into functional capabilities, such as nutrient metabolism or environmental response."),
                      tags$div("All pathway abundances are normalized to counts per million (CPM).")
                    ),
                    
                    h3("Pathway Relative Abundance"),
                    card(
                      tags$div("Visualize the relative abundance of specific pathways across groups."),
                      tags$div("P-values were calculated using the Wilcoxon test with BH correction, where applicable.")
                    ),
                    
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("pathways_subset_data", "Subset data:", choices = c("All", "Donors", "Recipients")),
                          selectInput("pathways_split_by", "Select a grouping variable:", choices = variable_mapping[names(variable_mapping) != "Age"]),
                          selectInput("pathways_split_by2", "Select a second grouping variable:", choices = c("None")),
                          selectizeInput("selected_pathways_abu", "Select pathways to plot:", choices = NULL, multiple = TRUE, options = list(placeholder = "Select one or more pathways")),
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
                      tags$div("Identify significant pathways associated with metadata using Maaslin2."),
                      tags$div("Note: Donor samples were excluded from this analysis.")
                    ),
                    
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput("pw_covariates", "Select model covariates:", choices = c(variable_mapping[names(variable_mapping) %in% c("Response", "Sex", "Pre- or Post-FMT", "Age", "S point")], "Response + Timepoint"), multiple = TRUE, 
                                      options = list(placeholder = "Select one or more covariates")),
                          actionButton("generate_pw_diffabundance_table", label = "Run")
                        ),
                        mainPanel(
                          uiOutput("warning_pw_diffabundance"),
                          dataTableOutput("pw_diffabundance_table")
                        )
                      )
                    ),
                    
                    h3("Pathway Fold Changes"),
                    card(
                      tags$div("Visualize fold changes in pathway abundance pre- vs. post-FMT for specific patient groups."),
                      tags$div("Note: Donor samples were excluded. P-values calculated using the Wilcoxon test.")
                    ),
                    
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("pathways_responder_filter", "Subset by responder status:", choices = c("All Recipients", "Responder" = "R", "Non-Responder" = "NR")),
                          selectizeInput("selected_pathways_fc", "Select pathways:", choices = NULL, multiple = TRUE, options = list(placeholder = "Select one or more pathways")),
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
                      tags$div("Visualize the distribution of pathway coverage using pie charts."),
                      tags$div("Coverage categories: High (≥ 0.95), Low (≤ 0.05), and Intermediate (between 0.05 and 0.95).")
                    ),
                    
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("pathwayscov_subset_data", "Subset data:", choices = c("All", "Donors", "Recipients")),
                          selectizeInput("selected_pathways_cov", "Select pathways to plot:", choices = NULL, multiple = TRUE, options = list(placeholder = "Select one or more pathways")),
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
                    tags$style(HTML(".card { margin-bottom: 20px; }")),
                    card(
                      tags$div("Enzyme Commission (EC) numbers categorize enzymes based on the reactions they catalyze."),
                      tags$div("EC abundance reflects the prevalence of enzyme-mediated reactions within microbial communities."),
                      tags$div("All EC abundances are normalized to counts per million (CPM).")
                    ),
                    
                    h3("Enzyme Relative Abundance"),
                    card(
                      tags$div("Visualize the relative abundance of enzymes across groups."),
                      tags$div("P-values were calculated using the Wilcoxon test with BH correction, where applicable.")
                    ),
                    
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("enzymes_subset_data", "Subset data:", choices = c("All", "Donors", "Recipients")),
                          selectInput("enzymes_split_by", "Select a grouping variable:", choices = variable_mapping[names(variable_mapping) != "Age"]),
                          selectInput("enzymes_split_by2", "Select a second grouping variable:", choices = c("None")),
                          selectizeInput("selected_enzymes_abu", "Select enzymes to display:", choices = NULL, multiple = TRUE, options = list(placeholder = "Select one or more enzymes")),
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
                      tags$div("Identify significant enzymes associated with metadata using Maaslin2."),
                      tags$div("Note: Donor samples were excluded from this analysis.")
                    ),
                    
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput("ec_covariates", "Select model covariates:", choices = c(variable_mapping[names(variable_mapping) %in% c("Response", "Sex", "Pre- or Post-FMT", "Age", "S point")], "Response + Timepoint"), multiple = TRUE, 
                                      options = list(placeholder = "Select one or more covariates")),
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
                      tags$div("Visualize changes in enzyme abundance pre- vs. post-FMT in selected recipient subgroups."),
                      tags$div("Note: Donor samples were excluded. P-values calculated using the Wilcoxon test.")
                    ),
                    
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("enzymes_responder_filter", "Subset by responder status:", choices = c("All Recipients", "Responder" = "R", "Non-Responder" = "NR")),
                          selectizeInput("selected_enzymes_fc", "Select enzyme commissions to plot:", choices = NULL, multiple = TRUE, options = list(placeholder = "Select one or more enzymes")),
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
