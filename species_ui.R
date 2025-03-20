tabPanel("Taxonomic Profiling",
         useShinyjs(),
         titlePanel("Taxonomic Profiling"),
           tabsetPanel(
             tabPanel("Summary Statistics",
                      fluidPage(
                        h3("Top Taxa"),
                        card(
                          p("Use this tool to visualize the top mean abundance levels of microbes at various taxa levels.")
                        ),
                        tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "taxa_level",
                              "Choose taxonomic level:",
                              choices = list(
                                "Kingdom"="k__", 
                                "Phylum"="p__", 
                                "Class"="c__", 
                                "Order"="o__", 
                                "Family"="f__", 
                                "Genus"="g__", 
                                "Species"="s__", 
                                "SGBs"="t__"
                                ),
                              selected = "Genus"
                            ),
                            numericInput(
                              "top_taxa", 
                              "Number of top taxa to display:", 
                              value = 10, 
                              min = 1, 
                              max = 20
                            ),
                            selectInput(
                              "species_subset_data",
                              "Subset data:",
                              choices = c("All", "Recipients", "Donors")
                            ),
                            selectInput(
                              "ss_split_by",
                              "Split bars by:",
                              choices = NULL
                            ),
                            actionButton("generate_summary1", "Plot")
                          ),
                          mainPanel(
                            uiOutput("warning_ss1"),
                            plotlyOutput("top_taxa_plot")
                          )
                        ),
                        h3("Taxa by Condition"),
                        card(
                          p("Use this tool to visualize the mean abundance levels of microbes, at various taxa levels, across grouped samples.")
                        ),
                        tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "taxa_level2",
                              "Choose taxonomic level:",
                              choices = list(
                                "Kingdom"="k__", 
                                "Phylum"="p__", 
                                "Class"="c__", 
                                "Order"="o__", 
                                "Family"="f__", 
                                "Genus"="g__", 
                                "Species"="s__", 
                                "SGBs"="t__"
                              ),
                              selected = "Genus"
                            ),
                            selectizeInput(
                              "selected_taxa",
                              "Select taxa to display",
                              choices = NULL,  # Choices will be populated dynamically
                              multiple = TRUE, 
                              options = list(placeholder = "Select one or more taxa")
                            ),
                            selectInput(
                              "species2_subset_data",
                              "Subset data:",
                              choices = c("All", "Recipients", "Donors")
                            ),
                            selectInput(
                              "split_by",
                              "Choose grouping variable:",
                              choices = variable_mapping[names(variable_mapping) != "Age"]
                            ),
                            selectInput(
                              "split_by2",
                              "Choose a second grouping variable:",
                              choices = c("None", variable_mapping[names(variable_mapping) != "Age"])
                            ),
                            actionButton("generate_summary2", "Plot")
                          ),
                          mainPanel(
                            uiOutput("warning_ss2"),
                            plotlyOutput("relabu_plot"),
                            dataTableOutput("relabu_table")
                          )
                        )
                      )
             ),
             tabPanel("Alpha Diversity", 
                      fluidPage(
                        h3("Alpha Diversity"),
                        card(
                          tags$div("Use this tool to visualize the alpha diversity (richness and evenness) of microbial samples at various taxa levels, across groups."),
                          tags$div("P-values were calculated using the wilcoxon test with BH correction, where applicable.")
                        ),
                        tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "adiversity_taxa_level",
                              "Choose taxonomic level to analyze:",
                              choices = list(
                                "Kingdom"="k__", 
                                "Phylum"="p__", 
                                "Class"="c__", 
                                "Order"="o__", 
                                "Family"="f__", 
                                "Genus"="g__", 
                                "Species"="s__", 
                                "SGBs"="t__"
                              )
                            ),
                            selectInput(
                              "ad_subset_data",
                              "Subset data:",
                              choices = c("All", "Recipients", "Donors")
                            ),
                            selectInput(
                              "diversity_index",
                              "Select diversity index:",
                              choices = c("Observed", "Simpson", "Shannon", "InvSimpson")
                            ),
                            selectInput(
                              "x_axis",
                              "Select X-axis variable:",
                              choices = variable_mapping
                            ),
                            selectInput(
                              "colour_by",
                              "Colour points by:",
                              choices = c("None", variable_mapping[names(variable_mapping) != "Age"])
                            ),
                            selectInput(
                              "adjustment_method",
                              "Select p-value adjustment method (if more than two groups):",
                              choices = c("holm", "hochberg", "bonferroni", "BY", "fdr", "none")
                            ),
                            actionButton("refresh_alpha", "Plot")
                          ),
                          mainPanel(
                            uiOutput("warning_ad"),
                            plotlyOutput("a_diversity_plot"),
                            my_card(
                              header = "Wilcoxon Rank Sum Test Results",
                              tableOutput("wilcoxon_tests")
                            )
                          )
                        )
                      )
             ),
             tabPanel("Beta Diversity", 
                      fluidPage(
                        h3("Beta Diversity"),
                        card(
                          tags$div("Use this tool to visualize the beta diversity (dissimilarity) of microbial samples at various taxa levels, across groups."),
                          tags$div("P-values were calculated using the PERMANOVA test.")
                        ),
                        tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "bdiversity_taxa_level",
                              "Choose taxonomic level to analyze:",
                              choices = list(
                                "Kingdom"="k__", 
                                "Phylum"="p__", 
                                "Class"="c__", 
                                "Order"="o__", 
                                "Family"="f__", 
                                "Genus"="g__", 
                                "Species"="s__", 
                                "SGBs"="t__"
                                )
                            ),
                            selectInput(
                              "bd_subset_data",
                              "Subset data:",
                              choices = c("All", "Recipients")
                            ),
                            selectInput(
                              "method",
                              "Choose a distance method:",
                              choices = c("bray", "euclidean", "manhattan", "jaccard")
                            ),
                            selectInput(
                              "colour_by2",
                              "Colour points by (grouping condition for PERMANOVA):",
                              choices = variable_mapping[names(variable_mapping) != "Age"]
                            ),
                            selectInput(
                              "shape_by",
                              "Shape points by:",
                              choices = c("None", variable_mapping[names(variable_mapping) != "Age"])
                            ),
                            sliderInput(
                              "k",
                              "Set k (number of groups):",
                              min = 2,
                              max = 10,
                              value = 5,
                              step = 1
                            ),
                            actionButton("refresh_beta", "Plot")
                          ),
                          mainPanel(
                            uiOutput("warning_bd"),
                            plotlyOutput("b_diversity_plot"),
                            my_card(
                              header = "Permutational Multivariate Analysis of Variance (PERMANOVA) Results",
                              tableOutput("permanova_tests")
                            )
                          )
                        )
                      )
             ),
             tabPanel("Differential Abundance", 
                      fluidPage(
                        h3("Differential Abundance"),
                        card(
                          tags$div("Differential abundance is performed using Maaslin2 (Multivariable Association Discovery in Population-scale Meta-omics Studies)."),
                          tags$div("Maaslin2 is a statistical tool used to identify associations between microbial community composition and metadata using multivariable linear models."),
                          tags$div("Note: donor data was removed from this analysis.")
                        ),
                        tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "maas_taxa_level",
                              "Choose taxonomic level to analyze:",
                              choices = list("Kingdom"="k__", 
                                             "Phylum"="p__", 
                                             "Class"="c__", 
                                             "Order"="o__", 
                                             "Family"="f__", 
                                             "Genus"="g__", 
                                             "Species"="s__", 
                                             "SGBs"="t__")
                            ),
                            selectInput(
                              "covariates",
                              "Choose covariates:",
                              choices = c(variable_mapping[names(variable_mapping) %in% c("Response", 
                                                                                          "Sex", 
                                                                                          "Pre- or Post-FMT", 
                                                                                          "Age", 
                                                                                          "S point")], 
                                          "Response + Timepoint"),
                              multiple = TRUE
                            ),
                            actionButton("run2", label = "Run")
                          ),
                          mainPanel(
                            uiOutput("warning_maas"),
                            dataTableOutput("maas_table")
                          )
                        )
                      )
             ),
             tabPanel("LEfSe", 
                      fluidPage(
                        h3("LEfSe"),
                        card(
                          tags$div("LEfSe is a statistical tool used to identify microbial taxa that are significantly different between two groups."),
                          tags$div("Note: donor data was removed from LEfSe analysis.")
                        ),
                        tags$style(HTML("
                          .card {
                            margin-bottom: 20px;
                          }
                        ")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "lefse_taxa_level",
                              "Choose taxonomic level to analyze:",
                              choices = list("Kingdom"="k__", 
                                             "Phylum"="p__", 
                                             "Class"="c__", 
                                             "Order"="o__", 
                                             "Family"="f__", 
                                             "Genus"="g__", 
                                             "Species"="s__", 
                                             "SGBs"="t__")
                            ),
                            selectInput(
                              "class",
                              "Choose class variable:",
                              choices = variable_mapping[names(variable_mapping) %in% c("Response", "Sex", "Pre- or Post-FMT")]
                            ),
                            selectInput(
                              "subclass",
                              "Choose subclass variable:",
                              choices = c("None", variable_mapping[names(variable_mapping) %in% c("Response", "Sex", "Pre- or Post-FMT")])
                            ),
                            numericInput(
                              "kw_alpha",
                              "Set alpha value threshold for the Kruskal-Wallis Rank Sum Test:",
                              min = 0.01,
                              max = 1,
                              value = 0.05
                            ),
                            numericInput(
                              "wilcoxon_alpha",
                              "Set alpha value threshold for the Wilcoxon Rank Sum Test:",
                              min = 0.01,
                              max = 1,
                              value = 0.05
                            ),
                            numericInput(
                              "lda",
                              "Set effect size (LDA score) threshold:",
                              min = 0.5,
                              max = 4,
                              value = 2
                            ),
                            actionButton("run", label = "Run")
                          ),
                          mainPanel(
                            uiOutput("warning_lefse"),
                            plotlyOutput("lefse_plot"),
                            dataTableOutput("lefse_table")
                          )
                        )
                      )
             )
           )
)