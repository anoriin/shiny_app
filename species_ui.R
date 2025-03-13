tabPanel("Abundance",
         useShinyjs(),
         titlePanel("Abundance"),
           tabsetPanel(
             tabPanel("Summary Statistics",
                      fluidPage(
                        h3("Top Taxa"),
                        card(
                          p("Visualize the top mean abundance levels of microbes.")
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
                            uiOutput("warning3"),
                            plotlyOutput("top_taxa_plot")
                          )
                        ),
                        h3("Taxa by Condition"),
                        card(
                          p("Visualize the mean abundance levels of microbes, at various taxa levels, across grouped samples.")
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
                            uiOutput("warning4"),
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
                          p("Alpha diversity measures the extent of microbial diversity within a specific area or ecosystem. In this context, we are examining the alpha diversity of the microbiome, at the strain level, in stool samples collected from melanoma patients undergoing FMT.")
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
                              "diversity_index",
                              "Select diversity index:",
                              choices = c("Simpson", "Shannon", "InvSimpson")
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
                            checkboxInput(
                              "include_donors",
                              "Include donors",
                              value = TRUE
                            ),
                            selectInput(
                              "adjustment_method",
                              "Select p-value adjustment method (if more than two groups):",
                              choices = c("holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none")
                            ),
                            actionButton("refresh_alpha", "Plot")
                          ),
                          mainPanel(
                            uiOutput("warning5"),
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
                          p("Beta diversity measures the difference between two microbial samples by comparing the number of unique taxa, at the strain level, specific to each ecosystem.")
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
                            checkboxInput(
                              "include_donors2",
                              "Include donors",
                              value = TRUE
                            ),
                            actionButton("refresh_beta", "Plot")
                          ),
                          mainPanel(
                            uiOutput("warning6"),
                            plotlyOutput("b_diversity_plot"),
                            my_card(
                              header = "Permutational Multivariate Analysis of Variance (PERMANOVA) Results",
                              tableOutput("permanova_tests")
                            )
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
                                             "Strain"="t__")
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
                            actionButton("run", label = "Run LEfSe")
                          ),
                          mainPanel(
                            uiOutput("warning7"),
                            plotlyOutput("lefse_plot"),
                            dataTableOutput("lefse_table")
                          )
                        )
                      )
             )
           )
)