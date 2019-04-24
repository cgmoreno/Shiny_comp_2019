shinyUI(fluidPage(
  navbarPage("Cluster Analysis with the Fast Food Dataset",
             tabPanel("Data Overview",
               tags$h3("Background"),
               tags$h5(textOutput("background")),
               tags$h6(helpText("*Some subjective post-processing applied, see global.R for original source and details.")),
               tags$h3("Get to Know the Data"),
               radioButtons("view_intro", "Select", choices = c("raw data", "univariate view")),
               conditionalPanel(
                 condition = "input.view_intro == 'univariate view'",
                 selectInput("var_list", "Select variable:", choices = names(food_use %>% select(-X1, -item))),
                 tableOutput("univariate_view")
               ),
               conditionalPanel(
                 condition = "input.view_intro == 'raw data'",
                 dataTableOutput("dataset")
               )
             ),
             tabPanel("Clustering",
               fluidRow(
                 tags$h3("Select Cluster Method"),
                 tags$h6(helpText("Press buttons below to generate cluster results after selecting method inputs."))
               ),
               fluidRow(
                 column(6,
                        selectInput("dist1", "Distance Metric", choices = c(c("euclidean", "maximum", "manhattan", "canberra"))),
                        selectInput("linkage1", "Heirarchical Clustering Method", choices = c("ward.D2", "single", "complete", "average", "minimax")),
                        
                        actionButton("conduct_clust1", "Cluster Group 1"),
                        plotOutput("hclust_plot1"),
                        numericInput("cutree1", "# Clusters", value = 2, min = 2, max = nrow(food_use_clust)),
                        checkboxInput("isolate1", "Isolate single cluster?"),
                        conditionalPanel(
                          condition = "input.isolate1 == true",
                          uiOutput("isolate_clust1"),
                          dataTableOutput("view_items_clust1")
                        )
                 ),
                 column(6,
                        selectInput("dist2", "Distance Metric", choices = c(c("euclidean", "maximum", "manhattan", "canberra", "minkowski"))),
                        selectInput("linkage2", "Heirarchical Clustering Method", choices = c("ward.D2", "single", "complete", "average", "minimax")),
                        actionButton("conduct_clust2", "Cluster Group 2"),
                        plotOutput("hclust_plot2"),
                        numericInput("cutree2", "# Clusters", value = 2, min = 2, max = nrow(food_use_clust)),
                        checkboxInput("isolate2", "Isolate single cluster?"),
                        conditionalPanel(
                          condition = "input.isolate2 == true",
                          uiOutput("isolate_clust2"),
                          dataTableOutput("view_items_clust2")
                        )
                 )
             ),
             tags$h3("Interpret Cluster Results"),
             fluidRow(
               column(6, 
                      selectInput("group", "Select grouping variable", choices = c("restaurant", "veg", "has_salad", "has_bacon", "crispy", "bbq"))
                      ),
               column(6,
                      textInput("custom_group", "Type custom grouping variable")
                      )
             ),
             plotOutput("counts_by_group_fig"),
             hr(),
             fluidRow(
                 column(6, 
                        selectInput("xvar1", "X", choices = c(cont_x, cat_x), selected = "cholesterol"),
                        selectInput("colorvar1", "Color", choices = c(cont_x, cat_x))
                        # radioButtons("view_clust_res", "Include Results from Method:", choices = c(1, 2))
                        ),
                 column(6,
                        selectInput("yvar1", "Y", choices = cont_x, selected = "sodium"),
                        selectInput("facet_var1", "Facet", choices = cat_x)
                        )
               ),
             plotOutput("custom_plot1")
             
  ) 
 
  )
))