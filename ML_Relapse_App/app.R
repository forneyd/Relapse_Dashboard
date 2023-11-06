############## Add Source Files #########

source("load.R")
source("functions.R")
source("resources.R")

###### Libraries ######


# library(ggplot2)
# library(DT)
library(shinydashboard)
library(shiny)
# library(tidyverse)




# list of variables

c1 <- analysis %>%
  select(-"who", -"project") %>%
  names()



######################### app  ######################

# rmarkdown::render("teammiami_outline.Rmd")


ui <- dashboardPage(
  dashboardHeader(
    title = "BST692: Exploring CTN-0094 Data with R & Shiny Dashboard", titleWidth = 650, 
    tags$li(class="dropdown",tags$a(href="https://github.com/hermes33ac/Team-Miami", icon("github"), "Source Code", target="_blank")
    )
  ), # dasboardHeader
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("About", tabName = "about", selected = TRUE),
      menuItem("Predictors and Outcome", tabName = "pred_outcome"),
      conditionalPanel("input.sidebar == 'pred_outcome' && input.t2 == 'viz'", selectInput(inputId = "var_x", label = "Select a variable", choices = c1, selected = "Age")),
      menuItem("Model",
               tabName = "model",
               menuSubItem("Traditional Logistic Regression", tabName = "logistic"),
               menuSubItem("KNN Model", tabName = "knn"),
               menuSubItem("Logistic model using Lasso", tabName = "lasso"),
               menuSubItem("Cart Model", tabName = "cart"),
               menuSubItem("Random Forest Model", tabName = "rf")
      ), # menuItem
      
      menuItem("Summary of Models", tabName = "summary"),
      menuItem("Conclusion", tabName = "conclusion")
    ) # sidebarMenu
  ), # dashboardSidebar
  
  
  
  dashboardBody(
    tabItems(
      
      # First tab Item
      tabItem(
        tabName = "about",
        tabBox(
          id = "t1", width = 12,
          tabPanel("About", fluidPage(includeMarkdown("about.Rmd"))),
          tabPanel("Dataset", DT::dataTableOutput("analysisOutput_df"))
        ) # tabBox
      ), # tabItem1
      
      # 2nd tab item
      
      tabItem(
        tabName = "pred_outcome",
        tabBox(
          id = "t2", width = 12,
          # tabPanel("Outcome", fluidPage(includeHTML("./www/Outcome.html"))),
          tabPanel(
            "Outcome",
            fluidPage(
              htmltools::tags$iframe(
                src = "Outcome.html", width = "100%", height = 1000, style = "border:none;"
              )
            )
          ),
          tabPanel("Predictors", fluidPage(includeMarkdown("Predictors.Rmd"))),
          tabPanel("Visualization",
                   value = "viz",
                   fluidRow(
                     box(plotOutput("auto_plot"), width = 12)
                   ),
                   fluidRow(
                     tableOutput("sumry")
                   )
          ), # tabpanel
          tabPanel(
            "Summary Statistics",
            fluidPage(
              htmltools::tags$iframe(
                src = "descriptive_stat.html", width = "100%", height = 1000, style = "border:none;"
              )
            )
          ),
          
        ) # tabBox
      ), # tabItem2
      
      # 3rd tab Item
      
      tabItem(
        tabName = "logistic",
        tabBox(
          id = "t3", width = 12,
          tabPanel(
            "CTN-0094 Data",
            fluidRow(
              box(
                width = 12,
                includeMarkdown("logistic_results.Rmd")
              )
            ),
            fluidRow(
              box(
                width = 8, status = "primary",
                title = "Logistic Regression Results",
                tableOutput("logistic_betas")
              )
            ),
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Confusion Matrix",
                plotOutput("logistic_conf_mat")
              ),
              box(
                width = 4, status = "primary",
                title = "ROC plot",
                plotOutput("logistic_roc_plot")
              )
            ),
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Prediction Performance",
                tableOutput("logistic_metrices")
              ),
              box(
                width = 4, status = "primary",
                title = "Prediction Plot",
                plotOutput("logistic_pred_plot")
              )
            )
          ), # tabpanel
          tabPanel("CTN-0027 Data"),
          tabPanel("CTN-0030 Data"),
          tabPanel("CTN-0051 Data")
        ) # tabBox
      ), # tabItem3
      
      
      
      # 4th tab item
      
      tabItem(
        tabName = "lasso",
        tabBox(
          id = "t3", width = 12,
          tabPanel(
            "CTN-0094 Data",
            fluidRow(
              box(
                width = 12,
                includeMarkdown("lasso_results.Rmd")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "primary",
                title = "Important Predictors ",
                plotOutput("lasso_vip")
              )
            ),
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Prediction Performance",
                tableOutput("lasso_metric")
              ),
              box(
                width = 4, status = "primary",
                title = "Confusion Matrix",
                plotOutput("lasso_conf_mat")
              )
            ), # fluidrow
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "ROC plot",
                plotOutput("lasso_roc")
              ),
              box(
                width = 4, status = "primary",
                title = "Prediction Plot",
                plotOutput("lasso_pred_plot")
              )
            ) # fluidrow
          ), # tabpanel
          tabPanel("CTN-0027 Data"),
          tabPanel("CTN-0030 Data"),
          tabPanel("CTN-0051 Data")
        ) # tabBox
      ), # tabItem4
      
      
      # 5th tab Item
      
      tabItem(
        tabName = "knn",
        tabBox(
          id = "t3", width = 12,
          tabPanel(
            "CTN-0094 Data",
            fluidRow(
              box(
                width = 12,
                includeMarkdown("knn_results.Rmd")
              ),
            ),
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Prediction Results",
                tableOutput("knn_matrices")
              ),
              box(
                width = 4, status = "primary",
                title = "ROC plot",
                plotOutput("knn_roc_plot")
              )
            ), # fluidrow
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Confusion Matrix",
                plotOutput("knn_conf_mat")
              ),
              box(
                width = 4, status = "primary",
                title = "Prediction Plot",
                plotOutput("knn_pred_plot")
              )
            ) # fluidrow
          ), # tabpanel
          
          tabPanel("CTN-0027 Data"),
          tabPanel("CTN-0030 Data"),
          tabPanel("CTN-0051 Data")
        ) # tabBox
      ), # tabItem5
      
      # 6th tab item
      
      tabItem(
        tabName = "cart",
        tabBox(
          width = 12,
          id = "t3",
          tabPanel(
            "CTN-0094 Data",
            fluidRow(
              box(
                width = 12,
                includeMarkdown("cart_results.Rmd")
              )
            ),
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Confusion Matrix",
                plotOutput("cart_conf_mat")
              ),
              box(
                width = 4, status = "primary",
                title = "ROC plot",
                plotOutput("cart_roc_plot")
              ),
              box(
                width = 4, status = "primary",
                title = "Prediction Performance",
                tableOutput("cart_metric")
              )
            ),
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Important Predictors",
                plotOutput("cart_vip")
              ),
              box(
                width = 4, status = "primary",
                title = "Prediction Plot",
                plotOutput("cart_prediction_plot")
              )
            ),
            fluidRow(
              box(
                width = 7, status = "primary",
                title = "Decision Tree",
                plotOutput("cart_tree")
              ),
              box(
                width = 3, status = "primary",
                title = "Decision Tree Interpretation",
                "Here the decision tree is showing only the top few nodes from the entire
                tree to simplify the findings. We can see that which project the participants
                belong to is the feature that provided the best split"
              )
            )
          ), # tabpanel
          tabPanel("CTN-0027 Data"),
          tabPanel("CTN-0030 Data"),
          tabPanel("CTN-0051 Data")
        ) # tabBox
      ), # tabItem6
      
      
      # 7th tab Item
      
      tabItem(
        tabName = "rf",
        tabBox(
          id = "t3", width = 12,
          tabPanel(
            "CTN-0094 Data",
            fluidRow(
              box(
                width = 12,
                includeMarkdown("rf_results.Rmd")
              ),
            ),
            fluidRow(
              box(
                width = 10, status = "primary",
                title = "Prediction Results",
                tableOutput("rf_mets")
              ),
              box(
                width = 4, status = "primary",
                title = "ROC plot",
                plotOutput("rf_roc_plot")
              ),
              box(
                width = 4, status = "primary",
                title = "Confusion Matrix",
                plotOutput("rf_CF_Matrix")
              )
            ), # fluidrow
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Important Predictors",
                plotOutput("rf_vip")
              ),
              box(
                width = 4, status = "primary",
                title = "Prediction Plot",
                plotOutput("rf_prediction_plot")
              ),
              box(
                width = 4, status = "primary",
                title = "AUC plot",
                plotOutput("rf_auc")
              )
            ) # fluidrow
          ), # tabpanel
          tabPanel("CTN-0027 Data"),
          tabPanel("CTN-0030 Data"),
          tabPanel("CTN-0051 Data")
        ) # tabBox
      ), # tabItem7
      
      
      # 8th tab Item
      tabItem(
        tabName = "summary",
        tabBox( width = 12,
                tabPanel(
                  "CTN-0094 Data",
                  fluidRow(
                    box(
                      width = 12,
                      includeMarkdown("summary.Rmd")
                    )
                  ),
                  fluidRow(
                    box(
                      width = 12, status = "primary",
                      title = "Model Comparison",
                      tableOutput("com_table")
                    ),
                  )
                )
        )
      ), # tabItem8
      
      # 9th tab Item
      tabItem(
        tabName = "conclusion",
        tabBox(width = 12,
               tabPanel(
                 "CTN-0094 Data",
                 fluidRow(
                   box(
                     width = 12,
                     includeMarkdown("conclusion.Rmd")
                   )
                 ),
                 fluidRow(
                   box(
                     width = 6, status = "primary",
                     title = "Prediction Results",
                     tableOutput("rf_test_roc")
                   ),
                   box(
                     width = 6, status = "primary",
                     title = "ROC plot",
                     plotOutput("test_roc_plot"))
                 ),
                 fluidRow(
                   box(
                     width = 6, status = "primary",
                     title = "Important Predictors",
                     plotOutput("test_vip")),
                   box(
                     width = 6, status = "primary",
                     title = "Prediction Plot",
                     plotOutput("test_pred_plot")
                   )
                 ) # fluidrow
               ) # tabpanel
        ) # tabbox
      ) # tabItem8
    ) # tabItems
  ) # dashboardBody
) # dashboardPage





server <- function(input, output, session) {
  ## Data table Output
  
  output$analysisOutput_df <- DT::renderDataTable({
    DT::datatable(
      analysis_df,
      rownames = TRUE,
      extensions = "Buttons",
      options = list(
        autoWidth = FALSE, scrollX = TRUE,
        columnDefs = list(list(
          width = "125px", targets = "_all"
        )),
        dom = "tpB",
        lengthMenu = list(c(5, 15, -1), c("5", "15", "All")),
        pageLength = 15,
        buttons = list(
          list(
            extend = "collection",
            text = "Show More",
            action = DT::JS(
              "function ( e, dt, node, config ) {
                             dt.page.len(50);
                             dt.ajax.reload();}"
            )
          ),
          list(
            extend = "collection",
            text = "Show Less",
            action = DT::JS(
              "function ( e, dt, node, config ) {
                             dt.page.len(10);
                             dt.ajax.reload();}"
            )
          )
        )
      )
    )
  })
  
  
  ## visualization plot output
  
  output$auto_plot <-
    renderPlot({
      plot_fun(input$var_x)
    })
  
  ## Summary
  
  output$sumry <- renderTable({
    table1(~ analysis[[input$var_x]] | project, data = analysis, subset = analysis$project != "")
  })
  
  ## logistic output
  
  output$logistic_betas <- renderTable(
    logistic_betas_rds
  )
  
  
  output$logistic_metrices <- renderTable(
    logistic_matrices_rds %>%
      select(-".config") %>%
      rename(
        "Metric" = ".metric",
        "Estimator" = ".estimator",
        "Estimate" = ".estimate"
      )
  )
  
  output$logistic_conf_mat <- renderPlot(
    logistic_conf_mat_rds
  )
  
  output$logistic_roc_plot <- renderPlot(
    logistic_ROC_rds
  )
  
  output$logistic_pred_plot <- renderPlot(
    logistic_pred_plot_rds
  )
  
  
  ## Lasso output
  
  output$lasso_vip <- renderPlot(
    vip_lasso_overall_rds
  )
  
  output$lasso_metric <- renderTable(
    Lasso_metrics_rds %>%
      select(-".config") %>%
      rename(
        "Metric" = ".metric",
        "Estimator" = ".estimator",
        "Estimate" = ".estimate"
      )
  )
  
  output$lasso_conf_mat <- renderPlot(
    lasso_conf_mat_rds
  )
  
  output$lasso_roc <- renderPlot(
    roc_lasso_plot_rds
  )
  
  output$lasso_pred_plot <- renderPlot(
    Pred_plot_lasso_rds
  )
  
  
  ## Random Forest output
  
  output$rf_mets <- renderTable(
    rf_mets_rds %>%
      select(-".config") %>%
      rename(
        "Metric" = ".metric",
        "Estimator" = ".estimator",
        "Estimate" = ".estimate"
      )
  )
  
  output$rf_roc_plot <- renderPlot(
    rf_roc_rds
  )
  
  output$rf_CF_Matrix <- renderPlot(
    rf_CF_Matrix_rds
  )
  
  output$rf_vip <- renderPlot(
    rf_vip_rds
  )
  
  output$rf_prediction_plot <- renderPlot(
    rf_plot_rds
  )
  
  output$rf_auc <- renderPlot(
    rf_auc_rds
  )
  
  ## knn output
  
  output$knn_matrices <- renderTable(
    knn_matrices_rds %>%
      select(-".config") %>%
      rename(
        "Metric" = ".metric",
        "Estimator" = ".estimator",
        "Estimate" = ".estimate"
      )
  )
  
  output$knn_roc_plot <- renderPlot(
    replace_roc_plot_rds
  )
  
  output$knn_conf_mat <- renderPlot(
    knn_conf_mat_rds
  )
  
  output$knn_pred_plot <- renderPlot(
    knn_pred_plot_rds
  )
  
  
  ## cart output
  
  
  output$cart_conf_mat <- renderPlot(
    cart_conf_mat_rds
  )
  
  output$cart_roc_plot <- renderPlot(
    cart_roc_plot_rds
  )
  
  output$cart_metric <- renderTable(
    cart_metrics_rds %>%
      select(-".config") %>%
      rename(
        "Metric" = ".metric",
        "Estimator" = ".estimator",
        "Estimate" = ".estimate"
      )
  )
  
  
  output$cart_vip <- renderPlot(
    cart_vip_rds
  )
  
  
  output$cart_prediction_plot <- renderPlot(
    cart_prediction_plot_rds
  )
  
  output$cart_tree <- renderPlot(
    rpart.plot::rpart.plot(cart_tree_fit_rds,
                           roundint = FALSE,
                           tweak = 1,
                           clip.facs = TRUE
    )
  )
  
  # comparison table
  
  output$com_table <- renderTable(
    com_table
  )
  
  # test outputs
  
  output$rf_test_roc <- renderTable(
    rf_test_roc_auc_rds %>% 
      rename(
        "Metric" = ".metric",
        "Estimator" = ".estimator",
        "Estimate" = ".estimate"
      )
  )
  
  output$test_roc_plot <- renderPlot(
    rf_test_curve_rds
  )
  
  output$test_vip <- renderPlot(
    test_vip_rds
  )
  
  output$test_pred_plot <- renderPlot(
    rf_test_plot_rds
  )
}

shinyApp(ui, server)