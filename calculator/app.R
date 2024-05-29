library(shiny)
library(bslib)
library(bsicons)
#library(shinythemes)
library(shinydashboard)
source("CORE_all.R")
#source("CORE_risk.R")

# Define UI ----
ui <- page_sidebar(
  title = "CORE Calculator",
  #theme = shinytheme("sandstone"), 
  sidebar = sidebar(
  width = 400,
    # card(
    #   card_header("Risk group*"),
    #   radioButtons(
    #     "risk_group", "",
    #     choices = list("No" = 1, "Yes" = 2),
    #     selected = 1,
    #     inline = TRUE
    #     ),
    #   helpText(
    #     "*Risk group here means having type 2 diabetes or 2 metabolic risk factors."
    #   )
    # ),
    card(
      card_header("Age (years)"),
      textInput("age", NULL, #"Age (years):", 
                value = NULL)
    ),
    card(
      card_header("Sex"),
      radioButtons(
        "sex", NULL, #"Select sex:",
        choices = list("Male" = 0, "Female" = 1),
        selected = 0,
        inline = TRUE
      )
    ),
    card(
      card_header("GGT (Gamma-Glutamate Transferase)"),
      radioButtons(
        "ggt_unit", NULL,
        choices = list("microkat/L" = 1, "U/L" = 1/60),
        selected = 1,
        inline = TRUE
      ),
      textInput("ggt", NULL, # "Value:", 
                value = NULL)
    ),
    card(
      card_header("AST (Aspartate Aminotransferase)"),
      radioButtons(
        "ast_unit", NULL,
        choices = list("microkat/L" = 1, "U/L" = 1/60),
        selected = 1,
        inline = TRUE
      ),
      textInput("ast", NULL, # "Value:", 
                value = NULL)
    ),
    card(
      card_header("ALT (Alanine Aminotransferase)"),
      radioButtons(
        "alt_unit", NULL,
        choices = list("microkat/L" = 1, "U/L" = 1/60),
        selected = 1,
        inline = TRUE
      ),
      textInput("alt", NULL, # "Value:", 
                value = NULL)
    ), 
  actionButton("calculate", "Calculate")
  ),
  card(
    #textOutput("risk")),
    value_box("The 10 year risk is:", textOutput("risk"),
              showcase = icon("heart-pulse"),
              theme = "green"
              )#,
    #valueBoxOutput("vbox")
    ),
card(
    verbatimTextOutput("messages")) 
)

# Define server logic ----
server <- function(input, output) {
  NumberCheck <- function(x, var) {
    y <- as.numeric(x)
    if (is.na(y)) {
      y <- gsub(",", ".", x, fixed = TRUE)
      y <- as.numeric(y)
    }
    if (is.na(y) | y < 0) {
      return(paste0(var, " has an invalid format."))
    } else {
      return(y)
    }
  }
  ValueWarning <- function(x, l = 0, u = Inf, var) {
    unit <- ifelse(var == "Age", " years.", " microkat/L.") 
    if (x < l) {
      paste0(var, " is extremely low. The estimate might be unreliable below ", l, unit)
    } else if (x > u) {
      paste0(var, " is extremely high. The estimate might be unreliable above ", u, unit)
    } else {
      NULL
    }
  }
  
  p_update <- eventReactive(input$calculate, {
    output$messages <- NULL
    
    age <- NumberCheck(input$age, "Age")
    ggt <- NumberCheck(input$ggt, "GGT")
    ast <- NumberCheck(input$ast, "AST")
    alt <- NumberCheck(input$alt, "ALT")
    
    varlist <- list(age, ggt, ast, alt)
    
    valid <- sapply(varlist, is.numeric)
    
    if (sum(valid) < 4) {
      output$messages <- renderText({paste0(unlist(varlist[!valid]), collapse = "\n")})
    } else {
      
      val_args <- list(list(age, 20, 80, "Age"), 
           list(ggt, 0, 25, "GGT"), 
           list(ast, 0, 12, "AST"), 
           list(alt, 0, 20, "ALT"))
      output$messages <- renderText({paste0(unlist(sapply(val_args, 
                                                          function(lst) do.call(ValueWarning, lst))), 
                                            collapse = "\n")})
      d <- data.frame(age = age,
                      sex = factor(input$sex, 0:1),
                      GGT = ggt * as.numeric(input$ggt_unit),
                      AST = ast * as.numeric(input$ast_unit),
                      ALT = alt * as.numeric(input$alt_unit)
      )
      # f <- ifelse(input$risk_group == 1, 
      #             PredictCORE_full, 
      #             PredictCORE_full)
      f <- PredictCORE_full
      f(d)
    }
  })
  output$risk <- renderText({
    p <- p_update()
    if (is.numeric(p)) {
      if (p > 0.2) {
        ">20%"
      } else {
        paste0(round(p * 100, 2), "%")
      }
    }
    })
  # boxcol <- function(p) cut(p, c(-Inf, 0.01, 0.1, Inf), c("green", "orange", "red"))
  # output$vbox <- renderValueBox({
  #   valueBox("The 10 year risk is:", textOutput("risk"),
  #             icon = icon("heart-pulse"),
  #             color = boxcol(p_update()), 
  #            width = NULL
  #             )
  # })
  #output$col <- "bg-green" #cut2(output$risk, c(0.01, 0.1), labels = c("green", "orange", "red"))
}

# Run the app ----
shinyApp(ui = ui, server = server)
