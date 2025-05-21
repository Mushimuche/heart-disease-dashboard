# Load libraries
library(tidyverse)
library(mice)        # For PMM (numeric imputation)
library(missForest)  # For Random Forest imputation
library(httr)
library(shiny)
library(bs4Dash)
library(plotly)
library(thematic)
library(waiter)
library(fresh)
library(textshaping)
library(fresh)
library(bslib)
library(htmltools)

# Create a custom theme that will be responsive to light/dark mode
custom_theme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#343a40",
    navbar_dark_color = "#f8f9fa",
    sidebar_dark_color = "#f8f9fa",
    card_cap_bg = "#dc3545",
    card_cap_color = "#ffffff",
    font_family_base = "'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif"
  ),
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#343a40",
    text_light = "#f8f9fa"
  ),
  bs4dash_layout(
  ),
  bs4dash_sidebar_dark(
    color = "#f8f9fa",
    bg = "#343a40",
    hover_color = "#ffffff", 
    hover_bg = "#dc3545"
  ),
  bs4dash_status(
    danger = "#dc3545",
    primary = "#007bff"
  )
)

# Data preprocessing
urlfile = "https://raw.githubusercontent.com/Mushimuche/heart-disease-dashboard/refs/heads/main/heart_disease_uci.csv"

df <- read_csv(url(urlfile))

head(df)
glimpse(df)
colSums(is.na(df))

# Remove ID column
df <- df %>% select(-id)

# Convert character columns to factors and logical columns to factors
df <- df %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.logical), as.factor))

# Ensure categorical columns are properly set as factors
cat_cols <- c("sex", "fbs", "restecg", "exang", "slope", "ca", "thal")
df[cat_cols] <- lapply(df[cat_cols], as.factor)

# Separate numeric and categorical columns
df_numeric <- df %>% select(where(is.numeric))
df_categorical <- df %>% select(where(is.factor))

# Impute numeric variables using mice (PMM)
set.seed(123)
mice_imputed <- mice(df_numeric, method = 'pmm', m = 1, maxit = 5, printFlag = FALSE)
df_numeric_imputed <- complete(mice_imputed)

# Combine numeric-imputed data with original categorical
df_combined <- cbind(df_numeric_imputed, df_categorical)

# Ensure numeric columns stay numeric
numeric_cols <- names(df_numeric_imputed)
df_combined[numeric_cols] <- lapply(df_combined[numeric_cols], as.numeric)

# Double check and reconvert factor columns
factor_cols <- names(df_categorical)
df_combined[factor_cols] <- lapply(df_combined[factor_cols], as.factor)

# Final check for high-cardinality factors
high_card <- sapply(df_combined, function(x) if (is.factor(x)) nlevels(x) else NA)
high_card[!is.na(high_card) & high_card > 53]

# Impute using missForest
set.seed(456)
missforest_imputed <- missForest(df_combined)

# Final imputed data
df_final <- missforest_imputed$ximp

head(df_final)
sum(is.na(df_final))
sum(duplicated(df_final))
colSums(is.na(df_final))
glimpse(df_final)

# Define UI tabs
home_tab <- tabItem(
  tabName = "home",
  
  jumbotron(
    title = "Welcome!",
    status = "info",
    lead = "This web dashboard serves as our learning evidence for CS 226 Data Analytics - Statistics Using R, AY 2024–2025.
It utilizes the Heart Disease Data Set from the UCI repository, which includes patient demographics and clinical measurements such as blood pressure, cholesterol, blood sugar, and heart rate.

The application aims to identify patterns that contribute to the prediction of heart disease and provide interactive visualizations based on various health and demographic indicators.

This dashboard offers valuable insights for those in the medical and healthcare fields, particularly in monitoring patient trends, identifying risk factors, and supporting preventive care.",
    btnName = "Link to Dataset",
    href = "https://www.kaggle.com/datasets/redwankarimsony/heart-disease-data",
    "Heart Disease Data Set from UCI data repository"
  ),
  
  fluidRow(
    column(width = 4,
           userBox(
             width = NULL, # This makes the box take full width of its parent column
             collapsible = FALSE,
             title = userDescription(
               title = "Khinje Louis P. Curugan",
               subtitle = "BSCS Student",
               image = "https://raw.githubusercontent.com/Mushimuche/heart-disease-dashboard/ai/assets/recent pic.jpg
",
               type = 1
             ),
             status = "teal",
             HTML("<br>College of Information and Computing<br>BS Computer Science Major in Data Science<br>CS 226 Data Analytics - Stat Using R, AY 2024-2025")
           )
    ),
    column(width = 4,
           userBox(
             width = NULL,
             collapsible = FALSE,
             title = userDescription(
               title = "Rui Manuel A. Palabon",
               subtitle = "BSCS Student",
               image = "https://raw.githubusercontent.com/Mushimuche/heart-disease-dashboard/ai/assets/BSCS2_Palabon.jpg",
               type = 1
             ), 
             status = "purple",
             HTML("<br>College of Information and Computing<br>BS Computer Science Major in Data Science<br>CS 226 Data Analytics - Stat Using R, AY 2024-2025")
           )
    ),
    column(width = 4,
           userBox(
             width = NULL,
             collapsible = FALSE,
             title = userDescription(
               title = "Aj Ian L. Ressureccion",
               subtitle = "BSCS Student",
               image = "https://raw.githubusercontent.com/Mushimuche/heart-disease-dashboard/ai/assets/received_2306525082881700_2.jpeg
",
               type = 1
             ),
             status = "gray",
             HTML("<br>College of Information and Computing<br>BS Computer Science Major in Data Science<br>CS 226 Data Analytics - Stat Using R, AY 2024-2025")
           )
    )
  )
)

predict_tab <- tabItem(
  tabName = "predict_tab",
  
  # Loading spinner overlay
  useWaiter(),
  
  fluidRow(
    # Left column: Input controls
    column(
      width = 5,
      bs4Card(
        title = "Enter Patient Information",
        status = "danger",
        solidHeader = TRUE,
        width = 12,
        elevation = 3,
        
        # Age input
        numericInput(
          "pred_age", 
          "Age", 
          value = 45,
          min = 20, 
          max = 100
        ),
        
        # Sex input
        radioButtons(
          "pred_sex", 
          "Sex", 
          choices = c("Male" = "0", "Female" = "1"),
          selected = "0",
          inline = TRUE
        ),
        
        # Chest pain type
        selectInput(
          "pred_cp", 
          "Chest Pain Type", 
          choices = c(
            "Typical Angina" = "0",
            "Atypical Angina" = "1",
            "Non-anginal Pain" = "2",
            "Asymptomatic" = "3"
          ),
          selected = "0"
        ),
        
        # Resting blood pressure
        numericInput(
          "pred_trestbps", 
          "Resting Blood Pressure (mm Hg)", 
          value = 120,
          min = 80, 
          max = 200
        ),
        
        # Cholesterol
        numericInput(
          "pred_chol", 
          "Serum Cholesterol (mg/dl)", 
          value = 200,
          min = 100, 
          max = 600
        ),
        
        # Fasting blood sugar
        radioButtons(
          "pred_fbs", 
          "Fasting Blood Sugar > 120 mg/dl", 
          choices = c("No" = "FALSE", "Yes" = "TRUE"),
          selected = "FALSE",
          inline = TRUE
        ),
        
        # Resting ECG
        selectInput(
          "pred_restecg", 
          "Resting ECG Results", 
          choices = c(
            "Normal" = "0",
            "ST-T Wave Abnormality" = "1",
            "Left Ventricular Hypertrophy" = "2"
          ),
          selected = "0"
        ),
        
        # Max heart rate
        numericInput(
          "pred_thalch", # Changed from "pred_thal"
          "Maximum Heart Rate Achieved", 
          value = 150,
          min = 60, 
          max = 220
        ),
        
        # Exercise induced angina
        radioButtons(
          "pred_exang", 
          "Exercise Induced Angina", 
          choices = c("No" = "FALSE", "Yes" = "TRUE"),
          selected = "FALSE",
          inline = TRUE
        ),
        
        # ST depression
        numericInput(
          "pred_oldpeak", 
          "ST Depression Induced by Exercise", 
          value = 0.0,
          min = 0, 
          max = 10,
          step = 0.1
        ),
        
        # Slope of peak exercise ST segment
        selectInput(
          "pred_slope", 
          "Slope of Peak Exercise ST Segment", 
          choices = c(
            "Upsloping" = "0",
            "Flat" = "1",
            "Downsloping" = "2"
          ),
          selected = "0"
        ),
        
        # Number of major vessels
        selectInput(
          "pred_ca", 
          "Number of Major Vessels Colored by Fluoroscopy", 
          choices = c("0" = "0", "1" = "1", "2" = "2", "3" = "3"),
          selected = "0"
        ),
        
        # Thalassemia
        selectInput(
          "pred_thal", 
          "Thalassemia", 
          choices = c(
            "Normal" = "0",
            "Fixed Defect" = "1",
            "Reversible Defect" = "2"
          ),
          selected = "0"
        ),
        
        # Prediction button
        div(
          style = "text-align: center; margin-top: 20px; margin-bottom: 15px;",
          actionButton(
            "predict_button", 
            "Predict Heart Disease Risk", 
            icon = icon("heartbeat"),
            class = "btn-lg btn-danger"
          )
        ),
        
        # Added model metrics display
        uiOutput("model_metrics_ui")
      )
    ),
    
    # Right column: Results and visualizations
    column(
      width = 7,
      
      # Prediction Result Card
      bs4Card(
        title = "Prediction Result",
        status = "danger",
        solidHeader = TRUE,
        width = 12,
        elevation = 3,
        
        fluidRow(
          column(
            width = 12,
            # Results will appear here
            uiOutput("prediction_result"),
            
            # Risk gauge visualization
            plotlyOutput("risk_gauge", height = "275px"),
            
            # Feature importance
            plotlyOutput("feature_importance", height = "225px")
          )
        )
      ),
      
      # Health recommendations card
      bs4Card(
        title = "Health Recommendations",
        status = "danger",
        solidHeader = TRUE,
        width = 12,
        elevation = 3,
        
        uiOutput("health_recommendations")
      )
    )
  )
  
)

ai_tab <- tabItem(
  tabName = "ai_tab",
  
  # Main container for the AI assistant
  fluidRow(
    column(
      width = 12,
      bs4Card(
        title = "Heart Disease AI Assistant",
        status = "danger",
        solidHeader = TRUE,
        width = 12,
        elevation = 3,
        
        # Introduction message
        uiOutput("ai_intro_text"),
        
        # Chat history container with default welcome message
        uiOutput("chat_container"),
        
        # Input row with text input and send button
        fluidRow(
          column(
            width = 10,
            textInput(
              "user_message",
              label = NULL,
              placeholder = "Ask me something about heart disease or the dashboard..."
            )
          ),
          column(
            width = 2,
            actionButton(
              "send_message",
              label = "Send",
              icon = icon("paper-plane"),
              width = "100%",
              class = "btn-danger",
              style = "margin-top: 5px;"
            )
          )
        ),
        
        # JavaScript for handling Enter key
        tags$script('
          $(document).ready(function() {
            // Handle Enter key press
            $("#user_message").keypress(function(event) {
              if (event.which === 13) {
                event.preventDefault();
                $("#send_message").click();
              }
            });
            
            // Make sure chat history scrolls to bottom when updated
            function scrollChatToBottom() {
              var chatHistory = document.getElementById("chat-history");
              if (chatHistory) {
                chatHistory.scrollTop = chatHistory.scrollHeight;
              }
            }
            
            // Call function once on load
            scrollChatToBottom();
            
            // Set interval to check for changes and scroll
            setInterval(scrollChatToBottom, 500);
          });
        ')
      )
    )
  )
)

dashboard_tab <- tabItem(
  tabName = "dashboard",
  
  # Fluid Row for valueboxes
  fluidRow(
    valueBox(
      # Total number of patients
      value = nrow(df_final),
      subtitle = "Total Patients",
      color = "danger",
      icon = icon("heartbeat")
    ),
    valueBox(
      # Number of patients with heart disease (num > 0)
      value = sum(df_final$num > 0),
      subtitle = "Patients With Heart Disease",
      color = "danger",
      icon = icon("exclamation-triangle")
    ),
    valueBox(
      # Patients with Diabetes (fbs == "TRUE")
      value = sum(df_final$fbs == "TRUE"),
      subtitle = "Patients with Diabetes (FBS > 120)",
      color = "danger",
      icon = icon("syringe")
    ),
    valueBox(
      # Average age of patients
      value = round(mean(df_final$age), 1),
      subtitle = "Average Age of Patients",
      color = "danger",
      icon = icon("user")
    )
  ),
  
  # Fluid Row for the switch (Sex or Age) input
  fluidRow(
    column(
      width = 3,
      # Add select input to switch between Sex or Age
      selectInput(
        inputId = "plot_type",
        label = "Choose Plot By:", 
        choices = c("Sex", "Age"),
        selected = "Sex"  # Default selection
      )
    )
  ),
  
  # Fluid Row for first two plots
  fluidRow(
    # Plot 1: Heart Disease Occurrence
    bs4Card(
      title = "Heart Disease Occurrence",
      width = 6,
      status = "danger",
      solidHeader = TRUE,
      plotlyOutput("disease_occurrence_plot", height = "300px")
    ),
    # Plot 2: Cholesterol Levels
    bs4Card(
      title = "Cholesterol Levels",
      width = 6,
      status = "danger",
      solidHeader = TRUE,
      plotlyOutput("cholesterol_plot", height = "300px")
    )
  ),
  
  # Fluid Row for second two plots
  fluidRow(
    # Plot 3: Blood Pressure and Heart Rate
    bs4Card(
      title = "Blood Pressure and Heart Rate",
      width = 6,
      status = "danger",
      solidHeader = TRUE,
      plotlyOutput("bp_hr_plot", height = "300px")
    ),
    # Plot 4: Exercise-induced Angina
    bs4Card(
      title = "Exercise-induced Angina",
      width = 6,
      status = "danger",
      solidHeader = TRUE,
      plotlyOutput("angina_plot", height = "300px")
    )
  )
)

# Define UI
thematic_shiny(font = "auto")
ui <- dashboardPage(
  
  # Dashboard options
  dark = TRUE,
  help = NULL,
  fullscreen = TRUE,
  
  # Header
  dashboardHeader(
    title = dashboardBrand(
      title = "Heart Disease",
      image = "https://cdn-icons-png.flaticon.com/512/1048/1048469.png"
    )
  ),
  
  # Sidebar
  dashboardSidebar(
    skin = "dark",
    elevation = 3,
    status = "danger",
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Prediction",
        tabName = "predict_tab",
        icon = icon("stethoscope")
      ),
      menuItem(
        "AI",
        tabName = "ai_tab",
        icon = icon("robot")
      ),
      menuItem(
        "About",
        tabName = "home",
        icon = icon("home")
      )
    )
  ),
  
  controlbar = dashboardControlbar(
    
  ),
  
  # Body
  dashboardBody(
    tabItems(
      dashboard_tab,
      predict_tab,
      ai_tab,
      home_tab
    )
  ),
  
  # Footer
  footer = dashboardFooter(
    left = "Heart Disease Dashboard",
    right = "2025"
  )
)

# Define server
server <- function(input, output, session) {
  
  ############################ PREDICTION SERVER FUNCTIONS ######################################
  # Train a prediction model - fixed to handle factor contrast issues
  train_prediction_model <- function(data) {
    # Check if data exists and has required columns
    if (is.null(data) || nrow(data) == 0) {
      stop("No data available for training model")
    }
    
    required_cols <- c("age", "sex", "cp", "trestbps", "chol", "fbs", 
                       "restecg", "thalch", "exang", "oldpeak", "slope", "ca", "thal", "num")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # Create a copy of the data for preprocessing
    model_data <- data
    
    # Ensure factors have proper levels
    model_data <- model_data %>%
      mutate(
        # Convert sex to factor first to ensure proper levels
        sex = factor(sex, levels = c("Male", "Female")),
        sex_numeric = as.numeric(sex == "Female"),
        
        # For cp (already numeric from 0-3)
        cp_numeric = as.numeric(cp),
        
        # For fbs - ensure it has both TRUE and FALSE
        fbs = factor(fbs, levels = c(FALSE, TRUE)),
        fbs_numeric = as.numeric(fbs == TRUE),
        
        # For restecg - ensure it has all levels
        restecg = factor(restecg, levels = c("Normal", "ST-T abnormality", "Hypertrophy")),
        restecg_numeric = as.numeric(restecg) - 1, # Convert to 0-based
        
        # For exang - ensure it has both TRUE and FALSE
        exang = factor(exang, levels = c(FALSE, TRUE)),
        exang_numeric = as.numeric(exang == TRUE),
        
        # For slope - ensure it has all levels
        slope = factor(slope, levels = c("Upsloping", "Flat", "Downsloping")),
        slope_numeric = as.numeric(slope) - 1, # Convert to 0-based
        
        # For ca - ensure it has all levels 0-3
        ca = factor(ca, levels = c("0", "1", "2", "3")),
        ca_numeric = as.numeric(as.character(ca)),
        
        # For thal - ensure it has all levels
        thal = factor(thal, levels = c("Normal", "Fixed defect", "Reversible defect")),
        thal_numeric = as.numeric(thal) - 1, # Convert to 0-based
        
        # Target variable - ensure it has both 0 and 1
        target = as.numeric(num > 0)
      )
    
    # Safe version of glm that handles factor contrast issues
    safe_glm <- function(formula, data, family) {
      result <- tryCatch({
        glm(formula, data = data, family = family)
      }, error = function(e) {
        if (grepl("contrasts", e$message)) {
          # If we have contrast issues, use a simplified formula with only numeric predictors
          message("Contrast error detected. Using simplified model.")
          simplified_formula <- as.formula("target ~ age + trestbps + chol + thalch + oldpeak")
          return(glm(simplified_formula, data = data, family = family))
        } else {
          # For other errors, just print the message and return NULL
          message("Error in model training: ", e$message)
          return(NULL)
        }
      })
      
      if (is.null(result)) {
        # Fallback to an extremely simple model if we still have issues
        message("Creating fallback model with minimal features")
        minimal_formula <- as.formula("target ~ age + trestbps")
        result <- glm(minimal_formula, data = data, family = "binomial")
      }
      
      return(result)
    }
    
    # Create model formula with numeric versions of variables
    model_formula <- as.formula("target ~ age + sex_numeric + cp_numeric + trestbps + 
                             chol + fbs_numeric + restecg_numeric + thalch + 
                             exang_numeric + oldpeak + slope_numeric + ca_numeric + thal_numeric")
    
    # Train a logistic regression model using our safe wrapper
    model <- safe_glm(model_formula, data = model_data, family = "binomial")
    
    return(model)
  }
  
  # Feature importance calculation function
  calculate_feature_importance <- function(model) {
    # Extract coefficients
    coefs <- tryCatch({
      coef(model)[-1]  # Remove intercept
    }, error = function(e) {
      message("Error getting coefficients: ", e$message)
      # Return some dummy values if there's an error
      dummy_coefs <- c(age = 0.5, trestbps = 0.3, chol = 0.2)
      return(dummy_coefs)
    })
    
    # Get absolute values and normalize
    importance <- abs(coefs)
    importance <- importance / sum(importance) * 100
    
    # Create a data frame
    feature_names <- names(coefs)
    feature_imp_df <- data.frame(
      Feature = feature_names,
      Importance = importance
    ) %>%
      arrange(desc(Importance))
    
    # Clean up feature names for display
    feature_imp_df$Feature <- gsub("_numeric$", "", feature_imp_df$Feature)
    
    return(feature_imp_df)
  }
  
  # Function to map feature codes to readable names
  get_feature_label <- function(feature_name) {
    labels <- c(
      "age" = "Age",
      "sex" = "Sex",
      "cp" = "Chest Pain Type",
      "trestbps" = "Resting BP",
      "chol" = "Cholesterol",
      "fbs" = "Fasting Blood Sugar",
      "restecg" = "Resting ECG",
      "thalch" = "Maximum Heart Rate",
      "exang" = "Exercise Angina",
      "oldpeak" = "ST Depression",
      "slope" = "ST Slope",
      "ca" = "Colored Vessels",
      "thal" = "Thalassemia"
    )
    
    return(ifelse(feature_name %in% names(labels), labels[feature_name], feature_name))
  }
  
  # Train the prediction model when app starts
  model <- reactive({
    # Use req() to ensure df_final exists and has data
    req(df_final)
    req(nrow(df_final) > 0)
    
    # Wrap in try-catch to handle potential errors
    tryCatch({
      train_prediction_model(df_final)
    }, error = function(e) {
      message("Error training model: ", e$message)
      NULL
    })
  })
  
  # Prediction function with robust error handling
  observeEvent(input$predict_button, {
    # Show loading spinner
    waiter_show(html = spin_fading_circles())
    
    # Add a slight delay to show the spinner
    Sys.sleep(1) 
    
    # Wrap everything in tryCatch to handle errors gracefully
    tryCatch({
      # First check if model exists
      if (is.null(model())) {
        output$prediction_result <- renderUI({
          tags$div(
            class = "text-center mb-4",
            tags$h3("Model Error", style = "color: #DC3545;"),
            tags$p("Unable to load prediction model. Please check the dataset.")
          )
        })
        return(NULL)
      }
      
      # Prepare input data - convert to proper format for prediction
      pred_data <- data.frame(
        age = as.numeric(input$pred_age),
        sex_numeric = as.numeric(input$pred_sex),
        cp_numeric = as.numeric(input$pred_cp),
        trestbps = as.numeric(input$pred_trestbps),
        chol = as.numeric(input$pred_chol),
        fbs_numeric = as.numeric(input$pred_fbs == "TRUE"), 
        restecg_numeric = as.numeric(input$pred_restecg),
        thalch = as.numeric(input$pred_thalch),
        exang_numeric = as.numeric(input$pred_exang == "TRUE"), 
        oldpeak = as.numeric(input$pred_oldpeak),
        slope_numeric = as.numeric(input$pred_slope),
        ca_numeric = as.numeric(input$pred_ca),
        thal_numeric = as.numeric(input$pred_thal)
      )
      
      # Make prediction with error handling
      pred_prob <- tryCatch({
        predict(model(), newdata = pred_data, type = "response")
      }, error = function(e) {
        # If prediction fails, show error and return NULL
        output$prediction_result <- renderUI({
          tags$div(
            class = "text-center mb-4",
            tags$h3("Prediction Error", style = "color: #DC3545;"),
            tags$p(paste("Unable to make prediction:", e$message)),
            tags$p("Please try different input values.")
          )
        })
        return(NULL)
      })
      
      # If prediction returned NULL (due to error), stop here
      if (is.null(pred_prob)) {
        waiter_hide()
        return(NULL)
      }
      
      # Process prediction results
      risk_percentage <- round(pred_prob * 100, 1)
      
      # Risk categories
      risk_level <- case_when(
        risk_percentage < 25 ~ "Low Risk",
        risk_percentage < 50 ~ "Moderate Risk",
        risk_percentage < 75 ~ "High Risk",
        TRUE ~ "Very High Risk"
      )
      
      # Risk color
      risk_color <- case_when(
        risk_percentage < 25 ~ "#3CB371",  # Green for low risk
        risk_percentage < 50 ~ "#FFA500",  # Orange for moderate risk
        risk_percentage < 75 ~ "#FF6347",  # Tomato for high risk
        TRUE ~ "#DC3545"                   # Danger red for very high risk
      )
      
      # Update prediction result output
      output$prediction_result <- renderUI({
        tags$div(
          class = "text-center mb-2",  # Reduced bottom margin from mb-4 to mb-2
          tags$h4(  # Changed from h3 to h4 for smaller size
            tags$span("Heart Disease Risk: ", style = "font-weight: normal;"),
            tags$span(
              paste0(risk_percentage, "%"), 
              style = paste0("color: ", risk_color, "; font-weight: bold;")
            )
          ),
          tags$h5(  # Changed from h4 to h5 for smaller size
            tags$span(risk_level),
            style = paste0("color: ", risk_color, "; font-weight: bold;")
          ),
          tags$p(
            class = "mt-3",  
            "Based on the information provided, this is the estimated risk of heart disease."
          )
        )
      })
      
      # Create gauge chart for risk visualization
      output$risk_gauge <- renderPlotly({
        is_dark <- isTRUE(input$dark_mode)
        bg_color <- if(is_dark) "#343a40" else "#ffffff"
        text_color <- if(is_dark) "#f8f9fa" else "#343a40"
        
        fig <- plot_ly(
          domain = list(x = c(0, 1), y = c(0, 0.9)),
          value = risk_percentage,
          title = list(text = "Risk Level"),
          type = "indicator",
          mode = "gauge+number",
          gauge = list(
            axis = list(range = list(0, 100)),
            bar = list(color = risk_color),
            steps = list(
              list(range = c(0, 25), color = "#3CB371"),  # Green
              list(range = c(25, 50), color = "#FFA500"), # Orange
              list(range = c(50, 75), color = "#FF6347"), # Tomato
              list(range = c(75, 100), color = "#DC3545") # Red
            ),
            threshold = list(
              line = list(color = "black", width = 4),
              thickness = 0.50,
              value = risk_percentage
            )
          )
        )
        
        fig <- fig %>% layout(
          paper_bgcolor = bg_color,
          font = list(color = text_color, family = "'Source Sans Pro', sans-serif"),
          margin = list(t = 25, b = 25)
        )
        
        fig
      })
      
      # Feature importance visualization
      output$feature_importance <- renderPlotly({
        is_dark <- isTRUE(input$dark_mode)
        bg_color <- if(is_dark) "#343a40" else "#ffffff"
        text_color <- if(is_dark) "#f8f9fa" else "#343a40"
        
        # Get feature importance
        feature_imp <- tryCatch({
          calculate_feature_importance(model())
        }, error = function(e) {
          # Create dummy feature importance if there's an error
          data.frame(
            Feature = c("age", "chol", "trestbps"),
            Importance = c(40, 30, 30)
          )
        })
        
        # Take top 6 features
        top_features <- head(feature_imp, 6)
        
        # Map feature codes to readable labels
        top_features$Feature <- sapply(top_features$Feature, get_feature_label)
        
        # Create bar chart
        p <- plot_ly(
          data = top_features,
          x = ~Importance,
          y = ~reorder(Feature, Importance),
          type = "bar",
          orientation = "h",
          marker = list(color = "#DC3545")
        ) %>%
          layout(
            title = list(text = "Top Risk Factors", font = list(size = 16)),
            xaxis = list(title = "Relative Importance (%)"),
            yaxis = list(title = ""),
            paper_bgcolor = bg_color,
            plot_bgcolor = bg_color,
            font = list(color = text_color, family = "'Source Sans Pro', sans-serif")
          )
        
        p
      })
      
      # Generate health recommendations
      output$health_recommendations <- renderUI({
        # Base recommendations
        recommendations <- list(
          list(
            title = "Regular Check-ups",
            icon = "calendar-check",
            text = "Schedule regular medical check-ups to monitor your heart health."
          ),
          list(
            title = "Balanced Diet",
            icon = "apple-alt",
            text = "Maintain a heart-healthy diet low in saturated fats and sodium."
          ),
          list(
            title = "Regular Exercise",
            icon = "walking",
            text = "Aim for at least 150 minutes of moderate exercise each week."
          )
        )
        
        # Add conditional recommendations based on inputs
        if (as.numeric(input$pred_chol) > 200) {
          recommendations <- c(recommendations, list(list(
            title = "Cholesterol Management",
            icon = "prescription-bottle",
            text = "Your cholesterol is elevated. Consider dietary changes and consult your doctor."
          )))
        }
        
        if (as.numeric(input$pred_trestbps) > 130) {
          recommendations <- c(recommendations, list(list(
            title = "Blood Pressure Control",
            icon = "heartbeat",
            text = "Monitor your blood pressure regularly and maintain healthy lifestyle habits."
          )))
        }
        
        if (input$pred_exang == "TRUE") {
          recommendations <- c(recommendations, list(list(
            title = "Angina Management",
            icon = "exclamation-triangle",
            text = "Discuss your exercise-induced angina with a cardiologist for proper management."
          )))
        }
        
        # Create HTML for recommendations
        is_dark <- isTRUE(input$dark_mode)
        bg_color <- if(is_dark) "#3e4551" else "#f8f9fa"
        
        rec_html <- tagList()
        
        for (rec in recommendations) {
          rec_html <- tagAppendChild(rec_html, 
                                     tags$div(
                                       class = "d-flex align-items-start mb-3",
                                       style = paste0("background-color: ", bg_color, "; padding: 15px; border-radius: 8px;"),
                                       tags$div(
                                         class = "mr-3",
                                         icon(rec$icon, class = "fa-2x text-danger")
                                       ),
                                       tags$div(
                                         tags$h5(rec$title, style = "font-weight: bold;"),
                                         tags$p(rec$text, class = "mb-0")
                                       )
                                     )
          )
        }
        
        # Disclaimer
        disclaimer <- tags$div(
          class = "mt-4 text-center small",
          tags$p(
            "DISCLAIMER: This prediction is for educational purposes only and should not replace professional medical advice.",
            style = "font-style: italic;"
          )
        )
        
        tagList(rec_html, disclaimer)
      })
      
    }, error = function(e) {
      # Master error handler - catches any errors not caught by specific handlers
      output$prediction_result <- renderUI({
        tags$div(
          class = "text-center mb-4",
          tags$h3("Error", style = "color: #DC3545;"),
          tags$p("An unexpected error occurred."),
          tags$p("Please try again or contact support if the problem persists.")
        )
      })
    })
    
    # Hide loading spinner
    waiter_hide()
  })
  
  # Model metrics calculation function 
  model_metrics <- reactive({
    req(df_final)
    req(nrow(df_final) > 0)
    
    # Wrap in try-catch to handle potential errors
    tryCatch({
      # Create a data copy for preprocessing
      model_data <- df_final %>%
        mutate(
          # Ensure all categorical variables are properly factored with correct levels
          sex = factor(sex, levels = c("Male", "Female")),
          sex_numeric = as.numeric(sex == "Female"),
          
          # Fix the cp variable - ensure it matches your data
          cp = factor(cp),  # First ensure it's a factor
          cp_numeric = as.numeric(cp),  # Then convert to numeric
          
          fbs = factor(fbs, levels = c(FALSE, TRUE)),
          fbs_numeric = as.numeric(fbs == TRUE),
          
          # Fix the restecg variable - ensure it matches your data
          restecg = factor(restecg),  # First ensure it's a factor
          restecg_numeric = as.numeric(restecg) - 1,
          
          exang = factor(exang, levels = c(FALSE, TRUE)),
          exang_numeric = as.numeric(exang == TRUE),
          
          # Fix the slope variable - ensure it matches your data
          slope = factor(slope),  # First ensure it's a factor
          slope_numeric = as.numeric(slope) - 1,
          
          # Fix the ca variable - ensure it's properly handled
          ca = as.character(ca),  # Convert to character first
          ca_numeric = as.numeric(ca),  # Then to numeric
          
          # Fix the thal variable - ensure it matches your data
          thal = factor(thal),  # First ensure it's a factor
          thal_numeric = as.numeric(thal) - 1,
          
          # Target variable - ensure it's binary
          target = as.numeric(num > 0)
        )
      
      # Add explicit error checks for variable conversion
      if(any(is.na(model_data$cp_numeric)) || any(is.na(model_data$restecg_numeric)) || 
         any(is.na(model_data$slope_numeric)) || any(is.na(model_data$ca_numeric)) || 
         any(is.na(model_data$thal_numeric))) {
        stop("Error in factor conversion - check your factor levels")
      }
      
      # Model formula with numeric versions of variables (same as in train_prediction_model)
      model_formula <- as.formula("target ~ age + sex_numeric + cp_numeric + trestbps + 
                         chol + fbs_numeric + restecg_numeric + thalch + 
                         exang_numeric + oldpeak + slope_numeric + ca_numeric + thal_numeric")
      
      # Set up k-fold cross-validation (5-fold)
      k <- 5
      set.seed(123) # For reproducibility
      folds <- sample(1:k, nrow(model_data), replace = TRUE)
      
      # Metrics storage
      metrics <- data.frame(
        Fold = integer(),
        Accuracy = numeric(),
        Sensitivity = numeric(),
        Specificity = numeric(),
        AUC = numeric(),
        F1Score = numeric(),
        stringsAsFactors = FALSE
      )
      
      # Perform k-fold cross-validation
      for (i in 1:k) {
        # Split data into training and testing
        train_data <- model_data[folds != i, ]
        test_data <- model_data[folds == i, ]
        
        # Ensure there are both positive and negative cases in training data
        if(length(unique(train_data$target)) < 2) {
          message("Fold ", i, " doesn't have both classes in training data, skipping")
          next
        }
        
        # Train model on training data
        fold_model <- glm(model_formula, data = train_data, family = "binomial")
        
        # Predict on test data
        pred_probs <- predict(fold_model, newdata = test_data, type = "response")
        pred_class <- ifelse(pred_probs > 0.5, 1, 0)
        
        # Check for NaN/NA values in predictions
        if(any(is.na(pred_probs)) || any(is.nan(pred_probs))) {
          message("Fold ", i, " has NA/NaN predictions, skipping")
          next
        }
        
        # Calculate metrics
        conf_matrix <- table(Predicted = factor(pred_class, levels=c(0,1)), 
                             Actual = factor(test_data$target, levels=c(0,1)))
        
        # Handle potential issues with confusion matrix dimensions
        if (nrow(conf_matrix) < 2 || ncol(conf_matrix) < 2) {
          # If we have only one class in predictions or actuals, create dummy values
          TN <- ifelse("0" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix), 
                       conf_matrix["0","0"], 0)
          FP <- ifelse("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix), 
                       conf_matrix["0","1"], 0)
          FN <- ifelse("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix), 
                       conf_matrix["1","0"], 0)
          TP <- ifelse("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix), 
                       conf_matrix["1","1"], 0)
        } else {
          # Normal case with both classes present
          TN <- conf_matrix[1,1]
          FP <- conf_matrix[1,2]
          FN <- conf_matrix[2,1]
          TP <- conf_matrix[2,2]
        }
        
        # Calculate metrics with safety checks
        fold_accuracy <- (TP + TN) / (TP + TN + FP + FN)
        
        fold_sensitivity <- if(TP + FN > 0) TP / (TP + FN) else NA  # True positive rate / Recall
        fold_specificity <- if(TN + FP > 0) TN / (TN + FP) else NA  # True negative rate
        fold_precision <- if(TP + FP > 0) TP / (TP + FP) else NA    # Positive predictive value
        
        # Calculate F1 Score with safety check
        if(!is.na(fold_precision) && !is.na(fold_sensitivity) && (fold_precision + fold_sensitivity > 0)) {
          fold_f1 <- 2 * (fold_precision * fold_sensitivity) / (fold_precision + fold_sensitivity)
        } else {
          fold_f1 <- NA
        }
        
        # Calculate AUC with safety check
        fold_auc <- tryCatch({
          if(requireNamespace("pROC", quietly = TRUE) && 
             length(unique(test_data$target)) > 1 && 
             !any(is.na(pred_probs))) {
            pROC::auc(pROC::roc(test_data$target, pred_probs, quiet = TRUE))
          } else {
            NA
          }
        }, error = function(e) {
          message("AUC calculation error in fold ", i, ": ", e$message)
          NA
        })
        
        # Store metrics for this fold
        metrics <- rbind(metrics, data.frame(
          Fold = i,
          Accuracy = fold_accuracy,
          Sensitivity = fold_sensitivity,
          Specificity = fold_specificity,
          AUC = fold_auc,
          F1Score = fold_f1
        ))
      }
      
      # Check if we have any valid metrics
      if(nrow(metrics) == 0) {
        stop("No valid metrics could be calculated across all folds")
      }
      
      # Calculate average metrics across all folds
      avg_metrics <- colMeans(metrics[, c("Accuracy", "Sensitivity", "Specificity", "AUC", "F1Score")], na.rm = TRUE)
      
      # Return results
      list(
        metrics = metrics,
        avg_accuracy = avg_metrics["Accuracy"] * 100, # Convert to percentage
        avg_sensitivity = avg_metrics["Sensitivity"] * 100,
        avg_specificity = avg_metrics["Specificity"] * 100,
        avg_auc = avg_metrics["AUC"],
        avg_f1 = avg_metrics["F1Score"] * 100
      )
    }, error = function(e) {
      # Print detailed error information
      message("Error calculating model metrics: ", e$message)
      print(traceback())
      
      # Return NA values to indicate calculation failure
      list(
        metrics = data.frame(),
        avg_accuracy = NA,
        avg_sensitivity = NA,
        avg_specificity = NA,
        avg_auc = NA,
        avg_f1 = NA
      )
    })
  })
  
  # Enhanced UI element to display model metrics with theme responsiveness
  output$model_metrics_ui <- renderUI({
    metrics <- model_metrics()
    is_dark <- isTRUE(input$dark_mode)
    
    # If metrics calculation failed, show error message instead
    if(is.na(metrics$avg_accuracy)) {
      return(tags$div(
        class = "alert alert-danger mt-3",
        tags$h5("Model Performance Calculation Failed"),
        tags$p("There was an error calculating the model metrics. Please check your data and model configuration.")
      ))
    }
    
    # Set colors based on theme
    bg_color <- if(is_dark) "#343a40" else "#f8f9fa"
    text_color <- if(is_dark) "#f8f9fa" else "#343a40"
    border_color <- if(is_dark) "#dc3545" else "#dc3545"
    heading_color <- if(is_dark) "#f8f9fa" else "#dc3545"
    
    tags$div(
      class = "mt-3 mb-3",
      style = paste0("background-color: ", bg_color, "; padding: 15px; border-radius: 8px; border-left: 4px solid ", border_color, ";"),
      
      tags$h5("Model Performance", 
              style = paste0("color: ", heading_color, "; font-weight: bold;")),
      
      # First row of metrics
      tags$div(
        class = "row mb-3",
        
        tags$div(
          class = "col-md-4 text-center",
          tags$div(style = paste0("font-weight: bold; color: ", text_color, ";"), "Accuracy"),
          tags$div(
            style = paste0("font-size: 1.4rem; color: ", text_color, ";"), 
            paste0(round(metrics$avg_accuracy, 1), "%"),
            tags$small(class = "ml-1", style = paste0("color: ", if(metrics$avg_accuracy >= 80) "#28a745" else "#dc3545", "; font-size: 0.7rem;"),
                       if(metrics$avg_accuracy >= 80) HTML("&nbsp;&#10004;") else HTML("&nbsp;&#10008;"))
          )
        ),
        
        tags$div(
          class = "col-md-4 text-center",
          tags$div(style = paste0("font-weight: bold; color: ", text_color, ";"), "F1 Score"),
          tags$div(style = paste0("font-size: 1.4rem; color: ", text_color, ";"), 
                   paste0(round(metrics$avg_f1, 1), "%"))
        ),
        
        tags$div(
          class = "col-md-4 text-center",
          tags$div(style = paste0("font-weight: bold; color: ", text_color, ";"), "AUC"),
          tags$div(style = paste0("font-size: 1.4rem; color: ", text_color, ";"), 
                   round(metrics$avg_auc, 2))
        )
      ),
      
      # Second row of metrics
      tags$div(
        class = "row",
        
        tags$div(
          class = "col-md-6 text-center",
          tags$div(style = paste0("font-weight: bold; color: ", text_color, ";"), "Sensitivity (Recall)"),
          tags$div(style = paste0("font-size: 1.2rem; color: ", text_color, ";"), 
                   paste0(round(metrics$avg_sensitivity, 1), "%"))
        ),
        
        tags$div(
          class = "col-md-6 text-center",
          tags$div(style = paste0("font-weight: bold; color: ", text_color, ";"), "Specificity"),
          tags$div(style = paste0("font-size: 1.2rem; color: ", text_color, ";"), 
                   paste0(round(metrics$avg_specificity, 1), "%"))
        )
      ),
      
      # Add legend/description
      tags$div(
        class = "small mt-3",
        style = paste0("color: ", text_color, "; opacity: 0.8;"),
        tags$p(
          HTML("<b>Metrics explanation:</b><br>"),
          HTML("<b>Accuracy:</b> Overall correctness of predictions<br> "),
          HTML("<b>F1 Score:</b> Harmonic mean of precision and recall<br> "),
          HTML("<b>AUC:</b> Area under ROC curve, measures discrimination ability<br> "),
          HTML("<b>Sensitivity:</b> True positive rate<br> "),
          HTML("<b>Specificity:</b> True negative rate<br>")
        ),
        tags$p(class = "mb-0", "Based on 5-fold cross-validation of the training data")
      )
    )
  })
  
  #============================================================================================
  
  ################################ AI CHAT BOT SERVER FUNCTIONS ################################
  # Render the introduction text with reactive colors
  output$ai_intro_text <- renderUI({
    is_dark <- isTRUE(input$dark_mode)
    text_color <- if(is_dark) "#f8f9fa" else "#343a40"
    
    tags$div(
      class = "mb-4",
      tags$p("Welcome to the Heart Disease AI Assistant! I can answer questions about heart disease, 
           explain the dashboard visualizations, or provide insights about the dataset.
           What would you like to know?"),
      style = paste0("color: ", text_color, ";")
    )
  })
  
  # Render the chat container with reactive colors
  output$chat_container <- renderUI({
    is_dark <- isTRUE(input$dark_mode)
    bg_color <- if(is_dark) "#343a40" else "#f8f9fa"
    text_color <- if(is_dark) "#f8f9fa" else "#f8f9fa"
    border_color <- if(is_dark) "#212529" else "#dee2e6"
    
    tags$div(
      id = "chat-history",
      class = "chat",
      style = paste0("height: 530px; overflow-y: auto; background-color: ", bg_color, 
                     "; border: 1px solid ", border_color,
                     "; border-radius: 5px; padding: 15px; margin-bottom: 15px; color: ", 
                     text_color, ";"),
      # Connect the output to the UI
      uiOutput("chat_display")
    )
  })
  
  # Function to create chat HTML with reactive colors
  create_chat_html <- function() {
    is_dark <- isTRUE(input$dark_mode)
    
    # Define colors based on theme
    user_bg <- if(is_dark) "#495057" else "#e9ecef"
    user_text <- if(is_dark) "#f8f9fa" else "#343a40"
    
    assistant_bg <- "#dc3545"  # Keep the red theme for assistant messages
    assistant_text <- "#ffffff"
    
    chat_elements <- tagList()
    
    for (msg in chat_history$messages) {
      if (msg$role == "user") {
        # User message (right-aligned)
        chat_elements <- tagAppendChild(
          chat_elements,
          tags$div(
            class = "d-flex justify-content-end mb-3",
            tags$div(
              class = "px-3 py-2",
              style = paste0("background-color: ", user_bg, "; color: ", user_text, 
                             "; border-radius: 15px 15px 0 15px; max-width: 75%;"),
              msg$content
            )
          )
        )
      } else {
        # Assistant message (left-aligned)
        chat_elements <- tagAppendChild(
          chat_elements,
          tags$div(
            class = "d-flex justify-content-start mb-3",
            tags$div(
              class = "px-3 py-2",
              style = paste0("background-color: ", assistant_bg, "; color: ", assistant_text, 
                             "; border-radius: 15px 15px 15px 0; max-width: 75%;"),
              msg$content
            )
          )
        )
      }
    }
    
    return(chat_elements)
  }
    
  observe({
    # Get the current theme value from the reactive
    is_dark <- input$dark_mode
    
    # Update thematic when theme changes
    if(is_dark) {
      thematic::thematic_on(
        bg = "#343a40",
        fg = "#f8f9fa",
        accent = "#dc3545",
        font = "Source Sans Pro",
        qualitative = c("#dc3545", "#fd7e14", "#ffc107", "#28a745", "#20c997", "#17a2b8", "#007bff", "#6f42c1", "#e83e8c")
      )
    } else {
      thematic::thematic_on(
        bg = "#ffffff",  # Use white background for light theme
        fg = "#343a40",  # Use dark text for light theme
        accent = "#dc3545",
        font = "Source Sans Pro",
        qualitative = c("#dc3545", "#fd7e14", "#ffc107", "#28a745", "#20c997", "#17a2b8", "#007bff", "#6f42c1", "#e83e8c")
      )
    }
  })
  
  
  # AI
  # Create reactive values for chat
  chat_history <- reactiveValues(messages = list(
    list(role = "assistant", content = "Welcome! I'm your Heart Disease AI Assistant. Ask me anything about heart disease, the dashboard, or the dataset.")
  ))
  
  # AI Assistant function to get responses
  get_ai_response <- function(user_message) {
    # Create system message with context about the dashboard and dataset
    system_message <- paste(
      "You are an AI assistant specialized in heart disease analysis. ",
      "You're part of a dashboard with the following features:",
      "1. Dashboard visualizations showing heart disease occurrence by sex/age, cholesterol levels, ",
      "blood pressure and heart rate trends, and exercise-induced angina prevalence.",
      "2. The dataset includes variables like age, sex, chest pain type (cp), ",
      "resting blood pressure (trestbps), cholesterol (chol), fasting blood sugar (fbs), ",
      "resting ECG (restecg), maximum heart rate (thalch), exercise-induced angina (exang), ",
      "ST depression (oldpeak), slope of peak exercise ST segment (slope), ",
      "number of major vessels colored by fluoroscopy (ca), thalassemia (thal), ",
      "and target variable 'num' indicating presence of heart disease (values > 0 mean presence).",
      "3. Keep responses focused on heart disease topics.",
      "4. If asked about technical aspects of the dashboard you're unsure about, be honest.",
      "5. Keep responses concise but informative.",
      sep = ""
    )
    
    # Create messages array
    messages <- list(
      list(role = "system", content = system_message)
    )
    
    # Add recent message history (last 5 exchanges)
    if (length(chat_history$messages) > 0) {
      recent_messages <- tail(chat_history$messages, 10)
      for (msg in recent_messages) {
        messages <- c(messages, list(list(role = msg$role, content = msg$content)))
      }
    }
    
    # Add current user message
    messages <- c(messages, list(list(role = "user", content = user_message)))
    
    # Try to call the API
    tryCatch({
      response <- httr::POST(
        url = "https://openrouter.ai/api/v1/chat/completions",
        httr::add_headers(
          "Content-Type" = "application/json",
          "Authorization" = paste("Bearer", "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"),
          "HTTP-Referer" = "https://heart-disease-dashboard.shinyapps.io",
          "X-Title" = "Heart Disease Dashboard"
        ),
        body = list(
          model = "deepseek/deepseek-r1-distill-llama-70b:free",
          messages = messages
        ),
        encode = "json"
      )
      
      # Process the response
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, "parsed")
        if (!is.null(content$choices) && length(content$choices) > 0) {
          return(content$choices[[1]]$message$content)
        }
      }
      return("I'm sorry, I couldn't process your request right now. Please try again.")
    }, error = function(e) {
      return(paste("Sorry, I encountered an error:", e$message))
    })
  }
  
  # Function to create chat HTML
  create_chat_html <- function() {
    chat_elements <- tagList()
    
    for (msg in chat_history$messages) {
      if (msg$role == "user") {
        # User message (right-aligned)
        chat_elements <- tagAppendChild(
          chat_elements,
          tags$div(
            class = "d-flex justify-content-end mb-3",
            tags$div(
              class = "px-3 py-2",
              style = "background-color: #495057; border-radius: 15px 15px 0 15px; max-width: 75%;",
              msg$content
            )
          )
        )
      } else {
        # Assistant message (left-aligned)
        chat_elements <- tagAppendChild(
          chat_elements,
          tags$div(
            class = "d-flex justify-content-start mb-3",
            tags$div(
              class = "px-3 py-2",
              style = "background-color: #dc3545; border-radius: 15px 15px 15px 0; max-width: 75%;",
              msg$content
            )
          )
        )
      }
    }
    
    return(chat_elements)
  }
  
  # Output chat display - This connects to the uiOutput in the UI
  output$chat_display <- renderUI({
    create_chat_html()
  })
  
  # Observer for sending messages
  observeEvent(input$send_message, {
    # Get user input
    user_message <- input$user_message
    
    # Validate user input
    if (is.null(user_message) || trimws(user_message) == "") {
      return()
    }
    
    # Add user message to chat history
    chat_history$messages <- c(chat_history$messages, list(list(role = "user", content = user_message)))
    
    # Clear input field
    updateTextInput(session, "user_message", value = "")
    
    # Show loading message
    showNotification("Getting response...", id = "ai_loading", type = "default")
    
    # Get AI response
    withProgress(message = "AI thinking...", {
      ai_response <- get_ai_response(user_message)
      
      # Add AI response to chat history
      chat_history$messages <- c(chat_history$messages, list(list(role = "assistant", content = ai_response)))
      
      # Remove loading notification
      removeNotification(id = "ai_loading")
    })
  })
  
  #============================================================================================
  
  # Plot 1: Heart Disease Occurrence by Sex or Age Group
  output$disease_occurrence_plot <- renderPlotly({
    is_dark <- isTRUE(input$dark_mode)
    bg_color <- if(is_dark) "#343a40" else "#ffffff"
    text_color <- if(is_dark) "#f8f9fa" else "#343a40"
    grid_color <- if(is_dark) "rgba(255,255,255,0.1)" else "rgba(0,0,0,0.1)"
    
    if(input$plot_type == "Sex") {
      # Plot disease occurrence by sex
      p <- ggplot(df_final, aes(x = sex, fill = factor(num > 0))) +
        geom_bar(position = "dodge", alpha = 0.9) +
        scale_fill_manual(values = c("#3CB371", "#dc3545"),
                          labels = c("No Disease", "Disease")) +
        labs(
          title = "Heart Disease Occurrence by Sex",
          x = "Sex", 
          y = "Count", 
          fill = "Heart Disease Status"
        ) +
        theme_minimal() +
        theme(
          text = element_text(family = "'Source Sans Pro', sans-serif"),
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = bg_color, color = NA),
          panel.background = element_rect(fill = bg_color, color = NA),
          panel.grid.major = element_line(color = grid_color),
          panel.grid.minor = element_line(color = grid_color),
          axis.text = element_text(color = text_color),
          axis.title = element_text(color = text_color, face = "bold"),
          legend.background = element_rect(fill = bg_color),
          legend.text = element_text(color = text_color),
          legend.title = element_text(color = text_color, face = "bold"),
          legend.position = "bottom"
        )
      
      ggplotly(p) %>% 
        layout(
          title = list(text = "Heart Disease Occurrence by Sex", font = list(color = text_color)),  # Add this line
          legend = list(orientation = "h", y = -0.2),
          paper_bgcolor = bg_color,
          plot_bgcolor = bg_color,
          font = list(color = text_color, family = "'Source Sans Pro', sans-serif"),
          margin = list(t = 50)
        ) %>%
        config(displayModeBar = FALSE)
    } else {
      
      # Plot disease occurrence by age group
      p <- df_final %>%
        mutate(age_group = cut(age, breaks = seq(min(age), max(age) + 5, by = 10),
                               labels = paste(seq(min(age), max(age), by = 10), 
                                              seq(min(age) + 9, max(age) + 9, by = 10), 
                                              sep = "-"))) %>%
        ggplot(aes(x = age_group, fill = factor(num > 0))) +
        geom_bar(position = "dodge", alpha = 0.9) +
        scale_fill_manual(values = c("#3CB371", "#dc3545"),
                          labels = c("No Disease", "Disease")) +
        labs(
          title = "Heart Disease Occurrence by Age Group",
          x = "Age Group", 
          y = "Count", 
          fill = "Heart Disease Status"
        ) +
        theme_minimal() +
        theme(
          text = element_text(family = "'Source Sans Pro', sans-serif"),
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = bg_color, color = NA),
          panel.background = element_rect(fill = bg_color, color = NA),
          panel.grid.major = element_line(color = grid_color),
          panel.grid.minor = element_line(color = grid_color),
          axis.text = element_text(color = text_color),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(color = text_color, face = "bold"),
          legend.background = element_rect(fill = bg_color),
          legend.text = element_text(color = text_color),
          legend.title = element_text(color = text_color, face = "bold"),
          legend.position = "bottom"
        )
      
      ggplotly(p) %>% 
        layout(
          title = list(text = "Heart Disease Occurrence by Age Group", font = list(color = text_color)),  # Add this line
          legend = list(orientation = "h", y = -0.2),
          paper_bgcolor = bg_color,
          plot_bgcolor = bg_color,
          font = list(color = text_color, family = "'Source Sans Pro', sans-serif"),
          margin = list(t = 50)
        ) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  # Plot 2: Cholesterol Levels by Sex or Age Group
  output$cholesterol_plot <- renderPlotly({
    is_dark <- isTRUE(input$dark_mode)
    bg_color <- if(is_dark) "#343a40" else "#ffffff"
    text_color <- if(is_dark) "#f8f9fa" else "#343a40"
    grid_color <- if(is_dark) "rgba(255,255,255,0.1)" else "rgba(0,0,0,0.1)"
    
    if(input$plot_type == "Sex") {
      # Plot cholesterol levels by sex
      p <- ggplot(df_final, aes(x = sex, y = chol, fill = sex)) +
        geom_violin(alpha = 0.7) +
        geom_boxplot(width = 0.2, alpha = 0.9, outlier.color = "#dc3545") +
        scale_fill_manual(values = c("#007bff", "#dc3545")) +
        labs(
          title = "Cholesterol Distribution by Sex",
          x = "Sex", 
          y = "Cholesterol Level (mg/dl)"
        ) +
        theme_minimal() +
        theme(
          text = element_text(family = "'Source Sans Pro', sans-serif"),
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = bg_color, color = NA),
          panel.background = element_rect(fill = bg_color, color = NA),
          panel.grid.major = element_line(color = grid_color),
          panel.grid.minor = element_line(color = grid_color),
          axis.text = element_text(color = text_color),
          axis.title = element_text(color = text_color, face = "bold"),
          legend.position = "none"
        )
      
      ggplotly(p) %>% 
        layout(
          title = list(text = "Cholesterol Distribution by Sex", font = list(color = text_color)),  # Add this line
          paper_bgcolor = bg_color,
          plot_bgcolor = bg_color,
          font = list(color = text_color, family = "'Source Sans Pro', sans-serif"),
          margin = list(t = 50)
        ) %>%
        config(displayModeBar = FALSE)
    } else {
      # Plot cholesterol levels by age group
      p <- df_final %>%
        mutate(age_group = cut(age, breaks = seq(min(age), max(age) + 5, by = 10),
                               labels = paste(seq(min(age), max(age), by = 10), 
                                              seq(min(age) + 9, max(age) + 9, by = 10), 
                                              sep = "-"))) %>%
        ggplot(aes(x = age_group, y = chol, fill = age_group)) +
        geom_boxplot(alpha = 0.8, outlier.color = "#dc3545") +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "white") +
        labs(
          title = "Cholesterol Levels by Age Group",
          x = "Age Group", 
          y = "Cholesterol Level (mg/dl)"
        ) +
        theme_minimal() +
        theme(
          text = element_text(family = "'Source Sans Pro', sans-serif"),
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = bg_color, color = NA),
          panel.background = element_rect(fill = bg_color, color = NA),
          panel.grid.major = element_line(color = grid_color),
          panel.grid.minor = element_line(color = grid_color),
          axis.text = element_text(color = text_color),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(color = text_color, face = "bold"),
          legend.position = "none"
        )
      
      ggplotly(p) %>% 
        layout(
          title = list(text = "Cholesterol Levels by Age Group", font = list(color = text_color)),  # Add this line
          paper_bgcolor = bg_color,
          plot_bgcolor = bg_color,
          font = list(color = text_color, family = "'Source Sans Pro', sans-serif"),
          margin = list(t = 50)
        ) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  # Plot 3: Blood Pressure and Heart Rate by Sex or Age Group
  output$bp_hr_plot <- renderPlotly({
    is_dark <- isTRUE(input$dark_mode)
    bg_color <- if(is_dark) "#343a40" else "#ffffff"
    text_color <- if(is_dark) "#f8f9fa" else "#343a40"
    grid_color <- if(is_dark) "rgba(255,255,255,0.1)" else "rgba(0,0,0,0.1)"
    
    if(input$plot_type == "Sex") {
      # Plot BP and HR by sex with dual y-axes
      # Create a wider data format for plotly
      bp_data <- df_final %>%
        select(sex, trestbps) %>%
        rename(value = trestbps) %>%
        mutate(measure = "Blood Pressure")
      
      hr_data <- df_final %>%
        select(sex, thalch) %>%
        rename(value = thalch) %>%
        mutate(measure = "Heart Rate")
      
      combined_data <- bind_rows(bp_data, hr_data)
      
      p <- plot_ly(combined_data, height = 300) %>%
        add_boxplot(
          x = ~sex,
          y = ~value,
          color = ~measure,
          colors = c("#1E90FF", "#FF8C00"),
          boxpoints = "outliers",
          jitter = 0.3,
          pointpos = -1.8,
          boxmean = TRUE,
          showlegend = TRUE,
          type = "box",
          name = ~measure
        ) %>%
        layout(
          title = list(
            text = "Blood Pressure and Heart Rate by Sex",
            font = list(family = "'Source Sans Pro', sans-serif", size = 16)
          ),
          xaxis = list(
            title = "Sex",
            tickfont = list(family = "'Source Sans Pro', sans-serif"),
            color = text_color
          ),
          yaxis = list(
            title = "Value",
            tickfont = list(family = "'Source Sans Pro', sans-serif"),
            color = text_color
          ),
          legend = list(
            orientation = "h",
            y = -0.2, 
            x = 0.5, 
            xanchor = "center",
            font = list(family = "'Source Sans Pro', sans-serif")
          ),
          paper_bgcolor = bg_color,
          plot_bgcolor = bg_color,
          font = list(color = text_color, family = "'Source Sans Pro', sans-serif")
        ) %>%
        config(displayModeBar = FALSE)
      
      p
    } else {
      # Plot BP and HR by age group as a line chart
      age_data <- df_final %>%
        mutate(age_group = cut(age, breaks = seq(min(age), max(age) + 5, by = 10),
                               labels = paste(seq(min(age), max(age), by = 10), 
                                              seq(min(age) + 9, max(age) + 9, by = 10), 
                                              sep = "-"))) %>%
        group_by(age_group) %>%
        summarize(
          avg_bp = mean(trestbps, na.rm = TRUE),
          avg_hr = mean(thalch, na.rm = TRUE),
          sd_bp = sd(trestbps, na.rm = TRUE),
          sd_hr = sd(thalch, na.rm = TRUE)
        )
      
      p <- plot_ly(height = 300) %>%
        add_trace(
          data = age_data,
          x = ~age_group,
          y = ~avg_bp,
          name = "Blood Pressure",
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#1E90FF", width = 3),
          marker = list(color = "#1E90FF", size = 8),
          error_y = list(
            type = "data",
            array = ~sd_bp,
            color = "#1E90FF",
            thickness = 1.5,
            width = 3
          )
        ) %>%
        add_trace(
          data = age_data,
          x = ~age_group,
          y = ~avg_hr,
          name = "Heart Rate",
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#FF8C00", width = 3),
          marker = list(color = "#FF8C00", size = 8),
          error_y = list(
            type = "data",
            array = ~sd_hr,
            color = "#FF8C00",
            thickness = 1.5,
            width = 3
          )
        ) %>%
        layout(
          title = list(
            text = "Average Blood Pressure and Heart Rate by Age Group",
            font = list(family = "'Source Sans Pro', sans-serif", size = 16)
          ),
          xaxis = list(
            title = "Age Group",
            tickfont = list(family = "'Source Sans Pro', sans-serif"),
            color = text_color,
            tickangle = 45
          ),
          yaxis = list(
            title = "Value",
            tickfont = list(family = "'Source Sans Pro', sans-serif"),
            color = text_color
          ),
          legend = list(
            orientation = "h",
            y = -0.2, 
            x = 0.5, 
            xanchor = "center",
            font = list(family = "'Source Sans Pro', sans-serif")
          ),
          paper_bgcolor = bg_color,
          plot_bgcolor = bg_color,
          font = list(color = text_color, family = "'Source Sans Pro', sans-serif")
        ) %>%
        config(displayModeBar = FALSE)
      
      p
    }
  })
  
  # Plot 4: Exercise-induced Angina by Sex or Age Group
  output$angina_plot <- renderPlotly({
    is_dark <- isTRUE(input$dark_mode)
    bg_color <- if(is_dark) "#343a40" else "#ffffff"
    text_color <- if(is_dark) "#f8f9fa" else "#343a40"
    grid_color <- if(is_dark) "rgba(255,255,255,0.1)" else "rgba(0,0,0,0.1)"
    
    if(input$plot_type == "Sex") {
      # Plot angina by sex with enhanced visuals
      angina_by_sex <- df_final %>%
        group_by(sex, exang) %>%
        summarize(count = n(), .groups = 'drop') %>%
        group_by(sex) %>%
        mutate(percentage = count / sum(count) * 100)
      
      p <- plot_ly(height = 300) %>%
        add_trace(
          data = angina_by_sex,
          x = ~sex,
          y = ~percentage,
          color = ~exang,
          colors = c("#1E90FF", "#FF6347"),
          type = "bar",
          text = ~paste0(round(percentage, 1), "%"),
          textposition = "auto",
          hoverinfo = "text",
          hovertext = ~paste0(
            "Sex: ", sex, "<br>",
            "Status: ", ifelse(exang == "TRUE", "Angina", "No Angina"), "<br>",
            "Count: ", count, "<br>",
            "Percentage: ", round(percentage, 1), "%"
          )
        ) %>%
        layout(
          title = list(
            text = "Exercise-induced Angina by Sex",
            font = list(family = "'Source Sans Pro', sans-serif", size = 16)
          ),
          xaxis = list(
            title = "Sex",
            tickfont = list(family = "'Source Sans Pro', sans-serif"),
            color = text_color
          ),
          yaxis = list(
            title = "Percentage (%)",
            tickfont = list(family = "'Source Sans Pro', sans-serif"),
            color = text_color,
            range = c(0, 100)
          ),
          barmode = "stack",
          legend = list(
            title = list(text = "Exercise-induced Angina"),
            orientation = "h",
            y = -0.2, 
            x = 0.5, 
            xanchor = "center",
            font = list(family = "'Source Sans Pro', sans-serif")
          ),
          paper_bgcolor = bg_color,
          plot_bgcolor = bg_color,
          font = list(color = text_color, family = "'Source Sans Pro', sans-serif")
        ) %>%
        config(displayModeBar = FALSE)
      
      p
    } else {
      # Plot angina by age group with enhanced visuals
      angina_by_age <- df_final %>%
        mutate(age_group = cut(age, breaks = seq(min(age), max(age) + 5, by = 10),
                               labels = paste(seq(min(age), max(age), by = 10), 
                                              seq(min(age) + 9, max(age) + 9, by = 10), 
                                              sep = "-"))) %>%
        group_by(age_group, exang) %>%
        summarize(count = n(), .groups = 'drop') %>%
        group_by(age_group) %>%
        mutate(percentage = count / sum(count) * 100)
      
      p <- plot_ly(height = 300) %>%
        add_trace(
          data = angina_by_age,
          x = ~age_group,
          y = ~percentage,
          color = ~exang,
          colors = c("#1E90FF", "#FF6347"),
          type = "bar",
          text = ~paste0(round(percentage, 1), "%"),
          textposition = "auto",
          hoverinfo = "text",
          hovertext = ~paste0(
            "Age Group: ", age_group, "<br>",
            "Status: ", ifelse(exang == "TRUE", "Angina", "No Angina"), "<br>",
            "Count: ", count, "<br>",
            "Percentage: ", round(percentage, 1), "%"
          )
        ) %>%
        layout(
          title = list(
            text = "Exercise-induced Angina by Age Group",
            font = list(family = "'Source Sans Pro', sans-serif", size = 16)
          ),
          xaxis = list(
            title = "Age Group",
            tickfont = list(family = "'Source Sans Pro', sans-serif"),
            color = text_color,
            tickangle = 45
          ),
          yaxis = list(
            title = "Percentage (%)",
            tickfont = list(family = "'Source Sans Pro', sans-serif"),
            color = text_color,
            range = c(0, 100)
          ),
          barmode = "stack",
          legend = list(
            title = list(text = "Exercise-induced Angina"),
            orientation = "h",
            y = -0.2, 
            x = 0.5, 
            xanchor = "center",
            font = list(family = "'Source Sans Pro', sans-serif")
          ),
          paper_bgcolor = bg_color,
          plot_bgcolor = bg_color,
          font = list(color = text_color, family = "'Source Sans Pro', sans-serif")
        ) %>%
        config(displayModeBar = FALSE)
      
      p
    }
  })}

shinyApp(ui, server)
# 4th version