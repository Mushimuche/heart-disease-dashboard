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
    lead = "This is a dashboard about heart disease analysis",
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
               title = "Team Member 1",
               subtitle = "BSCS Student",
               image = "https://img.freepik.com/free-psd/3d-rendering-kawaii-animal-icon_23-2151646218.jpg?t=st=1746961502~exp=1746965102~hmac=90544668d3e09481b1b229a70f5ed4ec225f5514ddf85e16a1cd78269e6dd5e8&w=826",
               type = 1
             ),
             status = "teal",
             "Team member bio here"
           )
    ),
    column(width = 4,
           userBox(
             width = NULL,
             collapsible = FALSE,
             title = userDescription(
               title = "Team Member 2",
               subtitle = "BSCS Student",
               image = "https://img.freepik.com/free-psd/3d-rendering-kawaii-animal-icon_23-2151646218.jpg?t=st=1746961502~exp=1746965102~hmac=90544668d3e09481b1b229a70f5ed4ec225f5514ddf85e16a1cd78269e6dd5e8&w=826",
               type = 1
             ),
             status = "purple",
             "Super impressive bio"
           )
    ),
    column(width = 4,
           userBox(
             width = NULL,
             collapsible = FALSE,
             title = userDescription(
               title = "Team Member 3",
               subtitle = "BSCS Student",
               image = "https://img.freepik.com/free-psd/3d-rendering-kawaii-animal-icon_23-2151646218.jpg?t=st=1746961502~exp=1746965102~hmac=90544668d3e09481b1b229a70f5ed4ec225f5514ddf85e16a1cd78269e6dd5e8&w=826",
               type = 1
             ),
             status = "gray",
             "Team member bio"
           )
    )
  )
)

predict_tab <- tabItem(
  tabName = "predict_tab",
  fluidRow(
    column(
      width = 12,
      bs4Card(
        title = "Heart Disease Prediction",
        status = "danger",
        solidHeader = TRUE,
        "Prediction functionality will be implemented here."
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
        tags$div(
          class = "mb-4",
          tags$p("Welcome to the Heart Disease AI Assistant! I can answer questions about heart disease, 
                 explain the dashboard visualizations, or provide insights about the dataset.
                 What would you like to know?"),
          style = "color: #f8f9fa;"
        ),
        
        # Chat history container with default welcome message
        tags$div(
          id = "chat-history",
          class = "chat",
          style = "height: 530px; overflow-y: auto; background-color: #343a40; 
                  border-radius: 5px; padding: 15px; margin-bottom: 15px; color: #f8f9fa;",
          # Connect the output to the UI
          uiOutput("chat_display")
        ),
        
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
        "Home",
        tabName = "home",
        icon = icon("home")
      ),
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
      )
    )
  ),
  
  controlbar = dashboardControlbar(
    
  ),
  
  # Body
  dashboardBody(
    tabItems(
      home_tab,
      dashboard_tab,
      predict_tab,
      ai_tab
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

# 3rd version