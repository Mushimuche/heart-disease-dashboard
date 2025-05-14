# Load libraries
library(tidyverse)
library(mice)        # For PMM (numeric imputation)
library(missForest)  # For Random Forest imputation


urlfile = "https://raw.githubusercontent.com/Mushimuche/heart-disease-dashboard/refs/heads/main/heart_disease_uci.csv"

df <- read_csv(url(urlfile))

head(df)

glimpse(df)

colSums(is.na(df))

sum(duplicated(df$id))

df <- df %>% select(-id)

df <- df %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.logical), as.factor))


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

library(shiny)
library(bs4Dash)
library(plotly)
library(thematic)
library(waiter)
library(fresh)
library(textshaping)


home_tab <- tabItem(
  tabName = "home",
  
  jumbotron(
    title = "welcome!",
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
               title = "Khinje Nigguh",
               subtitle = "BSCS Student",
               image = "https://img.freepik.com/free-psd/3d-rendering-kawaii-animal-icon_23-2151646218.jpg?t=st=1746961502~exp=1746965102~hmac=90544668d3e09481b1b229a70f5ed4ec225f5514ddf85e16a1cd78269e6dd5e8&w=826",
               type = 1
             ),
             status = "teal",
             "nigggguhhhhh"
           )
    ),
    column(width = 4,
           userBox(
             width = NULL,
             collapsible = FALSE,
             title = userDescription(
               title = "Rui Palabon",
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
               title = "Ianigguh",
               subtitle = "BSCS Student",
               image = "https://img.freepik.com/free-psd/3d-rendering-kawaii-animal-icon_23-2151646218.jpg?t=st=1746961502~exp=1746965102~hmac=90544668d3e09481b1b229a70f5ed4ec225f5514ddf85e16a1cd78269e6dd5e8&w=826",
               type = 1
             ),
             status = "gray",
             "this guy"
           )
    )
  )
)


predict_tab <- tabItem(
  
  tabName = "predict_tab"
  
)

ai_tab <- tabItem(
  
  tabName = "ai_tab"
  # Khinje diri ka mag edit
  
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
      # Patients with Diabetes (fbs == TRUE)
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

ui <- dashboardPage (
  
  dark = TRUE,
  help = NULL,
  fullscreen = TRUE,
  
  title = "Heart Disease Dashboard",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Heart Disease",
      image = "https://cdn-icons-png.flaticon.com/512/1048/1048469.png"
    )

  ),
  sidebar = dashboardSidebar(
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
        icon = icon("bar-chart")
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
  body = dashboardBody(
    tabItems(
      home_tab,
      dashboard_tab,
      predict_tab,
      ai_tab
    )
  )
  
)

# Dark Mode functionality
observeEvent(input$dark_mode, {
  toast(
    title = if (input$dark_mode) "Dark theme on!" else "Light theme on",
    options = list(position = "topRight", class = "bg-warning", autohide = TRUE)
  )
})

server <- function(input, output) {
  # Plot 1: Heart Disease Occurrence by Sex or Age Group
  output$disease_occurrence_plot <- renderPlotly({
    if(input$plot_type == "Sex") {
      # Plot disease occurrence by sex
      p <- ggplot(df_final, aes(x = sex, fill = factor(num > 0))) +
        geom_bar(position = "dodge") +
        scale_fill_manual(values = c("#3CB371", "#FF6347"),
                          labels = c("No Disease", "Disease")) +
        labs(x = "Sex", y = "Count", fill = "Heart Disease Status") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      ggplotly(p) %>% 
        layout(legend = list(orientation = "h", y = -0.2))
    } else {
      # Plot disease occurrence by age group
      p <- df_final %>%
        mutate(age_group = cut(age, breaks = seq(min(age), max(age) + 5, by = 10),
                               labels = paste(seq(min(age), max(age), by = 10), 
                                              seq(min(age) + 9, max(age) + 9, by = 10), 
                                              sep = "-"))) %>%
        ggplot(aes(x = age_group, fill = factor(num > 0))) +
        geom_bar(position = "dodge") +
        scale_fill_manual(values = c("#3CB371", "#FF6347"),
                          labels = c("No Disease", "Disease")) +
        labs(x = "Age Group", y = "Count", fill = "Heart Disease Status") +
        theme_minimal() +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p) %>% 
        layout(legend = list(orientation = "h", y = -0.2))
    }
  })
  
  # Plot 2: Cholesterol Levels by Sex or Age Group
  output$cholesterol_plot <- renderPlotly({
    if(input$plot_type == "Sex") {
      # Plot cholesterol levels by sex
      p <- ggplot(df_final, aes(x = sex, y = chol, fill = sex)) +
        geom_boxplot() +
        labs(x = "Sex", y = "Cholesterol Level (mg/dl)") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p)
    } else {
      # Plot cholesterol levels by age group
      p <- df_final %>%
        mutate(age_group = cut(age, breaks = seq(min(age), max(age) + 5, by = 10),
                               labels = paste(seq(min(age), max(age), by = 10), 
                                              seq(min(age) + 9, max(age) + 9, by = 10), 
                                              sep = "-"))) %>%
        ggplot(aes(x = age_group, y = chol)) +
        geom_boxplot(fill = "#69b3a2") +
        labs(x = "Age Group", y = "Cholesterol Level (mg/dl)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    }
  })
  
  # Plot 3: Blood Pressure and Heart Rate by Sex or Age Group
  output$bp_hr_plot <- renderPlotly({
    if(input$plot_type == "Sex") {
      # Plot BP and HR by sex with two y-axes
      p <- ggplot(df_final) +
        geom_boxplot(aes(x = sex, y = trestbps, fill = "Blood Pressure")) +
        geom_boxplot(aes(x = sex, y = thalch, fill = "Heart Rate"), 
                     position = position_dodge(width = 0.75)) +
        scale_fill_manual(values = c("Blood Pressure" = "#1E90FF", "Heart Rate" = "#FF8C00")) +
        labs(x = "Sex", y = "Value", fill = "Measurement") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      ggplotly(p) %>% 
        layout(legend = list(orientation = "h", y = -0.2))
    } else {
      # Plot BP and HR by age group
      p <- df_final %>%
        mutate(age_group = cut(age, breaks = seq(min(age), max(age) + 5, by = 10),
                               labels = paste(seq(min(age), max(age), by = 10), 
                                              seq(min(age) + 9, max(age) + 9, by = 10), 
                                              sep = "-"))) %>%
        ggplot() +
        stat_summary(aes(x = age_group, y = trestbps, color = "Blood Pressure"), 
                     fun = mean, geom = "line", group = 1, size = 1.2) +
        stat_summary(aes(x = age_group, y = thalch, color = "Heart Rate"), 
                     fun = mean, geom = "line", group = 2, size = 1.2) +
        scale_color_manual(values = c("Blood Pressure" = "#1E90FF", "Heart Rate" = "#FF8C00")) +
        labs(x = "Age Group", y = "Average Value", color = "Measurement") +
        theme_minimal() +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p) %>% 
        layout(legend = list(orientation = "h", y = -0.2))
    }
  })
  
  # Plot 4: Exercise-induced Angina by Sex or Age Group
  output$angina_plot <- renderPlotly({
    if(input$plot_type == "Sex") {
      # Plot angina by sex
      p <- ggplot(df_final, aes(x = sex, fill = exang)) +
        geom_bar(position = "fill") +
        scale_fill_manual(values = c("#1E90FF", "#FF6347"),
                          labels = c("No Angina", "Angina")) +
        labs(x = "Sex", y = "Proportion", fill = "Exercise-induced Angina") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      ggplotly(p) %>% 
        layout(legend = list(orientation = "h", y = -0.2))
    } else {
      # Plot angina by age group
      p <- df_final %>%
        mutate(age_group = cut(age, breaks = seq(min(age), max(age) + 5, by = 10),
                               labels = paste(seq(min(age), max(age), by = 10), 
                                              seq(min(age) + 9, max(age) + 9, by = 10), 
                                              sep = "-"))) %>%
        ggplot(aes(x = age_group, fill = exang)) +
        geom_bar(position = "fill") +
        scale_fill_manual(values = c("#1E90FF", "#FF6347"),
                          labels = c("No Angina", "Angina")) +
        labs(x = "Age Group", y = "Proportion", fill = "Exercise-induced Angina") +
        theme_minimal() +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p) %>% 
        layout(legend = list(orientation = "h", y = -0.2))
    }
  })
}

shinyApp(ui, server)


