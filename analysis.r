# Load required library
install.packages("readr")  # Run once if not installed
library(readr)

# Read the CSV file
data <- read_csv("NUTSEVWASTINGPREV,NUTSEVWASTINGNUM.csv")

# View first few rows
head(data)

# Summary statistics
summary(data)


# Extract numeric values for "Severe wasted numbers"
data$severe_wasted_numbers <- as.numeric(gsub(" \\[.*\\]", "", data$`Severe wasted numbers among children under 5 years of age (millions), model-based estimates`))

# Extract numeric values for "Severe wasting prevalence"
data$severe_wasting_prevalence <- as.numeric(gsub(" \\[.*\\]", "", data$`Severe wasting prevalence among children under 5 years of age (%), model-based estimates`))

# View cleaned data
head(data)

# Load ggplot2 for visualization
install.packages("ggplot2")  # Run once if not installed
library(ggplot2)

# Plot the severe wasting prevalence by region
ggplot(data, aes(x = `UN SDG Region`, y = severe_wasting_prevalence)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Severe Wasting Prevalence by Region", y = "Severe Wasting Prevalence (%)", x = "Region")
# Save the cleaned data to a new CSV file
write.csv(data, "cleaned_data.csv", row.names = FALSE)


# Load ggplot2 for visualization
library(ggplot2)

# Bar plot for Severe Wasting Prevalence by Region
ggplot(data, aes(x = `UN SDG Region`, y = severe_wasting_prevalence)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Severe Wasting Prevalence by Region", y = "Severe Wasting Prevalence (%)", x = "Region") +
  theme_minimal()
# Scatter plot for Severe Wasting Numbers vs Prevalence
ggplot(data, aes(x = severe_wasted_numbers, y = severe_wasting_prevalence)) +
  geom_point(color = "red") +
  labs(title = "Severe Wasting Numbers vs. Prevalence", x = "Severe Wasting Numbers (Millions)", y = "Severe Wasting Prevalence (%)") +
  theme_minimal()
# Build a linear regression model
model <- lm(severe_wasting_prevalence ~ severe_wasted_numbers, data = data)

# Summary of the model
summary(model)

# Visualize the model's predictions
ggplot(data, aes(x = severe_wasted_numbers, y = severe_wasting_prevalence)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Linear Regression: Severe Wasting Prevalence vs Wasted Numbers", x = "Severe Wasted Numbers (Millions)", y = "Severe Wasting Prevalence (%)")
# Residual plot to check model fit
plot(model$residuals, main = "Residuals of the Linear Model", ylab = "Residuals", xlab = "Fitted Values")


# Multiple linear regression example
model_multiple <- lm(severe_wasting_prevalence ~ severe_wasted_numbers + `Year`, data = data)

# Model summary
summary(model_multiple)
# Evaluate model performance
summary(model_multiple)$r.squared


install.packages("plotly")
library(plotly)
# Interactive plot using plotly
plot_ly(data, x = ~severe_wasted_numbers, y = ~severe_wasting_prevalence, type = "scatter", mode = "markers") %>%
  layout(title = "Interactive Plot: Wasting Numbers vs Prevalence", 
         xaxis = list(title = "Severe Wasted Numbers (Millions)"),
         yaxis = list(title = "Severe Wasting Prevalence (%)"))
install.packages("shiny")
library(shiny)
library(ggplot2)
install.packages("fastmap")
library(bslib)

# UI for the app
ui <- fluidPage(
  titlePanel("Interactive Wasting Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Interactive plot of Severe Wasting Numbers vs Prevalence")
    ),
    mainPanel(
      plotOutput("scatter_plot")
    )
  )
)

# Server function
server <- function(input, output) {
  output$scatter_plot <- renderPlot({
    ggplot(data, aes(x = severe_wasted_numbers, y = severe_wasting_prevalence)) +
      geom_point(color = "red") +
      labs(title = "Severe Wasting Numbers vs Prevalence")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
