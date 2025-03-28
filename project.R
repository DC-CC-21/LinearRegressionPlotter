library(shiny)
library(bslib)
library(ggplot2)

# Configurations
title <- "Linear Regression Plotter"
smooth_is_default <- TRUE
sequence_max <- 10
sequence_step <- 0.1



# Default dataset to use if there is not file loaded
default_data <- data.frame(
  x = seq(0, sequence_max, sequence_step)
)
default_data$y <- sample(default_data$x)
default_data$z <- rnorm(length(default_data$x), mean = 8, sd = 0.2)
x_range <- c(0, 10)

r_plot <- function(x, y, smooth = FALSE, range, plot_labels = c("x", "y")) {
  # Filter out values outside of the range
  valid_indices <- x >= range[1] & x <= range[2]

  x_filtered <- c()
  for (index in seq(1, length(valid_indices))) {
    if (valid_indices[index]) {
      x_filtered <- c(x_filtered, x[index])
    }
  }
  x <- x_filtered
  y <- y[valid_indices]

  # Fit the line
  model <- lm(y ~ x)

  # Create the plot
  gplot <- ggplot() +
    labs(x = paste(plot_labels[1], "Axis"), y = paste(plot_labels[2], "Axis")) +
    geom_line(aes(x, y)) +
    geom_line(aes(x, fitted(model)), col = "#ff0000") +
    labs(title = "Linear Regression Plot")

  # If smooth is on, add the smooth line
  if (smooth) {
    gplot <- gplot + geom_smooth(aes(x, y), method = "loess", formula = y ~ x)
  }

  # Return the plot and model
  return(list(plot = gplot, model = model))
}


ui <- page_sidebar(
  title = title,
  sidebar = sidebar(
    width = 300,
    helpText(
      "Create a linear regression model"
    ),
    card(
      "Configure File",
      fileInput(
        "file",
        "Choose a File"
      ),
      actionButton(
        "clear",
        "Clear Data"
      )
    ),
    card(
      "Configure Plot",
      selectInput(
        "x_column", "X Column",
        names(default_data),
        selected = names(default_data)[1]
      ),
      selectInput(
        "y_column", "Y Column",
        names(default_data),
        selected = names(default_data)[2]
      ),
      sliderInput(
        "xrange",
        "X Range",
        min = x_range[1],
        max = x_range[2],
        value = x_range,
      ),
      checkboxInput("showsmooth", "Show Smooth", value = smooth_is_default)
    ),
  ),
  mainPanel(
    textOutput("summary"),
    textOutput("coefficients"),
    plotOutput("plot"),
  )
)

server <- function(input, output, session) {
  data <- reactive({
    if (is.null(input$file)) {
      # If the file is not defined, return the default dataset
      return(default_data)
    } else {
      # Else, return the file's data
      read.csv2(input$file$datapath)
    }
  })

  xrange <- reactive({
    req(input$xrange)
    if (input$xrange[1] == input$xrange[2]) {
      # If the range min and max are the same, return the range of
      # the data to prevent an error
      d <- data()
      range <- c(
        min(d[[input$x_column]]),
        max(d[[input$x_column]])
      )
      c(range[1], range[2])
    } else {
      # Else, return the range
      c(input$xrange[1], input$xrange[2])
    }
  })

  observe({
    # Check if the file is defined
    req(input$file)
    col_names <- names(data())

    # If the file exists, update the dropdowns
    updateSelectInput(
      session,
      "x_column", "X Column",
      choices = col_names,
      selected = col_names[1]
    )
    updateSelectInput(
      session,
      "y_column", "Y Column",
      choices = col_names,
      selected = col_names[3]
    )
  })

  observe({
    # Update the slider
    d <- data()
    range <- c(
      min(d[[input$x_column]]),
      max(d[[input$x_column]])
    )

    updateSliderInput(
      session,
      "xrange",
      "X Range",
      min = range[1],
      max = range[2],
      value = range
    )
  })

  observe({
    # Gather the data and check if the column exists
    d <- data()
    col_names <- names(d)

    req(input$x_column %in% col_names)
    req(input$y_column %in% col_names)

    # Create the plot and model
    plot_values <- r_plot(
      x = d[[input$x_column]],
      y = d[[input$y_column]],
      smooth = input$showsmooth,
      range = xrange(),
      plot_labels = c(input$x_column, input$y_column)
    )
    model <- plot_values$model
    result <- summary(model)
    coefficients <- coef(model)

    # Display the plot
    output$plot <- renderPlot({
      plot_values$plot
    })


    # Show the R-squared value
    output$summary <- renderText({
      paste("R-squared: ", round(result$r.squared, 2))
    })

    # Show the equation of the line
    output$coefficients <- renderText({
      paste(
        "Equation of line: y = ",
        round(coefficients[2], 2),
        "x + ",
        round(coefficients[1], 2),
        sep = ""
      )
    })
  })
}

shinyApp(ui = ui, server = server)
