options(shiny.maxRequestSize = 1024^3)

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)

test_labels <- c(
  pval_flip = "flipmeta",
  pval_knha = "Knapp-Hartung",
  pval_adhoc = "Knapp-Hartung (ad hoc)",
  pval_rob = "CR2 robust",
  pval_wald = "Wald"
)

condition_labels <- c(
  k = "Number of studies (k)",
  n = "Sample size per group (n)",
  tau2 = "Baseline heterogeneity (tau²)",
  zeta = "Scale coefficient (zeta)",
  rXZ = "Moderator correlation (rho XZ)",
  method = "Fitted model",
  bX = "Focal coefficient (beta X)",
  bZ = "Nuisance coefficient (gamma Z)",
  b0 = "Intercept",
  nX = "Number of focal moderators",
  nZ = "Number of nuisance moderators"
)

find_default_rds <- function() {
  candidates <- c(
    file.path("..", "res.rds"),
    file.path("simulation", "res.rds"),
    "res.rds"
  )
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    return(NULL)
  }
  normalizePath(existing[[1]], mustWork = TRUE)
}

pretty_test_name <- function(x) {
  known <- unname(test_labels[x])
  fallback <- gsub("_", " ", sub("^pval_", "", x))
  ifelse(is.na(known), fallback, known)
}

prepare_simulation_data <- function(path, progress = NULL) {
  raw <- readRDS(path)
  if (!is.data.frame(raw) || !"res" %in% names(raw)) {
    stop("The RDS must contain a data frame with a list-column named 'res'.")
  }

  # Backward compatibility with simulation files created before the
  # scale parameter was renamed from gamma to zeta.
  if (!"zeta" %in% names(raw) && "gamma" %in% names(raw)) {
    raw <- rename(raw, zeta = gamma)
  }

  first_result <- NULL
  for (cell in raw$res) {
    valid <- Filter(is.data.frame, cell)
    if (length(valid)) {
      first_result <- valid[[1]]
      break
    }
  }
  if (is.null(first_result)) {
    stop("No valid simulation results were found in the 'res' column.")
  }

  p_columns <- grep("^pval_", names(first_result), value = TRUE)
  if (!length(p_columns)) {
    stop("No p-value columns beginning with 'pval_' were found.")
  }

  condition_columns <- intersect(names(condition_labels), names(raw))
  output <- vector("list", nrow(raw))

  for (i in seq_len(nrow(raw))) {
    if (is.function(progress) && (i == 1L || i %% 5L == 0L)) {
      progress(
        i / nrow(raw),
        sprintf("Summarising condition %s of %s", i, nrow(raw))
      )
    }

    results_i <- bind_rows(raw$res[[i]], .id = "replication")
    if (!nrow(results_i) || !"coef" %in% names(results_i)) {
      next
    }

    focal_i <- results_i |>
      filter(.data$coef == "x1") |>
      select(any_of(c("replication", p_columns))) |>
      pivot_longer(
        cols = all_of(p_columns),
        names_to = "test",
        values_to = "p_value"
      ) |>
      group_by(.data$test) |>
      summarise(
        rejected = sum(.data$p_value <= 0.05, na.rm = TRUE),
        n_valid = sum(!is.na(.data$p_value)),
        .groups = "drop"
      )

    if (!nrow(focal_i)) {
      next
    }

    conditions_i <- raw[rep(i, nrow(focal_i)), condition_columns, drop = FALSE]
    output[[i]] <- bind_cols(conditions_i, focal_i)
  }

  summary_data <- bind_rows(output) |>
    mutate(
      test_label = factor(
        pretty_test_name(.data$test),
        levels = pretty_test_name(p_columns)
      ),
      metric = if_else(.data$bX == 0, "Type I error", "Power"),
      source_file = basename(path)
    )

  if (!nrow(summary_data)) {
    stop("The RDS did not contain usable results for coefficient 'x1'.")
  }

  list(
    data = summary_data,
    source = normalizePath(path, mustWork = TRUE),
    tests = setNames(p_columns, pretty_test_name(p_columns)),
    conditions = condition_columns
  )
}

aggregate_results <- function(data, groups) {
  data |>
    group_by(across(all_of(c(groups, "test", "test_label")))) |>
    summarise(
      rejected = sum(.data$rejected),
      n_valid = sum(.data$n_valid),
      n_conditions = n(),
      rate = .data$rejected / .data$n_valid,
      mc_se = sqrt(.data$rate * (1 - .data$rate) / .data$n_valid),
      .groups = "drop"
    )
}

initial_path <- find_default_rds()
initial_result <- if (is.null(initial_path)) NULL else prepare_simulation_data(initial_path)

ui <- page_sidebar(
  title = "flipmeta simulation explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 340,
    fileInput(
      "simulation_file",
      "Simulation results (.rds)",
      accept = ".rds",
      buttonLabel = "Choose file"
    ),
    radioButtons(
      "metric",
      "Target quantity",
      choices = c("Type I error", "Power"),
      selected = "Type I error",
      inline = TRUE
    ),
    checkboxGroupInput(
      "tests",
      "Inferential procedures",
      choices = NULL
    ),
    selectInput(
      "x_axis",
      "Horizontal axis",
      choices = NULL
    ),
    checkboxGroupInput(
      "keep_conditions",
      "Keep these conditions separate",
      choices = NULL
    ),
    helpText(
      "Conditions that are not selected are averaged using the number of valid Monte Carlo replications as weights."
    )
  ),
  layout_columns(
    value_box(
      title = "Data source",
      value = textOutput("source_name")
    ),
    value_box(
      title = "Displayed conditions",
      value = textOutput("condition_count")
    ),
    value_box(
      title = "Valid replications",
      value = textOutput("replication_count")
    ),
    col_widths = c(4, 4, 4)
  ),
  navset_card_tab(
    nav_panel(
      "Plot",
      plotlyOutput("simulation_plot", height = "650px")
    ),
    nav_panel(
      "Summary table",
      DTOutput("summary_table")
    ),
    nav_panel(
      "Available conditions",
      DTOutput("conditions_table")
    )
  )
)

server <- function(input, output, session) {
  simulation <- reactiveVal(initial_result)

  observeEvent(input$simulation_file, {
    result <- withProgress(
      message = "Loading simulation results",
      value = 0,
      prepare_simulation_data(
        input$simulation_file$datapath,
        progress = function(value, detail) {
          setProgress(value = value, detail = detail)
        }
      )
    )
    simulation(result)
  }, ignoreInit = TRUE)

  observe({
    req(simulation())
    info <- simulation()
    data <- info$data

    updateCheckboxGroupInput(
      session,
      "tests",
      choices = info$tests,
      selected = unname(info$tests)
    )

    variable_conditions <- info$conditions[
      vapply(data[info$conditions], dplyr::n_distinct, integer(1)) > 1L
    ]
    choices <- setNames(variable_conditions, condition_labels[variable_conditions])
    default_x <- if ("k" %in% variable_conditions) "k" else variable_conditions[[1]]
    default_keep <- intersect(c("tau2", "zeta", "method"), variable_conditions)

    updateSelectInput(
      session,
      "x_axis",
      choices = choices,
      selected = default_x
    )
    updateCheckboxGroupInput(
      session,
      "keep_conditions",
      choices = choices,
      selected = default_keep
    )
  })

  selected_data <- reactive({
    req(simulation(), input$metric, input$tests)
    simulation()$data |>
      filter(
        .data$metric == input$metric,
        .data$test %in% input$tests
      )
  })

  plot_data <- reactive({
    req(input$x_axis)
    facets <- setdiff(input$keep_conditions, input$x_axis)
    aggregate_results(selected_data(), unique(c(input$x_axis, facets)))
  })

  output$source_name <- renderText({
    req(simulation())
    basename(simulation()$source)
  })

  output$condition_count <- renderText({
    req(nrow(plot_data()))
    format(sum(plot_data()$n_conditions), big.mark = ",")
  })

  output$replication_count <- renderText({
    req(nrow(plot_data()))
    format(sum(plot_data()$n_valid), big.mark = ",")
  })

  output$simulation_plot <- renderPlotly({
    data <- plot_data()
    req(nrow(data), input$x_axis)

    facets <- setdiff(input$keep_conditions, input$x_axis)
    if (length(facets)) {
      data <- data |>
        unite(
          ".panel",
          all_of(facets),
          sep = " | ",
          remove = FALSE
        )
      panel_formula <- stats::as.formula("~ .panel")
    }

    y_title <- if (identical(input$metric, "Type I error")) {
      "Type I error rate"
    } else {
      "Power"
    }

    data <- data |>
      mutate(
        tooltip = paste0(
          condition_labels[[input$x_axis]], ": ", .data[[input$x_axis]],
          "<br>Method: ", .data$test_label,
          "<br>", y_title, ": ", scales::percent(.data$rate, accuracy = 0.1),
          "<br>Valid replications: ", format(.data$n_valid, big.mark = ","),
          "<br>Averaged cells: ", .data$n_conditions
        )
      )

    plot <- ggplot(
      data,
      aes(
        x = .data[[input$x_axis]],
        y = .data$rate,
        colour = .data$test_label,
        group = .data$test_label,
        text = .data$tooltip
      )
    )

    if (is.numeric(data[[input$x_axis]])) {
      plot <- plot + geom_line(linewidth = 0.7)
    }
    plot <- plot + geom_point(size = 2.4)

    if (identical(input$metric, "Type I error")) {
      plot <- plot + geom_hline(
        yintercept = 0.05,
        linetype = "dashed",
        colour = "grey45"
      )
    }
    if (length(facets)) {
      plot <- plot + facet_wrap(panel_formula)
    }

    plot <- plot +
      scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
      labs(
        x = condition_labels[[input$x_axis]],
        y = y_title,
        colour = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )

    ggplotly(plot, tooltip = "text") |>
      layout(legend = list(orientation = "h", y = -0.18))
  })

  output$summary_table <- renderDT({
    data <- plot_data()
    req(nrow(data))

    groups <- unique(c(input$x_axis, setdiff(input$keep_conditions, input$x_axis)))
    table <- data |>
      mutate(
        rate = round(.data$rate, 4),
        mc_se = round(.data$mc_se, 4),
        test = as.character(.data$test_label)
      ) |>
      select(
        all_of(groups),
        test,
        rate,
        mc_se,
        rejected,
        n_valid,
        n_conditions
      )

    datatable(
      table,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv")
      )
    )
  })

  output$conditions_table <- renderDT({
    req(simulation())
    data <- simulation()$data
    condition_columns <- simulation()$conditions

    table <- bind_rows(lapply(condition_columns, function(variable) {
      values <- sort(unique(data[[variable]]))
      data.frame(
        variable = variable,
        description = unname(condition_labels[[variable]]),
        levels = paste(values, collapse = ", "),
        n_levels = length(values)
      )
    }))

    datatable(
      table,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)
