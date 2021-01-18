#### compare_images ############################################################

### Prepare data ###############################################################

# Load objects
result <- shiny::getShinyOption("result")
result_full <- shiny::getShinyOption("result_full")
result_corr <- shiny::getShinyOption("result_corr")
x_dir <- shiny::getShinyOption("x_dir")
y_dir <- shiny::getShinyOption("y_dir")
remove_duplicates <- shiny::getShinyOption("remove_duplicates")
if (remove_duplicates) result_b <- shiny::getShinyOption("result_b")
shiny::addResourcePath("x", x_dir)
shiny::addResourcePath("y", y_dir)

# Copy images to temp folders
file.copy(result$x_name, x_dir)
file.copy(result$y_name, y_dir)

# Make new path vectors
x_paths <- strsplit(result$x_name, '/')
x_paths <- sapply(x_paths, function(x) x[[length(x)]])
x_paths <- paste0("x/", x_paths)
y_paths <- strsplit(result$y_name, '/')
y_paths <- sapply(y_paths, function(x) x[[length(x)]])
y_paths <- paste0("y/", y_paths)


### UI object ##################################################################

ui <- shiny::fluidPage(

  # Load waiter
  waiter::use_waiter(),

  # App title
  shiny::titlePanel(paste0(
    "Image comparison for ", prettyNum(nrow(result), big.mark = ","), 
    " matches")),

  # Filter data frame
  shiny::fluidRow(
    shiny::column(
      width = 12,
      align = "center",
      shiny::radioButtons(
        inputId = "filter",
        label = "Filter results",
        choiceNames = c(
          "All results",
          paste0("Match (",
                 prettyNum(nrow(result[result$match == "match",]),
                           big.mark = ","),
                 ")"),
          paste0("Likely match (",
                 prettyNum(nrow(result[result$match == "likely match",]),
                           big.mark = ","),
                 ")"),
          paste0("Possible match (",
                 prettyNum(nrow(result[result$match == "possible match",
                                       ]), big.mark = ","),
                 ")"),
          paste0("No match (",
                 prettyNum(nrow(result[result$match == "no match",]),
                           big.mark = ","),
                 ")")
          ),
        choiceValues = c("all", "match", "likely match", "possible match",
                         "no match"),
        inline = TRUE))
  ),

  # Save output
  shiny::fluidRow(
    shiny::column(width = 12,
                  shiny::actionButton("save", "Save changes and exit"),
                  align = "center")
  ),

  shiny::fluidRow(shiny::column(width = 12, style = "height:50px")),

  # Display images
  shiny::fluidRow(
    shiny::column(width = 4, shiny::uiOutput("match"), align = "right"),
    shiny::column(width = 4, shiny::htmlOutput("image_1"), align = "right"),
    shiny::column(width = 4, shiny::htmlOutput("image_2"))
  ),

  # Save output
  shiny::fluidRow(
    shiny::column(width = 12,
                  shiny::actionButton("save_2", "Save changes and exit"),
                  align = "center")
    ),

  shiny::fluidRow(shiny::column(width = 12, style = "height:50px")),

  waiter::waiter_show_on_load(
    html = shiny::tagList(
      if (remove_duplicates) h3("Removing duplicates", style = "color:gray"),
      waiter::spin_3circles()), color = "white")
)


### Server object ##############################################################

server <- function(input, output) {

  ## Make change_table, match_vector and active_index --------------------------

  if (remove_duplicates) {
    change_table <- result[c(".UID", "x_name", "y_name", "match",
                             "x_id", "y_id")]
  } else change_table <- result[c(".UID", "x_name", "y_name", "match")]

  colnames(change_table)[colnames(change_table) == "match"] <-
    "new_match_status"

  change_table$new_match_status <-
    ifelse(change_table$new_match_status %in% c("match", "likely match"),
           "match", "no match")

  match_vector <-
    do.call(
      shiny::reactiveValues,
      as.list(setNames(change_table$new_match_status, change_table$.UID))
      )

  active_index <- shiny::reactive(
    if (input$filter == "all") seq_len(nrow(result)) else
      which(result$match == input$filter)
  )


  ## Produce match status ------------------------------------------------------

  output$match <- shiny::renderUI({

    match_buttons <- lapply(result$.UID[active_index()], function(x) {

      shiny::actionButton(paste0("match_", x),
                          "Match",
                          style = "height:123px",
                          class = if (match_vector[[x]] == "match") {
                            "btn-lg btn-block btn-success"
                            } else "btn-lg btn-block btn-default"
                          )
      })

    no_match_buttons <- lapply(result$.UID[active_index()], function(x) {

      shiny::actionButton(paste0("no_match_", x),
                          "No match",
                          style = "height:122px",
                          class = if (match_vector[[x]] == "match") {
                            "btn-lg btn-block btn-default"
                            } else "btn-lg btn-block btn-danger"
                          )
      })

    lines <- lapply(seq_along(active_index()), function(x) shiny::hr())
    together <- c(rbind(match_buttons, no_match_buttons, lines))

    do.call(shiny::tagList, together)

  })


  ## Produce images ------------------------------------------------------------

  output$image_1 <- shiny::renderUI({

    images <- lapply(x_paths[active_index()], function(x) {
      shiny::img(src = x, width = "250px", height = "250px")
      })

    lines <- lapply(x_paths[active_index()], function(x) shiny::hr())
    together <- c(rbind(images, lines))

    do.call(shiny::tagList, together)

  })

  output$image_2 <- shiny::renderUI({

    images <- lapply(y_paths[active_index()], function(x) {
      shiny::img(src = x, width = "250px", height = "250px")
      })

    lines <- lapply(y_paths[active_index()], function(x) shiny::hr())
    together <- c(rbind(images, lines))

    do.call(shiny::tagList, together)

  })


  ## Get changes ---------------------------------------------------------------

  lapply(result$.UID, function(x) {

    shiny::observeEvent(
      input[[paste0("match_", x)]],
      {match_vector[[x]] <- "match"}
      )

    shiny::observeEvent(
      input[[paste0("no_match_", x)]],
      {match_vector[[x]] <- "no match"}
      )

  })


  ## Exit function -------------------------------------------------------------

  shiny::observe({
    if (input$save > 0 || input$save_2 > 0) {

      final_vector <- unlist(shiny::reactiveValuesToList(match_vector))
      final_vector <- final_vector[order(names(final_vector))]

      change_table$new_match_status <- final_vector

      if (remove_duplicates) {

        change_groups <- change_table[c("x_id", "y_id", "new_match_status")]

        change_table_b <- merge(result_b, change_groups)
        change_table_b <- change_table_b[c(".UID", "x_name", "y_name",
                                           "new_match_status", "x_id", "y_id")]

        change_table <- rbind(change_table_b,
                              change_table[is.na(change_table$x_id) |
                                             is.na(change_table$y_id),])

        change_table <- change_table[order(change_table$.UID),]
        change_table$x_id <- NULL
        change_table$y_id <- NULL

        result <- result_full[result_full$correlation != 1,]

      }

      change_table <- merge(change_table, result[c(".UID", "match")])

      change_table <- change_table[change_table$new_match_status !=
                                     change_table$match,]

      change_table$.UID <- NULL
      change_table$match <- NULL

      if (requireNamespace("dplyr", quietly = TRUE)) {
        change_table <- dplyr::as_tibble(change_table)
      }

      shiny::stopApp(change_table)

      }
  })

  waiter::waiter_hide()

}

shiny::shinyApp(ui = ui, server = server)
