#### compare_images ############################################################

### Load objects ###############################################################

result <- shiny::getShinyOption("result")
result$.UID <-
  paste0("id-", formatC(seq_along(result$confirmation), width = 2, flag = "0"))

x_dir <- shiny::getShinyOption("x_dir")
y_dir <- shiny::getShinyOption("y_dir")
x_paths <- shiny::getShinyOption("x_paths")
y_paths <- shiny::getShinyOption("y_paths")

shiny::addResourcePath("x", x_dir)
shiny::addResourcePath("y", y_dir)


### UI object ##################################################################

ui <- shiny::fluidPage(

  ## App title -----------------------------------------------------------------

  shiny::titlePanel(paste0("Image comparison for ",
                           prettyNum(nrow(result), big.mark = ","),
                           " matches")),


  ## Filter data frame ---------------------------------------------------------

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
                 prettyNum(nrow(result[result$confirmation == "match",]),
                           big.mark = ","),
                 ")"),
          paste0("Likely match (",
                 prettyNum(nrow(result[result$confirmation == "likely match",]),
                           big.mark = ","),
                 ")"),
          paste0("Possible match (",
                 prettyNum(nrow(result[result$confirmation == "possible match",
                                       ]), big.mark = ","),
                 ")"),
          paste0("No match (",
                 prettyNum(nrow(result[result$confirmation == "no match",]),
                           big.mark = ","),
                 ")")
          ),
        choiceValues = c("all", "match", "likely match", "possible match",
                         "no match"),
        inline = TRUE))
  ),


  ## Save output ---------------------------------------------------------------

  shiny::fluidRow(
    shiny::column(width = 12,
                  shiny::actionButton("save", "Save changes and exit"),
                  align = "center")
  ),

  shiny::fluidRow(shiny::column(width = 12, style = "height:50px")),


  ## Display images ------------------------------------------------------------

  shiny::fluidRow(

    shiny::column(width = 4, shiny::uiOutput("match"), align = "right"),
    shiny::column(width = 4, shiny::htmlOutput("image_1"), align = "right"),
    shiny::column(width = 4, shiny::htmlOutput("image_2"))

  ),


  ## Save output ---------------------------------------------------------------

  shiny::fluidRow(
    shiny::column(width = 12,
                  shiny::actionButton("save_2", "Save changes and exit"),
                  align = "center")
    ),

  shiny::fluidRow(shiny::column(width = 12, style = "height:50px"))

)



### Server object ##############################################################

server <- function(input, output) {

  ## Make change_table, match_vector and active_index --------------------------

  if (requireNamespace("tibble", quietly = TRUE)) {

    change_table <-
      tibble::tibble(
        match_index = seq_along(result$confirmation),
        UID = result$.UID,
        x_name = result$x_name,
        y_name = result$y_name,
        new_match_status = result$confirmation)

  } else change_table <-
      data.frame(
        match_index = seq_along(result$confirmation),
        UID = result$.UID,
        x_name = result$x_name,
        y_name = result$y_name,
        new_match_status = result$confirmation)

  change_table$new_match_status <-
    ifelse(change_table$new_match_status %in% c("match", "likely match"),
           "match", "no match")

  match_vector <-
    do.call(
      shiny::reactiveValues,
      as.list(setNames(change_table$new_match_status, change_table$UID))
      )

  active_index <- shiny::reactive(
    if (input$filter == "all") seq_len(nrow(result)) else
      which(result$confirmation == input$filter)
  )


  ## Produce match status ------------------------------------------------------

  output$match <- shiny::renderUI({

    match_buttons <- lapply(result$.UID[active_index()], function(x) {

      shiny::actionButton(
        paste0("match_", x),
        "Match",
        style = "height:123px",
        class = if (match_vector[[x]] == "match") {
          "btn-lg btn-block btn-success"
        } else "btn-lg btn-block btn-default"
        )

    })

    no_match_buttons <- lapply(result$.UID[active_index()], function(x) {

      shiny::actionButton(
        paste0("no_match_", x),
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

    images <-
      lapply(x_paths[active_index()],
             function(x) shiny::img(src = x, width = "250px", height = "250px"))

    lines <- lapply(x_paths[active_index()], function(x) shiny::hr())
    together <- c(rbind(images, lines))

    do.call(shiny::tagList, together)

  })

  output$image_2 <- shiny::renderUI({

    images <-
      lapply(y_paths[active_index()],
             function(x) shiny::img(src = x, width = "250px", height = "250px"))

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
      change_table$UID <- NULL

      change_table <-
        change_table[change_table$new_match_status != result$confirmation,]

      shiny::stopApp(change_table)

      }
  })

}

shiny::shinyApp(ui = ui, server = server)
