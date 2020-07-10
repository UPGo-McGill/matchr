#### compare_images ############################################################

### Load objects ###############################################################

result <- shiny::getShinyOption("result")

result$.UID <- paste0("id-", formatC(
  seq_len(nrow(result)),
  width = floor(log10(nrow(result))) + 1,
  flag = "0"))

x_dir <- shiny::getShinyOption("x_dir")
y_dir <- shiny::getShinyOption("y_dir")
x_sigs <- shiny::getShinyOption("x_sigs")
y_sigs <- shiny::getShinyOption("y_sigs")
remove_duplicates <- shiny::getShinyOption("remove_duplicates")

shiny::addResourcePath("x", x_dir)
shiny::addResourcePath("y", y_dir)


### Remove duplicates ##########################################################

if (remove_duplicates) {

  ## Back up full result table -------------------------------------------------

  result_full <- result


  ## Helpers -------------------------------------------------------------------

  can_merge <- function(x, y) length(intersect(x, y)) > 0

  merge_fun <- function(x, y) sort(union(x, y))

  reduce_fun <- function(img_list) {

    Reduce(function(acc, curr) {

      curr_vec <- curr[[1]]

      to_merge_id_x <- Position(f = function(x) can_merge(x, curr_vec), acc)

      if (is.na(to_merge_id_x)) acc[[length(acc) + 1]] <- curr_vec else {
        acc[[to_merge_id_x]] <- merge_fun(acc[[to_merge_id_x]], curr_vec)
      }

      return(acc)
    }, Map(list, img_list))
  }

  ## Identify x images with correlation ~= 1 -----------------------------------

  if (is.null(x_sigs)) x_sigs <- identify_image(unique(result$x_name)) else {
    x_sigs <- x_sigs[sapply(x_sigs, attr, "file") %in% result$x_name]
  }

  x_matches <- match_signatures(x_sigs)
  x_matches <- identify_matches(x_matches)
  x_matches <- x_matches[x_matches$correlation >= 0.999,]

  ## Group x images together by correlation ------------------------------------

  x_matches <- mapply(function(x, y) c(x, y), x_matches$x_name,
                      x_matches$y_name, SIMPLIFY = FALSE)

  x_matches <- reduce_fun(x_matches)


  ## Create x table ------------------------------------------------------------

  x_table <- lapply(seq_along(x_matches), function(n) {
    data.frame(x_id = n, x_name = x_matches[[n]])
  })

  x_table <- if (requireNamespace("dplyr", quietly = TRUE)) {
    dplyr::bind_rows(x_table)} else do.call(rbind, x_table)

  x_table <- x_table[!duplicated(x_table),]


  ## Join IDs to result table --------------------------------------------------

  result <- merge(result, x_table, all = TRUE)


  ## Identify y images with correlation ~= 1 with counterparts in x_table ------

  y_candidates <- result[!is.na(result$x_id),]$y_name

  if (is.null(y_sigs)) y_sigs <- identify_image(unique(y_candidates)) else {
    y_sigs <- y_sigs[sapply(y_sigs, attr, "file") %in% y_candidates]
  }

  y_matches <- match_signatures(y_sigs)
  y_matches <- identify_matches(y_matches)
  y_matches <- y_matches[y_matches$correlation >= 0.999,]


  ## Group y images together by correlation ------------------------------------

  y_matches <- mapply(function(x, y) c(x, y), y_matches$x_name,
                      y_matches$y_name, SIMPLIFY = FALSE)

  y_matches <- reduce_fun(y_matches)


  ## Create y table ------------------------------------------------------------

  y_table <- lapply(seq_along(y_matches), function(n) {
    data.frame(y_id = n, x_name = y_matches[[n]])
  })

  y_table <- if (requireNamespace("dplyr", quietly = TRUE)) {
    dplyr::bind_rows(y_table)} else do.call(rbind, y_table)

  y_table <- y_table[!duplicated(y_table),]

  colnames(y_table)[colnames(y_table) == "x_name"] <- "y_name"

  result <- merge(result, y_table, all = TRUE)

  # Create trimmed result table
  result_b <- result[!is.na(result$x_id) & !is.na(result$y_id),]

  result <- rbind(result_b[!duplicated(result_b[c("x_id", "y_id")]),],
                  result[is.na(result$x_id) | is.na(result$y_id),])

  result <- result[order(result$.UID),]

}


### Copy images to temp folders ################################################

file.copy(result$x_name, x_dir)
file.copy(result$y_name, y_dir)


### Make new path vectors ######################################################

x_paths <- strsplit(result$x_name, '/')
x_paths <- sapply(x_paths, function(x) x[[length(x)]])
x_paths <- paste0("x/", x_paths)

y_paths <- strsplit(result$y_name, '/')
y_paths <- sapply(y_paths, function(x) x[[length(x)]])
y_paths <- paste0("y/", y_paths)


### UI object ##################################################################

ui <- shiny::fluidPage(

  ## Load waiter ---------------------------------------------------------------

  waiter::use_waiter(include_js = FALSE),


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

  shiny::fluidRow(shiny::column(width = 12, style = "height:50px")),

  waiter::waiter_show_on_load(
    html = shiny::tagList(if (remove_duplicates) h3("Removing duplicates",
                                                    style = "color:gray"),
                          waiter::spin_3circles()),
    color = "white")

)



### Server object ##############################################################

server <- function(input, output) {

  ## Make change_table, match_vector and active_index --------------------------

  if (remove_duplicates) {
    change_table <- result[c(".UID", "x_name", "y_name", "confirmation",
                             "x_id", "y_id")]
  } else change_table <- result[c(".UID", "x_name", "y_name", "confirmation")]

  colnames(change_table)[colnames(change_table) == "confirmation"] <-
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
      which(result$confirmation == input$filter)
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

        result <- result_full

        }

      change_table$.UID <- NULL

      change_table <-
        change_table[change_table$new_match_status != result$confirmation,]

      if (requireNamespace("dplyr", quietly = TRUE)) {
        change_table <- dplyr::as_tibble(change_table)
      }

      shiny::stopApp(change_table)

      }
  })

  waiter::waiter_hide()

}

shiny::shinyApp(ui = ui, server = server)
