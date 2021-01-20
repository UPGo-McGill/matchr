#### compare_images ############################################################

### Prepare data ###############################################################

# Load objects
result <- shiny::getShinyOption("result")
result_full <- shiny::getShinyOption("result_full")
result_corr <- shiny::getShinyOption("result_corr")
x_dir <- shiny::getShinyOption("x_dir")
y_dir <- shiny::getShinyOption("y_dir")
remove_duplicates <- shiny::getShinyOption("remove_duplicates")
batch_size <- shiny::getShinyOption("batch_size")
show_names <- shiny::getShinyOption("show_names")
corr_thresh <- shiny::getShinyOption("corr_thresh")
if (remove_duplicates) result_b <- shiny::getShinyOption("result_b")
shiny::addResourcePath("x", x_dir)
shiny::addResourcePath("y", y_dir)


### UI object ##################################################################

ui <- shiny::fluidPage(

  # Load waiter and shinyjs
  waiter::use_waiter(),
  shinyjs::useShinyjs(),
  
  # App title
  shiny::titlePanel(shiny::strong(paste0(
    "matchr image comparison for ", prettyNum(nrow(result), big.mark = ","), 
    " matches"))),
  shiny::fluidRow(style = "height:20px"),
  shiny::fluidRow(style = "height:5px;background-color:#000000"),
                  
  # Subtitle
  shiny::uiOutput("subtitle"),
  shiny::fluidRow(style = "height:20px;background-color:#FFFFFF;"),
  
  # Top menu
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'both'", 
    shiny::column(width = 8, shiny::span(
      shiny::actionButton("prev_t_1", "Previous (likely matches 1-13)"),
      shiny::actionButton("next_t_1", "Next (likely matches 1-13)"))),
    shiny::column(width = 4, shiny::actionButton(
      "save_t_1", "Save changes and exit"), align = "right")),
    style = "background-color:#FFFFFF;color:#000000;"),
  
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'prev'", 
    shiny::column(width = 8, shiny::span(
      shiny::actionButton("prev_t_2", "Previous (likely matches 1-13)"))),
    shiny::column(width = 4, shiny::actionButton(
      "save_t_2", "Save changes and exit"), align = "right")),
    style = "background-color:#FFFFFF;color:#000000;"),
  
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'next'", 
    shiny::column(width = 8, shiny::span(
      shiny::actionButton("next_t_2", "Next (likely matches 1-13)"))),
    shiny::column(width = 4, shiny::actionButton(
      "save_t_3", "Save changes and exit"), align = "right")),
    style = "background-color:#FFFFFF;color:#000000;"),
  
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'none'", 
    shiny::column(width = 12, shiny::actionButton(
      "save_t_4", "Save changes and exit"), align = "right")),
    style = "background-color:#FFFFFF;color:#000000;"),
  
  shiny::fluidRow(shiny::hr(), style = "background-color:#FFFFFF;color:#000000;"),
  
  # Display images
  shiny::fluidRow(
    shiny::column(width = 4, shiny::uiOutput("match"), align = "center"),
    shiny::column(width = 4, shiny::htmlOutput("image_1"), align = "center"),
    shiny::column(width = 4, shiny::htmlOutput("image_2"), align = "center"),
    style = "background-color:#FFFFFF;color:#000000;"),

  # Bottom menu
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'both'", 
    shiny::column(width = 8, shiny::span(
      shiny::actionButton("prev_b_1", "Previous (likely matches 1-13)"),
      shiny::actionButton("next_b_1", "Next (likely matches 1-13)"))),
    shiny::column(width = 4, shiny::actionButton(
      "save_b_1", "Save changes and exit"), align = "right")),
    style = "background-color:#FFFFFF;color:#000000;"),
  
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'prev'", 
    shiny::column(width = 8, shiny::span(
      shiny::actionButton("prev_b_2", "Previous (likely matches 1-13)"))),
    shiny::column(width = 4, shiny::actionButton(
      "save_b_2", "Save changes and exit"), align = "right")),
    style = "background-color:#FFFFFF;color:#000000;"),
  
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'next'", 
    shiny::column(width = 8, shiny::span(
      shiny::actionButton("next_b_2", "Next (likely matches 1-13)"))),
    shiny::column(width = 4, shiny::actionButton(
      "save_b_3", "Save changes and exit"), align = "right")),
    style = "background-color:#FFFFFF;color:#000000;"),
  
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'none'", 
    shiny::column(width = 12, shiny::actionButton(
      "save_b_4", "Save changes and exit"), align = "right")),
    style = "background-color:#FFFFFF;color:#000000;"),
  
  shiny::fluidRow(style = "height:20px;background-color:#FFFFFF;"),
  shiny::fluidRow(style = "height:5px;background-color:#000000"),
  
  # Footer
  shiny::fluidRow(style = "height:20px"),
  shiny::fluidRow(shiny::column(
    width = 12, shiny::em(paste0("matchr ", packageVersion("matchr"))), 
    align = "right")),
  shiny::fluidRow(style = "height:20px"),
  
  # Tags
  style = "background-color:#5A70BA;color:#FFFFFF;",
  shiny::tags$head(shiny::tags$style(HTML(
    '* {font-family: Futura, Helvetica, Arial, sans-serif !important};'))),
  
  # Loading screen
  waiter::waiter_show_on_load(
    html = shiny::tagList(
      shiny::strong(h1("matchr image comparison", style = 
           "color:#FFFFFF; font-family: Futura, Helvetica, Arial, sans-serif;")
           ), waiter::spin_3circles()), color = "#5A70BA")
  
)


### Server object ##############################################################

server <- function(input, output, session) {
  
  ## Load objects --------------------------------------------------------------
  
  # These are in server to come after loading screen
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
  
  # Make change table
  if (remove_duplicates) {
    change_table <- result[c(".UID", "x_name", "y_name", "match", "x_id", 
                             "y_id")]
  } else change_table <- result[c(".UID", "x_name", "y_name", "match")]
  colnames(change_table)[colnames(change_table) == "match"] <- 
    "new_match_status"
  change_table$new_match_status <- ifelse(
    change_table$new_match_status %in% c("match", "likely match"), "match", 
    "no match")
  
  # Need one table_n row per batch_size entries in each match status
  table_n <- table(result$match)
  table_n <- table_n[order(match(names(table_n), c(
    "match", "likely match", "possible match", "no match")))]
  table_n <-
    data.frame(
      name = unlist(mapply(function(x, y) rep(x, ceiling(y / batch_size)), 
                           names(table_n), table_n, USE.NAMES = FALSE)),
      i_1 = unlist(sapply(table_n, function(x) seq_len(ceiling(
        x / batch_size)) * batch_size - (batch_size - 1), USE.NAMES = FALSE)),
      i_2 = unlist(sapply(table_n, function(x) 
        pmin(seq_len(ceiling(x / batch_size)) * batch_size, x), 
        USE.NAMES = FALSE)),
      row.names = NULL)
  names(table_n) <- c("name", "i_1", "i_2")
  
  
  ## Track page status and related state ---------------------------------------
  
  # Track page count
  page_count <- shiny::reactive({
    sum(input$next_t_1, input$next_t_2, input$next_b_1, input$next_b_2) - 
      sum(input$prev_t_1, input$prev_t_2, input$prev_b_1, input$prev_b_2)
  })

  # Track maximum page count to know how many pages were changed/validated
  max_page_count <- shiny::reactiveVal(0)
  observeEvent(page_count(), {
    new_max <- max(max_page_count(), page_count())
    max_page_count(new_max) 
  })

  # Make match_vector to track match status changes
  match_vector <-
    do.call(
      shiny::reactiveValues,
      as.list(setNames(change_table$new_match_status, change_table$.UID))
    )
  
  # Make active_index to subset displayed images
  active_index <- shiny::reactive({
    which(result$match == table_n[page_count() + 1,]$name)[
      table_n[page_count() + 1,]$i_1:table_n[page_count() + 1,]$i_2]})
  
  
  ## Manage menus and UI -------------------------------------------------------
  
  # Control menus
  output$show_menu <- shiny::renderText({
    if (page_count() == 0 && page_count() < nrow(table_n) - 1) {
      "next"
    } else if (page_count() != 0 && page_count() == nrow(table_n) - 1) {
      "prev"
    } else if (page_count() != 0 && page_count() != nrow(table_n) - 1) {
      "both"
    } else "none"})
  
  outputOptions(output, "show_menu", suspendWhenHidden = FALSE)

  # Make subtitle
  output$subtitle <- shiny::renderUI({
    sub_text <- switch(
      table_n[page_count() + 1,]$name,
      "match" = "Matches ",
      "likely match" = "Likely matches ",
      "possible match" = "Possible matches ",
      "no match" = "Non-matches ")
    shiny::fluidRow(shiny::column(width = 12, shiny::h3(paste0(
      sub_text, table_n[page_count() + 1,]$i_1, "-", 
      table_n[page_count() + 1,]$i_2))), 
      style = "background-color:#FFFFFF;color:#000000;")
  })
  
  # Update buttons
  shiny::observeEvent(page_count(), {
    
    prev_text <- switch(
      paste0(table_n[page_count(),]$name, ""),
      "match" = "matches ",
      "likely match" = "likely matches ",
      "possible match" = "possible matches ",
      "no match" = "non-matches ",
      NA_character_)
    
    prev_text <- paste0("Previous (", prev_text, table_n[page_count(),]$i_1, 
                        "-", table_n[page_count(),]$i_2, ")")
    
    next_text <- switch(
      paste0(table_n[page_count() + 2,]$name, ""),
      "match" = "matches ",
      "likely match" = "likely matches ",
      "possible match" = "possible matches ",
      "no match" = "non-matches ",
      NA_character_)
    
    next_text <- paste0("Next (", next_text, table_n[page_count() + 2,]$i_1, 
                        "-", table_n[page_count() + 2,]$i_2, ")")
    
    shiny::updateActionButton(session, "next_t_1", label = next_text)
    shiny::updateActionButton(session, "next_t_2", label = next_text)
    shiny::updateActionButton(session, "next_b_1", label = next_text)
    shiny::updateActionButton(session, "next_b_2", label = next_text)
    shiny::updateActionButton(session, "prev_t_1", label = prev_text)
    shiny::updateActionButton(session, "prev_t_2", label = prev_text)
    shiny::updateActionButton(session, "prev_b_1", label = prev_text)
    shiny::updateActionButton(session, "prev_b_2", label = prev_text)
    
  })
  
  # Track save state
  save_count <- shiny::reactive({
    sum(input$save_t_1, input$save_t_2, input$save_t_3, input$save_t_4,
        input$save_b_1, input$save_b_2, input$save_b_3, input$save_b_4)
    })
  
  # Scroll to top on navigation click
  shiny::observe({
    input$next_b_1
    input$next_b_2
    input$prev_b_1
    input$prev_b_2
    shinyjs::runjs("window.scrollTo(0, 0)")
    })

  
  ## Produce match status ------------------------------------------------------

  output$match <- shiny::renderUI({
    match_buttons <- lapply(result$.UID[active_index()], function(x) {
      shiny::actionButton(paste0("match_", x), "Match", style = "height:123px",
                          class = if (match_vector[[x]] == "match") {
                            "btn-lg btn-block btn-success"
                            } else "btn-lg btn-block btn-default")})

    no_match_buttons <- lapply(result$.UID[active_index()], function(x) {
      shiny::actionButton(paste0("no_match_", x), "No match", 
                          style = "height:122px", 
                          class = if (match_vector[[x]] == "match") {
                            "btn-lg btn-block btn-default"
                            } else "btn-lg btn-block btn-danger")})

    lines <- lapply(seq_along(active_index()), function(x) shiny::hr())
    
    if (show_names) {
      text <- lapply(active_index(), function(x) shiny::h5(
        paste(x, result$.UID[x], sep = ": "), align = "center"))
      together <- c(rbind(text, match_buttons, no_match_buttons, lines))
    } else together <- c(rbind(match_buttons, no_match_buttons, lines))
    
    do.call(shiny::tagList, together)
  })


  ## Produce images ------------------------------------------------------------

  output$image_1 <- shiny::renderUI({
    images <- lapply(x_paths[active_index()], function(x) {
      shiny::img(src = x, width = "250px", height = "250px")})
    lines <- lapply(x_paths[active_index()], function(x) shiny::hr())
    if (show_names) {
      text <- lapply(x_paths[active_index()], shiny::h5, align = "center")
      together <- c(rbind(text, images, lines))
    } else together <- c(rbind(images, lines))
    do.call(shiny::tagList, together)
  })

  output$image_2 <- shiny::renderUI({
    images <- lapply(y_paths[active_index()], function(x) {
      shiny::img(src = x, width = "250px", height = "250px")})
    lines <- lapply(y_paths[active_index()], function(x) shiny::hr())
    if (show_names) {
      text <- lapply(y_paths[active_index()], shiny::h5, align = "center")
      together <- c(rbind(text, images, lines))
    } else together <- c(rbind(images, lines))
    do.call(shiny::tagList, together)
  })


  ## Get changes ---------------------------------------------------------------

  lapply(result$.UID, function(x) {
    shiny::observeEvent(input[[paste0("match_", x)]], 
                        {match_vector[[x]] <- "match"})
    shiny::observeEvent(input[[paste0("no_match_", x)]],
                        {match_vector[[x]] <- "no match"})
  })


  ## Exit function -------------------------------------------------------------

  shiny::observe({
    if (save_count() > 0) {

      # Apply new match status to pairs
      final_vector <- unlist(shiny::reactiveValuesToList(match_vector))
      final_vector <- final_vector[order(names(final_vector))]
      change_table$new_match_status <- final_vector
      
      # Add confirmed vector
      final_page <- max_page_count() + 1
      confirm_vector <- sapply(seq_len(final_page), function(n) {
        which(result$match == table_n[n,]$name)[table_n[n,]$i_1:table_n[n,]$i_2]
        })
      confirm_vector <- sort(unlist(confirm_vector))
      change_table <- change_table[confirm_vector,]
      
      if (remove_duplicates) {

        change_groups <- change_table[c("x_id", "y_id", "new_match_status")]
        change_table_b <- merge(result_b, change_groups)
        change_table_b <- change_table_b[c(".UID", "x_name", "y_name", 
                                           "new_match_status", "x_id", "y_id")]
        change_table <- rbind(change_table_b, change_table[is.na(
          change_table$x_id) | is.na(change_table$y_id),])
        change_table <- change_table[order(change_table$.UID),]
        change_table$x_id <- NULL
        change_table$y_id <- NULL
        
      }
      
      result <- result_full[result_full$correlation < corr_thresh, c(
        ".UID", "matrix", "x_index", "y_index", "x_sig", "y_sig", 
        "correlation")]
      
      change_table <- merge(result, change_table)
      change_table$.UID <- NULL
      change_table$x_name <- NULL
      change_table$y_name <- NULL
      
      result_corr$.UID <- NULL
      result_corr$x_name <- NULL
      result_corr$y_name <- NULL
      result_corr$x_id <- NULL
      result_corr$y_id <- NULL
      result_corr$match <- NULL
      result_corr$new_match_status <- "match"
      
      # Need to change to rbind once there is a method that handles matchr_sig
      change_table <- dplyr::bind_rows(change_table, result_corr)
      change_table <- change_table[order(
        change_table$matrix, change_table$x_index, change_table$y_index),]

      if (requireNamespace("dplyr", quietly = TRUE)) {
        change_table <- dplyr::as_tibble(change_table)
      }

      shiny::stopApp(change_table)

      }
  })
  
  waiter::waiter_hide()
  
}

shiny::shinyApp(ui = ui, server = server)
