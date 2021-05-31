#### compare_images ############################################################

### Prepare data ###############################################################

# Load objects
df <- shiny::getShinyOption("df")
table_n <- shiny::getShinyOption("table_n")
summary_table <- shiny::getShinyOption("summary_table")
x_paths <- shiny::getShinyOption("x_paths")
y_paths <- shiny::getShinyOption("y_paths")
x_dirs <- shiny::getShinyOption("x_dirs")
y_dirs <- shiny::getShinyOption("y_dirs")
batch_size <- shiny::getShinyOption("batch_size")
if (length(x_dirs) > 0) mapply(shiny::addResourcePath, x_paths, x_dirs)
if (length(y_dirs) > 0) mapply(shiny::addResourcePath, y_paths, y_dirs)


### UI object ##################################################################

ui <- shiny::fluidPage(

  # Load shinyjs
  shinyjs::useShinyjs(),

  # App title and summary table disclosure button
  shiny::fluidRow(shiny::column(width = 10, shiny::titlePanel(shiny::strong(
    paste0("matchr image comparison for ", summary_table$value[5],
           " matches")))),
    shiny::column(width = 2, shiny::br(), shiny::br(), shiny::div(
      align = "right", shiny::actionLink(inputId = "hide", label = "...",
      style = "color:white; font-weight:bold; padding: 100px 0;")))),

  # Summary table
  shiny::fluidRow(conditionalPanel(
    condition = "output.hide_status == 1", shiny::br(),
    shiny::column(width = 1),
    shiny::column(width = 5, shiny::tableOutput("summary_2")),
    shiny::column(width = 5, shiny::tableOutput("summary_1")),
    shiny::column(width = 1), align = "center")),
  shiny::fluidRow(style = "height:20px"),
  shiny::fluidRow(style = "height:5px; background-color:#000000"),
                  
  # Subtitle and options
  shiny::fluidRow(
    shiny::column(width = 8, shiny::uiOutput("subtitle")),
    shiny::column(width = 4, shiny::br(), shiny::actionLink(
      inputId = "highlight", label = "Enable highlighting"), shiny::HTML(" | "),
      shiny::actionLink(inputId = "paths", label = "Show file paths"), 
      align = "right"),
    style = "background-color:white; color:black"),
  
  shiny::fluidRow(style = "height:20px; background-color:#FFFFFF"),
  
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
  
  shiny::fluidRow(shiny::hr(), 
                  style = "background-color:#FFFFFF;color:#000000;"),
  
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
    '* {font-family: Futura, Helvetica, Arial, sans-serif !important};')))
  
)


### Server object ##############################################################

server <- function(input, output, session) {
  
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
  
  # Make highlight_vector to track highlighted matches
  highlight_vector <- do.call(
    shiny::reactiveValues, 
    as.list(setNames(rep(FALSE, length(change_table$.UID)), change_table$.UID)))
  
  # Make active_index to subset displayed images
  active_index <- shiny::reactive({
    which(df$match == table_n[page_count() + 1,]$name)[
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

  
  ## Control options -----------------------------------------------------------
  
  shiny::observeEvent(input$highlight, {
    shiny::updateActionLink(
      session, "highlight", label = if (input$highlight %% 2 == 1) 
        "Disable highlighting" else "Enable highlighting")
  })
  shiny::observeEvent(input$paths, {
    shiny::updateActionLink(
      session, "paths", label = 
        if (input$paths %% 2 == 1) "Hide file paths" else "Show file paths")
  })
  
  
  ## Make summary tables -------------------------------------------------------
  
  output$summary_1 <- shiny::renderTable(summary_table[1:4,], colnames = FALSE, 
                                         align = "rl")
  output$summary_2 <- shiny::renderTable(summary_table[6:9,], colnames = FALSE, 
                                         align = "rl")
  
  # Hide compare status
  output$hide_status <- reactive(input$hide %% 2 == 1)
  outputOptions(output, "hide_status", suspendWhenHidden = FALSE)
  
  
  ## Produce match status and highlights ---------------------------------------

  output$match <- shiny::renderUI({
    
    match_buttons <- lapply(df$.UID[active_index()], function(x) {
      shiny::actionButton(paste0("match_", x), "Match", style = if 
                          (input$highlight %% 2 == 1) "height:100px" else 
                            "height:123px",
                          class = if (match_vector[[x]] == "match") {
                            "btn-lg btn-block btn-success"
                            } else "btn-lg btn-block btn-default")})

    no_match_buttons <- lapply(df$.UID[active_index()], function(x) {
      shiny::actionButton(paste0("no_match_", x), "No match", style = if
                          (input$highlight %% 2 == 1) "height:100px" else 
                            "height:122px",
                          class = if (match_vector[[x]] == "match") {
                            "btn-lg btn-block btn-default"
                            } else "btn-lg btn-block btn-danger")})

    lines <- lapply(seq_along(active_index()), function(x) shiny::hr())
    
    if (input$highlight %% 2 == 1) {
      
      open_star <- paste0(
        '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" ',
        'fill="currentColor" class="bi bi-star" viewBox="0 0 16 16"><path ',
        'd="M2.866 14.85c-.078.444.36.791.746.593l4.39-2.256 4.389 ',
        '2.256c.386.198.824-.149.746-.592l-.83-4.73 3.522-3.356c.33-.314.16-.', 
        '888-.282-.95l-4.898-.696L8.465.792a.513.513 0 0 0-.927 0L5.354 ',
        '5.12l-4.898.696c-.441.062-.612.636-.283.95l3.523 3.356-.83 ',
        '4.73zm4.905-2.767-3.686 1.894.694-3.957a.565.565 0 0 ',
        '0-.163-.505L1.71 6.745l4.052-.576a.525.525 0 0 0 .393-.288L8 ',
        '2.223l1.847 3.658a.525.525 0 0 0 .393.288l4.052.575-2.906 ',
        '2.77a.565.565 0 0 0-.163.506l.694 3.957-3.686-1.894a.503.503 0 0 ',
        '0-.461 0z"/></svg>')
      
      closed_star <- paste0(
        '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" ',
        'fill="currentColor" class="bi bi-star-fill" viewBox="0 0 16 16">',
        '<path d="M3.612 15.443c-.386.198-.824-.149-.746-.592l.83-4.73L.173 ',
        '6.765c-.329-.314-.158-.888.283-.95l4.898-.696L7.538.792c.197-.39.73-.',
        '39.927 0l2.184 4.327 4.898.696c.441.062.612.636.282.95l-3.522 ',
        '3.356.83 4.73c.078.443-.36.79-.746.592L8 13.187l-4.389 2.256z"/></svg>'
        )
      
      highlight_buttons <- lapply(df$.UID[active_index()], function(x) {
        shiny::actionButton(paste0("highlight_", x), label = shiny::HTML(
          if (highlight_vector[[x]] == TRUE) closed_star else open_star), 
          style = "height:40px", class = if (highlight_vector[[x]] == TRUE) 
            "btn-lg btn-block btn-warning" else "btn-lg btn-block btn-light")})
      
    }
    
    if (input$paths %% 2 == 1) {
      text <- lapply(active_index(), function(x) shiny::h5(
        paste(x, df$.UID[x], sep = ": "), align = "center"))
      } 
    
    together <- c(rbind(if (input$paths %% 2 == 1) text,
                        match_buttons, no_match_buttons, 
                        if (input$highlight %% 2 == 1) highlight_buttons,
                        lines))
    
    do.call(shiny::tagList, together)
  })
  

  ## Produce images ------------------------------------------------------------

  output$image_1 <- shiny::renderUI({
    images <- lapply(df$x_name[active_index()], function(x) {
      shiny::img(src = x, width = "250px", height = "250px")})
    lines <- lapply(df$x_name[active_index()], function(x) shiny::hr())
    if (input$paths %% 2 == 1) {
      text <- lapply(df$x_name[active_index()], shiny::h5, align = "center")
      together <- c(rbind(text, images, lines))
    } else together <- c(rbind(images, lines))
    do.call(shiny::tagList, together)
  })

  output$image_2 <- shiny::renderUI({
    images <- lapply(df$y_name[active_index()], function(x) {
      shiny::img(src = x, width = "250px", height = "250px")})
    lines <- lapply(df$y_name[active_index()], function(x) shiny::hr())
    if (input$paths %% 2 == 1) {
      text <- lapply(df$y_name[active_index()], shiny::h5, align = "center")
      together <- c(rbind(text, images, lines))
    } else together <- c(rbind(images, lines))
    do.call(shiny::tagList, together)
  })


  ## Get changes ---------------------------------------------------------------

  lapply(df$.UID, function(x) {
    shiny::observeEvent(input[[paste0("match_", x)]], 
                        {match_vector[[x]] <- "match"})
    shiny::observeEvent(input[[paste0("no_match_", x)]],
                        {match_vector[[x]] <- "no match"})
    shiny::observeEvent(input[[paste0("highlight_", x)]],
                        {highlight_vector[[x]] <- 
                          ifelse(highlight_vector[[x]], FALSE, TRUE)})
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
        which(df$match == table_n[n,]$name)[table_n[n,]$i_1:table_n[n,]$i_2]
        })
      confirm_vector <- sort(unlist(confirm_vector))
      change_table <- change_table[confirm_vector,]
      
      # Process highlight_vector
      highlight_vector <- unlist(shiny::reactiveValuesToList(highlight_vector))
      highlight_vector <- highlight_vector[order(names(highlight_vector))]
      
      shiny::stopApp(list(change_table, highlight_vector))

      }
  })
  
}

shiny::shinyApp(ui = ui, server = server)
