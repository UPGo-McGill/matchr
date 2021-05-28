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
show_paths <- shiny::getShinyOption("show_paths")
if (length(x_dirs) > 0) mapply(shiny::addResourcePath, x_paths, x_dirs)
if (length(y_dirs) > 0) mapply(shiny::addResourcePath, y_paths, y_dirs)


### UI object ##################################################################

ui <- shiny::fluidPage(

  # Load shinyjs
  shinyjs::useShinyjs(),

  # App title
  shiny::titlePanel(shiny::strong(paste0(
    "matchr image comparison for ", summary_table$value[5], " matches"))),
  
  # Summary table disclosure button
  shiny::div(align = "right", 
             shiny::actionLink(inputId = "hide", label = "...", 
                               style = "color:white; font-weight:bold;")),
  
  # Summary table
  shiny::fluidRow(conditionalPanel(
    condition = "output.hide_status == 1", shiny::column(width = 2),
    shiny::column(width = 4, shiny::tableOutput("summary_2")),
    shiny::column(width = 4, shiny::tableOutput("summary_1")),
    shiny::column(width = 2), align = "center")),
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

  
  ## Make summary tables -------------------------------------------------------
  
  output$summary_1 <- shiny::renderTable(summary_table[1:4,], colnames = FALSE, 
                                         align = "rl")
  output$summary_2 <- shiny::renderTable(summary_table[6:9,], colnames = FALSE, 
                                         align = "rl")
  
  # Hide compare status
  output$hide_status <- reactive(input$hide %% 2 == 1)
  outputOptions(output, "hide_status", suspendWhenHidden = FALSE)
  
  
  ## Produce match status ------------------------------------------------------

  output$match <- shiny::renderUI({
    match_buttons <- lapply(df$.UID[active_index()], function(x) {
      shiny::actionButton(paste0("match_", x), "Match", style = "height:123px",
                          class = if (match_vector[[x]] == "match") {
                            "btn-lg btn-block btn-success"
                            } else "btn-lg btn-block btn-default")})

    no_match_buttons <- lapply(df$.UID[active_index()], function(x) {
      shiny::actionButton(paste0("no_match_", x), "No match", 
                          style = "height:122px", 
                          class = if (match_vector[[x]] == "match") {
                            "btn-lg btn-block btn-default"
                            } else "btn-lg btn-block btn-danger")})

    lines <- lapply(seq_along(active_index()), function(x) shiny::hr())
    
    if (show_paths) {
      text <- lapply(active_index(), function(x) shiny::h5(
        paste(x, df$.UID[x], sep = ": "), align = "center"))
      together <- c(rbind(text, match_buttons, no_match_buttons, lines))
    } else together <- c(rbind(match_buttons, no_match_buttons, lines))
    
    do.call(shiny::tagList, together)
  })


  ## Produce images ------------------------------------------------------------

  output$image_1 <- shiny::renderUI({
    images <- lapply(df$x_name[active_index()], function(x) {
      shiny::img(src = x, width = "250px", height = "250px")})
    lines <- lapply(df$x_name[active_index()], function(x) shiny::hr())
    if (show_paths) {
      text <- lapply(df$x_name[active_index()], shiny::h5, align = "center")
      together <- c(rbind(text, images, lines))
    } else together <- c(rbind(images, lines))
    do.call(shiny::tagList, together)
  })

  output$image_2 <- shiny::renderUI({
    images <- lapply(df$y_name[active_index()], function(x) {
      shiny::img(src = x, width = "250px", height = "250px")})
    lines <- lapply(df$y_name[active_index()], function(x) shiny::hr())
    if (show_paths) {
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
      
      shiny::stopApp(change_table)

      }
  })
  
}

shiny::shinyApp(ui = ui, server = server)
