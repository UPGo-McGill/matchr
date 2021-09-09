#### compare_images ############################################################

### Prepare data ###############################################################

# Load objects
x <- shiny::getShinyOption("x_list")
y <- shiny::getShinyOption("y_list")
match <- shiny::getShinyOption("match")
new_match_status <- shiny::getShinyOption("new_match_status")
match_defaults <- shiny::getShinyOption("match_defaults")
batch_size <- shiny::getShinyOption("batch_size")
table_n <- shiny::getShinyOption("table_n")
info <- shiny::getShinyOption("info")
info_vectors <- shiny::getShinyOption("info_vectors")
if (length(x$dir) > 0) mapply(shiny::addResourcePath, x$path, x$dir)
if (length(y$dir) > 0) mapply(shiny::addResourcePath, y$path, y$dir)


### UI object ##################################################################

ui <- shiny::fluidPage(

  # Load shinyjs
  shinyjs::useShinyjs(),

  # App title and summary table disclosure button
  shiny::fluidRow(shiny::column(width = 10, shiny::titlePanel(shiny::strong(
    paste0("matchr image comparison for ", prettyNum(length(x$value), ","), 
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
      shiny::actionLink(inputId = "paths", label = "Show details"), 
      align = "right"),
    style = "background-color:white; color:black"),
  
  shiny::fluidRow(style = "height:20px; background-color:#FFFFFF"),
  
  # Top menu
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'both'", 
    shiny::column(width = 8, shiny::span(
      shiny::actionButton("prev_t_1", "Previous"),
      shiny::actionButton("next_t_1", "Next"))),
    shiny::column(width = 4, shiny::actionButton(
      "save_t_1", "Save changes and exit"), align = "right")),
    style = "background-color:#FFFFFF;color:#000000;"),
  
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'prev'", 
    shiny::column(width = 8, shiny::span(
      shiny::actionButton("prev_t_2", "Previous"))),
    shiny::column(width = 4, shiny::actionButton(
      "save_t_2", "Save changes and exit"), align = "right")),
    style = "background-color:#FFFFFF;color:#000000;"),
  
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'next'", 
    shiny::column(width = 8, shiny::span(
      shiny::actionButton("next_t_2", "Next"))),
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
      shiny::actionButton("prev_b_1", "Previous"),
      shiny::actionButton("next_b_1", "Next"))),
    shiny::column(width = 4, shiny::actionButton(
      "save_b_1", "Save changes and exit"), align = "right")),
    style = "background-color:#FFFFFF;color:#000000;"),
  
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'prev'", 
    shiny::column(width = 8, shiny::span(
      shiny::actionButton("prev_b_2", "Previous"))),
    shiny::column(width = 4, shiny::actionButton(
      "save_b_2", "Save changes and exit"), align = "right")),
    style = "background-color:#FFFFFF;color:#000000;"),
  
  shiny::fluidRow(shiny::conditionalPanel(
    condition = "output.show_menu == 'next'", 
    shiny::column(width = 8, shiny::span(
      shiny::actionButton("next_b_2", "Next"))),
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
  match_vector <- do.call(shiny::reactiveValues, as.list(new_match_status))
  
  # Make highlight_vector to track highlighted matches
  highlight_vector <- do.call(shiny::reactiveValues, as.list(
    setNames(rep(FALSE, length(new_match_status)), names(new_match_status))))
  
  # Make index to subset displayed images
  index <- shiny::reactive({
    which(match == table_n[page_count() + 1,]$name)[
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
    sub_text <- table_n[page_count() + 1,]$name
    shiny::fluidRow(shiny::column(width = 12, shiny::h3(paste0(
      sub_text, " ", table_n[page_count() + 1,]$i_1, "-", 
      table_n[page_count() + 1,]$i_2))), 
      style = "background-color:#FFFFFF;color:#000000;")
  })
  
  # Update buttons
  shiny::observeEvent(page_count(), {
    
    prev_text <- table_n[page_count(),]$name
    prev_text <- paste0("Previous (", prev_text, " ", 
                        table_n[page_count(),]$i_1, "-", 
                        table_n[page_count(),]$i_2, ")")
    next_text <- table_n[page_count() + 2,]$name
    next_text <- paste0("Next (", next_text, " ", 
                        table_n[page_count() + 2,]$i_1, "-", 
                        table_n[page_count() + 2,]$i_2, ")")
    
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
        if (input$paths %% 2 == 1) "Hide details" else "Show details")
  })
  
  
  ## Make summary tables -------------------------------------------------------
  
  output$summary_1 <- shiny::renderTable({
    sum_1 <- as.data.frame(table(match))
    sum_1[order(match(sum_1$match, names(match_defaults))),]
    }, colnames = FALSE, align = "rl")
  output$summary_2 <- shiny::renderTable(info, colnames = FALSE, align = "rl")
  
  # Hide compare status
  output$hide_status <- reactive(input$hide %% 2 == 1)
  outputOptions(output, "hide_status", suspendWhenHidden = FALSE)
  
  
  ## Produce match status and highlights ---------------------------------------

  output$match <- shiny::renderUI({
    
    match_buttons <- lapply(names(new_match_status)[index()], \(x) {
      shiny::actionButton(paste0("match_", x), "Match", style = if 
                          (input$highlight %% 2 == 1) "height:100px" else 
                            "height:123px",
                          class = if (match_vector[[x]] == TRUE) {
                            "btn-lg btn-block btn-success"
                            } else "btn-lg btn-block btn-default")})

    no_match_buttons <- lapply(names(new_match_status)[index()], \(x) {
      shiny::actionButton(paste0("no_match_", x), "No match", style = if
                          (input$highlight %% 2 == 1) "height:100px" else 
                            "height:122px",
                          class = if (match_vector[[x]] == TRUE) {
                            "btn-lg btn-block btn-default"
                            } else "btn-lg btn-block btn-danger")})

    lines <- lapply(seq_along(index()), function(x) shiny::hr())
    
    if (input$highlight %% 2 == 1) {
      highlight_buttons <- 
        lapply(names(new_match_status)[index()], \(x) {
          shiny::actionButton(paste0("highlight_", x), label = shiny::HTML(
            if (highlight_vector[[x]]) closed_star else open_star), 
            style = "height:40px", class = if (highlight_vector[[x]]) 
              "btn-lg btn-block btn-warning" else 
                "btn-lg btn-block btn-light")})
      }
    
    together <- c(rbind(if (input$paths %% 2 == 1) info_vectors[index()],
                        match_buttons, no_match_buttons, 
                        if (input$highlight %% 2 == 1) highlight_buttons,
                        lines))
    
    do.call(shiny::tagList, together)
  })
  

  ## Produce images ------------------------------------------------------------

  output$image_1 <- shiny::renderUI({
    images <- lapply(x$value[index()], \(val) {
      if (x$img) {
        if (is.na(val)) {
          shiny::img(src = "na_image.png", width = "250px", height = "250px")
        } else {
          name <- as.character(runif(1))
          output[[name]] <- shiny::renderPlot(plot(val, path = FALSE))
          shiny::plotOutput(name, width = 250, height = 250)
        }
      } else shiny::img(src = val, width = "250px", height = "250px",
                   onerror = "this.onerror=null;this.src='na_image.png';")})
    lines <- lapply(x$value[index()], \(x) shiny::hr())
    if (input$paths %% 2 == 1) 
      together <- c(rbind(x$name[index()], images, lines)) else 
      together <- c(rbind(images, lines))
    do.call(shiny::tagList, together)
  })

  output$image_2 <- shiny::renderUI({
    images <- lapply(y$value[index()], \(val) {
      if (y$img) {
        if (is.na(val)) {
          shiny::img(src = "na_image.png", width = "250px", height = "250px")
        } else {
          name <- as.character(runif(1))
          output[[name]] <- shiny::renderPlot(plot(val, path = FALSE))
          shiny::plotOutput(name, width = 250, height = 250)
        }
      } else shiny::img(src = val, width = "250px", height = "250px",
                 onerror = "this.onerror=null;this.src='na_image.png';")})
    lines <- lapply(y$value[index()], \(x) shiny::hr())
    if (input$paths %% 2 == 1) 
      together <- c(rbind(y$name[index()], images, lines)) else 
      together <- c(rbind(images, lines))
    do.call(shiny::tagList, together)
  })


  ## Get changes ---------------------------------------------------------------

  lapply(names(new_match_status), function(x) {
    shiny::observeEvent(input[[paste0("match_", x)]], 
                        {match_vector[[x]] <- TRUE})
    shiny::observeEvent(input[[paste0("no_match_", x)]],
                        {match_vector[[x]] <- FALSE})
    shiny::observeEvent(input[[paste0("highlight_", x)]],
                        {highlight_vector[[x]] <- 
                          ifelse(highlight_vector[[x]], FALSE, TRUE)})
  })


  ## Exit function -------------------------------------------------------------

  shiny::observe({
    if (save_count() > 0) {

      # Get match vector
      final_vector <- unlist(shiny::reactiveValuesToList(match_vector))
      final_vector <- final_vector[order(names(final_vector))]

      # Get highlight_vector
      highlight_vector <- unlist(shiny::reactiveValuesToList(highlight_vector))
      highlight_vector <- highlight_vector[order(names(highlight_vector))]
      
      # Remove unconfirmed values
      final_page <- max_page_count() + 1
      confirm_vector <- sapply(seq_len(final_page), function(n) {
        which(match == table_n[n,]$name)[table_n[n,]$i_1:table_n[n,]$i_2]})
      confirm_vector <- sort(unlist(confirm_vector))
      final_vector[-confirm_vector] <- NA
      highlight_vector[-confirm_vector] <- NA
      
      shiny::stopApp(list(final_vector, highlight_vector))

      }
  })
  
}

shiny::shinyApp(ui = ui, server = server)
