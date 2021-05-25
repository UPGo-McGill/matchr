#### compare_images ############################################################

### Prepare data ###############################################################

# Load objects
result <- shiny::getShinyOption("result")
x_paths <- shiny::getShinyOption("x_paths")
y_paths <- shiny::getShinyOption("y_paths")
x_dirs <- shiny::getShinyOption("x_dirs")
y_dirs <- shiny::getShinyOption("y_dirs")
remove_duplicates <- shiny::getShinyOption("remove_duplicates")
batch_size <- shiny::getShinyOption("batch_size")
show_names <- shiny::getShinyOption("show_names")
corr_thresh <- shiny::getShinyOption("corr_thresh")
previous <- shiny::getShinyOption("previous")
if (length(x_dirs) > 0) mapply(shiny::addResourcePath, x_paths, x_dirs)
if (length(y_dirs) > 0) mapply(shiny::addResourcePath, y_paths, y_dirs)
prog_check <- nrow(result) > 2000 && remove_duplicates == TRUE


### UI object ##################################################################

ui <- shiny::fluidPage(

  # Load waiter and shinyjs
  waiter::use_waiter(),
  if (prog_check) waiter::use_hostess(),
  shinyjs::useShinyjs(),

  # Loading screen
  
  {if (prog_check) {
    
    waiter::waiter_show_on_load(
      html = shiny::tagList(
        shiny::strong(shiny::h1(
          "matchr image comparison", style =
            "color:#FFFFFF; font-family: Futura, Helvetica, Arial, sans-serif;")),
        shiny::div(style = "height: 130px;"),
        shiny::div(shiny::h4("Identifying `x` duplicates"),
                   style = "height: 20px;"), # 20 with text, 30 without
        shiny::div(style = "height: 45px;"),
        shiny::span(waiter::hostess_loader("dup_1", preset = "circle",
                                           text_color = "white",
                                           class = "label-center",
                                           stroke_color = "white",
                                           center_page = TRUE))), 
      color = "#5A70BA")
    
  } else {
    
    waiter::waiter_show_on_load(
      html = shiny::tagList(
        shiny::strong(shiny::h1(
          "matchr image comparison", style =
            "color:#FFFFFF; font-family: Futura, Helvetica, Arial, sans-serif;")),
        shiny::div(style = "height: 40px;"),
        shiny::div(waiter::spin_3circles(), style = "height: 70px;"),
        shiny::div(style = "height: 95px;")), 
      color = "#5A70BA")
    
  }},
    
  waiter::waiter_hide_on_render("image_2"),
  
  # App title
  shiny::titlePanel(shiny::strong(paste0(
    # TKTK any way to get this working with change_table?
    "matchr image comparison for ", prettyNum(nrow(result), big.mark = ","), 
    # "matchr image comparison for ", prettyNum(nrow(change_table), big.mark = ","), 
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
  
  # Reduce helper
  
  reduce <- function(x, h = "none") {
    iterations <- floor(length(x) / 48)
    
    out <- Reduce(function(a, n) {
      if (h == "x" && n %% iterations == 0) hostess$set(n / iterations + 2)
      if (h == "y" && n %% iterations == 0) hostess$set(n / iterations + 52)
      merge_index <- lapply(a, intersect, x[[n]])
      
      if (sum(lengths(merge_index)) > 0) {
        merge_index <- which(lengths(merge_index) > 0)
        merged <- a[merge_index]
        merged <- unlist(merged)
        merged <- union(merged, x[[n]])
        merged <- list(sort(merged))
        not_merged <- a[-merge_index]
        out <- c(merged, not_merged)
      } else out <- c(a, list(x[[n]]))
    }, seq_along(x), init = list())
    
    stopifnot(length(unique(unlist(out))) == 
                length(unlist(lapply(out, unique))))
    out
  }
  

  ## Load objects --------------------------------------------------------------
  
  if (prog_check) hostess <- waiter::Hostess$new("dup_1")

  # These are in server to come after loading screen
  result$.UID <- paste0("id-", formatC(seq_len(nrow(
    result)), width = floor(log10(nrow(result))) + 1, flag = "0"))
  result_full <- result
  
  # Subset table if previous is TRUE
  if (previous && suppressWarnings(!is.null(result$confirmed))) {
    result <- result[result$confirmed == FALSE,]
    result$confirmed <- NULL
  }
  
  # Remove duplicates
  if (remove_duplicates) {
    
    # Identify x images with correlation ~= 1
    x_sig <- result$x_sig[!duplicated(vctrs::field(result$x_sig, "file"))]
    x_matches <- match_signatures(x_sig)
    
    if (prog_check) hostess$set(1)
    
    x_matches <- identify_matches(x_matches)
    x_matches <- x_matches[x_matches$correlation >= corr_thresh,]
    
    # Group x images together by correlation
    x_matches <- mapply(function(x, y) c(x, y), 
                        vctrs::field(x_matches$x_sig, "file"),
                        vctrs::field(x_matches$y_sig, "file"), SIMPLIFY = FALSE, 
                        USE.NAMES = FALSE)
    
    if (prog_check) hostess$set(2)
    
    # Add duplicates
    dup_x <- table(vctrs::field(result$x_sig, "file"))
    dup_x <- dup_x[dup_x >= 2]
    dup_x <- lapply(names(dup_x), function(x) x)
    x_matches <- c(x_matches, dup_x)
    
    if (prog_check) {
      hostess <- waiter::Hostess$new("dup_2")
      
      waiter::waiter_update(
        html = shiny::tagList(
          shiny::strong(shiny::h1(
            "matchr image comparison", style =
              "color:#FFFFFF; font-family: Futura, Helvetica, Arial, sans-serif;")),
          shiny::div(style = "height: 40px;"),
          shiny::div(style = "height: 70px;"),
          shiny::div(style = "height: 20px;"),
          shiny::div(shiny::h4("Removing `x` duplicates"),
                     style = "height: 20px;"), # 20 with text, 30 without
          shiny::div(style = "height: 45px;"),
          shiny::span(waiter::hostess_loader("dup_2", preset = "circle",
                                             text_color = "white",
                                             class = "label-center",
                                             stroke_color = "white",
                                             center_page = TRUE,
                                             "data-value" = 2,
                                             "data-transition-in" = FALSE))))
      
      # Reduce
      x_matches <- reduce(x_matches, "x")
      
    } else x_matches <- reduce(x_matches)
    
    # Create x table
    if (requireNamespace("dplyr", quietly = TRUE)) {
      x_table <- lapply(seq_along(x_matches), function(n) 
        dplyr::tibble(x_id = n, x_name = x_matches[[n]]))
      x_table <- dplyr::bind_rows(x_table)
    } else {
      x_table <- lapply(seq_along(x_matches), function(n) 
        data.frame(x_id = n, x_name = x_matches[[n]]))
      x_table <- do.call(rbind, x_table)
    }
    x_table <- x_table[!duplicated(x_table),]
    
    # Join IDs to result table
    result$x_name <- vctrs::field(result$x_sig, "file")
    result <- merge(result, x_table, all = TRUE)
    
    if (prog_check) {
      
      hostess <- waiter::Hostess$new("dup_3")
      
      waiter::waiter_update(
        html = shiny::tagList(
          shiny::strong(shiny::h1(
            "matchr image comparison", style =
              "color:#FFFFFF; font-family: Futura, Helvetica, Arial, sans-serif;")),
          shiny::div(style = "height: 40px;"),
          shiny::div(style = "height: 70px;"),
          shiny::div(style = "height: 20px;"),
          shiny::div(shiny::h4("Identifying `y` duplicates"),
                     style = "height: 20px;"), # 20 with text, 30 without
          shiny::div(style = "height: 45px;"),
          shiny::span(waiter::hostess_loader("dup_3", preset = "circle",
                                             text_color = "white",
                                             class = "label-center",
                                             stroke_color = "white",
                                             center_page = TRUE,
                                             "data-value" = 50,
                                             "data-transition-in" = FALSE))))
      
    }
    
    # Identify y images with correlation ~= 1 with counterparts in x_table
    y_sig <- result[!is.na(result$x_id),]$y_sig
    y_sig <- y_sig[!duplicated(vctrs::field(y_sig, "file"))]
    y_matches <- match_signatures(y_sig)
    
    if (prog_check) hostess$set(51)
    
    y_matches <- identify_matches(y_matches)
    y_matches <- y_matches[y_matches$correlation >= corr_thresh,]
    
    # Group y images together by correlation
    y_matches <- mapply(function(x, y) c(x, y), 
                        vctrs::field(y_matches$x_sig, "file"),
                        vctrs::field(y_matches$y_sig, "file"), SIMPLIFY = FALSE, 
                        USE.NAMES = FALSE)
    
    if (prog_check) hostess$set(52)
    
    # Add duplicates
    dup_y <- result[!is.na(result$x_id),]$y_sig
    dup_y <- table(vctrs::field(dup_y, "file"))
    dup_y <- dup_y[dup_y >= 2]
    dup_y <- lapply(names(dup_y), function(x) x)
    y_matches <- c(y_matches, dup_y)
    
    if (prog_check) {
      
      hostess <- waiter::Hostess$new("dup_4")
      
      waiter::waiter_update(
        html = shiny::tagList(
          shiny::strong(shiny::h1(
            "matchr image comparison", style =
              "color:#FFFFFF; font-family: Futura, Helvetica, Arial, sans-serif;")),
          shiny::div(style = "height: 40px;"),
          shiny::div(style = "height: 70px;"),
          shiny::div(style = "height: 20px;"),
          shiny::div(shiny::h4("Removing `y` duplicates"),
                     style = "height: 20px;"), # 20 with text, 30 without
          shiny::div(style = "height: 45px;"),
          shiny::span(waiter::hostess_loader("dup_4", preset = "circle",
                                             text_color = "white",
                                             class = "label-center",
                                             stroke_color = "white",
                                             center_page = TRUE,
                                             "data-value" = 52,
                                             "data-transition-in" = FALSE))))
      
      # Reduce
      y_matches <- reduce(y_matches, "y")
      
    } else y_matches <- reduce(y_matches)
    
    # Create y table
    if (requireNamespace("dplyr", quietly = TRUE)) {
      y_table <- lapply(seq_along(y_matches), function(n) 
        dplyr::tibble(y_id = n, y_name = y_matches[[n]]))
      y_table <- dplyr::bind_rows(y_table)
    } else {
      y_table <- lapply(seq_along(y_matches), function(n) 
        data.frame(y_id = n, y_name = y_matches[[n]]))
      y_table <- do.call(rbind, y_table)
    }
    y_table <- y_table[!duplicated(y_table),]
    
    # Join IDs to result table
    result$y_name <- vctrs::field(result$y_sig, "file")
    result <- merge(result, y_table, all = TRUE)
    
    # Create trimmed result table
    result_b <- result[!is.na(result$x_id) & !is.na(result$y_id),]
    result_b <- result_b[order(result_b$x_id, result_b$y_id, 
                               -1 * result_b$correlation),]
    result <- 
      dplyr::bind_rows(result_b[!duplicated(result_b[c("x_id", "y_id")]),], 
                       result[is.na(result$x_id) | is.na(result$y_id),])
    result <- result[order(result$.UID),]
    if (requireNamespace("dplyr", quietly = TRUE)) {
      result <- dplyr::as_tibble(result)}
    
  } else {
    result$x_name <- vctrs::field(result$x_sig, "file")
    result$y_name <- vctrs::field(result$y_sig, "file")
  }
  
  # Remove results with perfect correlation
  result <- result[result$correlation < corr_thresh,]
  
  # Update paths
  if (length(x_dirs) > 0) result$x_name <- as.vector(
    mapply(sub, x_dirs, x_paths, MoreArgs = list(result$x_name), 
           USE.NAMES = FALSE))
  if (length(y_dirs) > 0) result$y_name <- as.vector(
    mapply(sub, y_dirs, y_paths, MoreArgs = list(result$y_name), 
           USE.NAMES = FALSE))
  
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
    images <- lapply(result$x_name[active_index()], function(x) {
      shiny::img(src = x, width = "250px", height = "250px")})
    lines <- lapply(result$x_name[active_index()], function(x) shiny::hr())
    if (show_names) {
      text <- lapply(result$x_name[active_index()], shiny::h5, align = "center")
      together <- c(rbind(text, images, lines))
    } else together <- c(rbind(images, lines))
    do.call(shiny::tagList, together)
  })

  output$image_2 <- shiny::renderUI({
    images <- lapply(result$y_name[active_index()], function(x) {
      shiny::img(src = x, width = "250px", height = "250px")})
    lines <- lapply(result$y_name[active_index()], function(x) shiny::hr())
    if (show_names) {
      text <- lapply(result$y_name[active_index()], shiny::h5, align = "center")
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
        ".UID", "matrix", "list_index", "x_index", "y_index", "x_sig", "y_sig", 
        "correlation")]
      
      change_table <- merge(result, change_table)
      change_table$.UID <- NULL
      change_table$x_name <- NULL
      change_table$y_name <- NULL
      
      result_corr <- result_full[result_full$correlation >= corr_thresh,]
      result_corr$.UID <- NULL
      result_corr$match <- NULL
      result_corr$new_match_status <- "match"
      
      # Need to change to rbind once there is a method that handles matchr_sig
      change_table <- dplyr::bind_rows(change_table, result_corr)
      change_table <- change_table[order(
        change_table$matrix, change_table$list_index, change_table$x_index, 
        change_table$y_index),]

      if (requireNamespace("dplyr", quietly = TRUE)) {
        change_table <- dplyr::as_tibble(change_table)
      }

      shiny::stopApp(change_table)

      }
  })
  
  waiter::waiter_hide()
  
}

shiny::shinyApp(ui = ui, server = server)
