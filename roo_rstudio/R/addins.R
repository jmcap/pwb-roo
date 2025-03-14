#' AI Code Completion Addin
#'
#' Generates code suggestions using Roo Code's AI service
#'
#' @importFrom rstudioapi getActiveDocumentContext insertText
#' @importFrom httr POST content
roo_code_completion <- function() {
  # Get current code context
  context <- rstudioapi::getActiveDocumentContext()
  current_line <- context$selection[[1]]$line
  code_snippet <- paste(context$lines[current_line], collapse = "")
  
  # Prepare API request
  payload <- list(
    code = code_snippet,
    language = "R"
  )
  
  # Make request to Roo Code AI service
  response <- httr::POST(
    url = "https://api.roocode.com/ai/complete",
    body = payload,
    encode = "json",
    add_headers("Authorization" = "Bearer YOUR_API_KEY")
  )
  
  # Handle response
  if (httr::status_code(response) == 200) {
    suggestion <- httr::content(response)$suggestion
    rstudioapi::insertText(suggestion)
  } else {
    message("Error: ", httr::status_message(response))
  }
}

#' AI Refactor Addin
#'
#' Suggests refactoring options based on current code context
#'
#' @importFrom rstudioapi getActiveDocumentContext insertText
#' @importFrom httr POST content
roo_refactor <- function() {
  # Get entire document content
  document_content <- paste(rstudioapi::getActiveDocumentContent(), collapse = "\n")
  
  # Prepare API request
  payload <- list(
    code = document_content,
    language = "R",
    refactor = TRUE
  )
  
  # Make request to Roo Code AI service
  response <- httr::POST(
    url = "https://api.roocode.com/ai/refactor",
    body = payload,
    encode = "json",
    add_headers("Authorization" = "Bearer YOUR_API_KEY")
  )
  
  # Handle response
  if (httr::status_code(response) == 200) {
    suggestions <- httr::content(response)$suggestions
    # Display options to user (simplified implementation)
    selected <- utils::menu(suggestions, title = "Select Refactoring Option:")
    rstudioapi::insertText(suggestions[[selected]])
  } else {
    message("Error: ", httr::status_message(response))
  }
}

#' Shiny Browser Automation Test Addin
#'
#' Executes browser automation tests against Shiny applications using RSelenium
#'
#' @importFrom RSelenium remoteDriver rsDriver
#' @importFrom rstudioapi getActiveDocumentContext
roo_shiny_test <- function() {
  # Initialize RSelenium
  library(RSelenium)
  driver <- rsDriver(browser = "chrome", chromever = "119.0.6045.162") # Specify chromedriver version
  remDr <- driver$client
  
  on.exit({
    remDr$quit()
    invisible(rm(list = "driver", envir = .GlobalEnv))
  })
  
  # Navigate to Shiny app (default port)
  remDr$navigate("http://localhost:3838")
  
  # Example interaction (replace selectors based on app structure)
  tryCatch({
    # Click example button
    btn <- remDr$findElement(using = "css selector", "#actionButton1")
    btn$click()
    
    # Validate output
    output_element <- remDr$findElement(using = "css selector", "#outputPanel")
    result <- output_element$getElementText()
    message("Test Result: ", result)
  }, error = function(e) {
    message("Test Failed:", e$message)
  })
}

#' Mode Switcher Addin
#'
#' Allows switching between Roo Code modes via a menu
#' @importFrom utils menu
roo_switch_mode <- function() {
  modes <- c("Code", "Architect", "Debug", "Orchestrator", "Ask")
  
  # Determine mode storage file
  mode_file <- file.path(Sys.getenv("HOME"), ".roo_mode")
  
  # Function to save mode
  save_mode <- function(mode) {
    writeLines(mode, mode_file)
  }
  
  # Get current mode from file or default
  current_mode <- "code"
  if (file.exists(mode_file)) {
    current_mode <- readLines(mode_file, warn = FALSE)[1]
  }
  
  # Present selection
  selected <- utils::menu(
    modes,
    title = paste("Current Mode: ", current_mode, "\nSelect New Mode:"),
    graphics = FALSE
  )
  
  if (selected > 0) {
    new_mode <- tolower(modes[selected])
    save_mode(new_mode)
    message("Mode set to ", new_mode)
  } else {
    message("Mode change canceled")
  }
}

# Addin registrations
list(
  addins::addin(
    name = "roo_code_completion",
    title = "AI Code Completion",
    description = "Generates code suggestions using Roo Code's AI service",
    command = roo_code_completion
  ),
  addins::addin(
    name = "roo_refactor",
    title = "AI Refactor",
    description = "Suggests refactoring options based on current code context",
    command = roo_refactor
  ),
  addins::addin(
    name = "roo_shiny_test",
    title = "Run Shiny Browser Test",
    description = "Execute browser automation tests against Shiny applications using RSelenium",
    command = roo_shiny_test
  ),
  addins::addin(
    name = "roo_switch_mode",
    title = "Switch Roo Code Mode",
    description = "Switch between available Roo Code modes",
    command = roo_switch_mode
  )
)