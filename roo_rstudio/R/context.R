#' Scan R project files and extract symbols
scan_project <- function() {
  if (!rstudioapi::isAvailable()) {
    message("RStudio not detected - using current working directory")
    project_dir <- getwd()
  } else {
    project_dir <- rstudioapi::getActiveProject()
    if (is.null(project_dir)) {
      message("No active project detected - using current working directory")
      project_dir <- getwd()
    }
  }
  files <- list.files(project_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  symbols <- list()
  for (file in files) {
    symbols[[file]] <- parse_symbols(file)
  }
  return(symbols)
}

#' Parse R file for function definitions and dependencies
parse_symbols <- function(file_path) {
  content <- readLines(file_path)
  symbols <- list()
  calls <- list()
  
  # Extract function definitions
  func_pattern <- "^[[:space:]]*(\\w+)\\s*<-\\s*function\\("
  func_lines <- grep(func_pattern, content, value = TRUE, perl = TRUE)
  for (line in func_lines) {
    name <- sub(func_pattern, "\\1", line, perl = TRUE)
    symbols[[name]] <- list(type = "function", file = file_path)
  }
  
  # Find all function calls in the file
  call_pattern <- "(?<![.[:alnum:]])\\b(\\w+)\\s*\\("
  calls <- unlist(regmatches(content, gregexpr(call_pattern, content, perl = TRUE)))
  calls <- gsub("\\s*\\(", "", calls)
  calls <- unique(calls)
  
  return(list(symbols = symbols, calls = calls))
}

#' Build dependency graph between functions across files
build_dependency_graph <- function(project_symbols) {
  all_functions <- unique(unlist(lapply(project_symbols, function(x) names(x$symbols))))
  
  # Initialize directed graph
  graph <- igraph::graph.empty(directed = TRUE)
  graph <- igraph::add.vertices(graph, V = all_functions)
  
  for (file in names(project_symbols)) {
    file_sym <- project_symbols[[file]]$symbols
    file_calls <- project_symbols[[file]]$calls
    
    # For each function in this file
    for (func_name in names(file_sym)) {
      for (call in file_calls) {
        if (call %in% all_functions && call != func_name) {
          graph <- igraph::add_edges(graph, tail = func_name, head = call)
        }
      }
    }
  }
  
  return(graph)
}