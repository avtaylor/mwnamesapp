library(shiny)
library(httr)
library(jsonlite)
library(base64enc)

ui <- fluidPage(
  tableOutput("summary_table")
)

server <- function(input, output, session) {
  # Load data from GitHub API once on app start
  repo <- "avtaylor/malawinames"
  path <- "data/MWI_firstnames.csv"
  branch <- "main"
  token <- Sys.getenv("GITHUB_PAT")
  
  api_url <- paste0("https://api.github.com/repos/", repo, "/contents/", path, "?ref=", branch)
  
  res <- httr::GET(api_url, httr::add_headers(Authorization = paste("token", token)))
  stopifnot(httr::status_code(res) == 200)
  
  content_json <- httr::content(res, as = "text")
  content_list <- jsonlite::fromJSON(content_json)
  raw_csv <- base64enc::base64decode(content_list$content)
  csv_text <- rawToChar(raw_csv)
  
  df <- read.csv(text = csv_text, stringsAsFactors = FALSE)
  colnames(df) <- tolower(trimws(colnames(df)))
  
  output$summary_table <- renderTable({
    print("Rendering summary_table")
    head(df)  # just display first few rows
  })
}

shinyApp(ui, server)
