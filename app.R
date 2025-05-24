library(shiny)
library(dplyr)
library(stringdist)
library(DT)
library(readr)
library(tidyr)
library(ggplot2)
library(httr)

ui <- fluidPage(
  titlePanel("Malawi Name-District Analysis"),
  
  # Replace sidebarLayout with just fluidRow or mainPanel content directly
  fluidRow(
    column(
      width = 12,
      verbatimTextOutput("status"),
      tabsetPanel(
        tabPanel("Summary Table", DTOutput("summary_table")),
        verbatimTextOutput("debug_columns")
      )
    )
  )
)

server <- function(input, output) {

df_data <- reactive({
  # Parameters for GitHub access
repo <- "avtaylor/malawinames"
path <- "data/MWI_firstnames.csv"
branch <- "main"
token <- Sys.getenv("GITHUB_PAT")

if (token == "") stop("GITHUB_PAT is not set.")

# Construct GitHub API URL
api_url <- paste0("https://api.github.com/repos/", repo, "/contents/", path, "?ref=", branch)

# GET request with Authorization header
res <- httr::GET(api_url, httr::add_headers(Authorization = paste("token", token)))

if (httr::status_code(res) != 200) {
  stop("Failed to fetch file from GitHub API. Status code: ", httr::status_code(res))
}

# Extract download URL from API response
download_url <- httr::content(res)$download_url

# Read CSV directly
df <- read.csv(download_url, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
colnames(df) <- tolower(trimws(colnames(df)))
if (!all(c("name", "frequency", "district") %in% colnames(df))) {
    stop("CSV must contain 'name', 'frequency', and 'district' columns.",head(df, 10))
}
  # Clean district list
  df <- df %>%
    mutate(
      frequency = as.integer(frequency),
      district_clean = gsub("[\\{\\}'\"]", "", district),
      district_list_raw = strsplit(district_clean, ",\\s*"),
      district_list = lapply(district_list_raw, function(dlist) {
        cleaned <- gsub("-(EAST|WEST|NORTH|SOUTH|URBAN|RURAL|CITY)$", "",
                        gsub("(EAST|WEST|NORTH|SOUTH|URBAN|RURAL|CITY)$", "", dlist))
        cleaned <- gsub(" ", "", toupper(cleaned))
        cleaned <- gsub("NKHATA-BAY", "NKHATABAY", cleaned, fixed = TRUE)
        cleaned <- gsub("BLANTYRECITY|BLANTYREURBAN|BLANTYRERURAL", "BLANTYRE", cleaned)
        cleaned <- gsub("LILONGWECITY|LILONGWEEAST|LILONGWEWEST", "LILONGWE", cleaned)
        cleaned <- gsub("ZOMBARURAL|ZOMBAURBAN", "ZOMBA", cleaned)
        unique(trimws(cleaned))
      }),
      num_districts = sapply(district_list, length)
    )

  return(df)
})


output$status <- renderPrint({
    df <- df_data()
    paste("Rows:", nrow(df), " | Columns:", paste(nrow(df), collapse = ", "))
  })
  
  output$summary_table <- renderDT({
    df <- df_data()
    print(head(df))
    datatable(df %>%
                select(name, frequency, num_districts, district),
              options = list(pageLength = 10))
  })
   
}

shinyApp(ui = ui, server = server)
