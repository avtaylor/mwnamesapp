library(shiny)
library(dplyr)
library(stringdist)
library(DT)
library(readr)
library(tidyr)
library(ggplot2)
library(httr)
library(plotly)

ui <- fluidPage(
  titlePanel("Malawi Name-District Analysis"),
  radioButtons("name_type", "Choose a dataset:",
               choices = c("First names" = "first", "Surnames" = "surnames"),
               inline = TRUE),
  # Replace sidebarLayout with just fluidRow or mainPanel content directly
  fluidRow(
    column(
      width = 12,
      verbatimTextOutput("status"),
      tabsetPanel(
        tabPanel("Summary Table", DTOutput("summary_table")),
        tabPanel("Name Index",
                 fluidRow(
                   column(
                     width = 6,
                     h4("Name Index by First Letter"),
                     dataTableOutput("name_index")
                   ),
                   column(
                     width = 6,
                     h4("Names Starting With Selected Letter"),
                     dataTableOutput("names_by_letter")
                   )
                 )
        ),
        tabPanel("Letter Index by District",
                 fluidRow(
                   column(4,
                          uiOutput("district_selector_ui")
                   )
                 ),
                 fluidRow(
                   column(
                     width = 6,
                     h4("Name Count by Letter in Selected District"),
                     dataTableOutput("letter_district_index")
                   ),
                   column(
                     width = 6,
                     h4("Names Starting With Selected Letter in District"),
                     dataTableOutput("names_by_letter_district")
                   )
                 ),
                 tableOutput("clicked_letter_names")
        ),
        tabPanel("Histogram by Letter and District",
        fluidRow(
          column(4,
            uiOutput("hist_district_selector_ui")
          )
        ),
        fluidRow(
          column(6,
            plotlyOutput("letter_district_histogram", height = "500px")
          ),
          column(6,
            h4("Names Starting with Selected Letter"),
            tableOutput("clicked_letter_names")
          )
        )
      ),
        tabPanel("Histogram by Letter",
                 fluidRow(
                   column(4,
                          uiOutput("letter_selector_ui")
                   )
                 ),
                 plotOutput("district_letter_histogram", height = "500px")
        ),
        verbatimTextOutput("debug_columns")
      )
    )
  )
)

server <- function(input, output) {

df_data <- reactive({
  # Parameters for GitHub access
repo <- "avtaylor/malawinames"
#path <- "data/MWI_firstnames.csv"
branch <- "main"
token <- Sys.getenv("GITHUB_PAT")

if (token == "") stop("GITHUB_PAT is not set.")
path <- if (input$name_type == "first") {
    "data/MWI_firstnames.csv"
} else {
    "data/MWI_surnames.csv"
}
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
df <- read.csv(download_url, stringsAsFactors = FALSE,fileEncoding = "UTF-8-BOM")
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


    selected_letter <- reactive({
    df <- df_data()
    name_index <- df %>%
      mutate(first_letter = toupper(substr(name, 1, 1))) %>%
      group_by(first_letter) %>%
      summarise(name_count = n(), .groups = "drop") %>%
      arrange(first_letter)
    
    sel <- input$name_index_rows_selected
    if (length(sel)) {
      name_index$first_letter[sel]
    } else {
      NULL
    }
  })
  
  output$names_by_letter_district <- renderDataTable({
    req(input$selected_district)
    req(selected_letter_district())
    
    df <- df_data() %>%
      select(name, frequency, district_list) %>%
      unnest(district_list) %>%
      mutate(
        district = toupper(trimws(district_list)),
        first_letter = toupper(substr(name, 1, 1))
      ) %>%
      filter(district == toupper(input$selected_district),
             first_letter == selected_letter_district()) %>%
      arrange(name)
    
    datatable(df %>% select(Name = name, Frequency = frequency), options = list(pageLength = 10),
              rownames = FALSE)
  })

  selected_letter_district <- reactive({
    selected <- input$letter_district_index_rows_selected
    if (is.null(selected)) return(NULL)
    
    df <- df_data() %>%
      select(name, district_list) %>%
      unnest(district_list) %>%
      mutate(
        district = toupper(trimws(district_list)),
        first_letter = toupper(substr(name, 1, 1))
      ) %>%
      filter(district == toupper(input$selected_district)) %>%
      group_by(first_letter) %>%
      summarise(name_count = n(), .groups = "drop") %>%
      arrange(first_letter)
    
    df$first_letter[selected]
  })
  
  output$status <- renderPrint({
    df <- df_data()
    paste("Rows:", nrow(df), " | Columns:", paste(names(df), collapse = ", "))
  })
  
  output$summary_table <- renderDT({
    df <- df_data()
    datatable(df %>%
                select(name, frequency, num_districts, district),
              options = list(pageLength = 10))
  })
  
  
  output$name_index <- renderDataTable({
    df <- df_data()
    
    name_index <- df %>%
      mutate(first_letter = toupper(substr(name, 1, 1))) %>%
      group_by(first_letter) %>%
      summarise(name_count = n(), .groups = "drop") %>%
      arrange(first_letter)
    
    datatable(
      name_index,
      colnames = c("Letter", "Number of Names"),
      options = list(pageLength = 26),
      selection = "single"  # Allow single row selection
    )
  })
  
  output$names_by_letter <- renderDataTable({
    req(selected_letter())
    df <- df_data()
    
    filtered <- df %>%
      filter(toupper(substr(name, 1, 1)) == selected_letter()) %>%
      arrange(name)
    
    datatable(filtered %>% select(Name = name, Frequency = frequency),
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  
  output$hist_district_selector_ui <- renderUI({
    df <- df_data()
    
    districts <- df %>%
      select(name, district_list) %>%
      tidyr::unnest(district_list) %>%
      pull(district_list) %>%
      unique() %>%
      sort()
    
    selectInput("hist_selected_district", "Choose District:",
                choices = districts, selected = districts[1])
  })
  
  
  output$names_by_letter_district <- renderDataTable({
    req(input$selected_district)
    req(selected_letter_district())
    
    df <- df_data() %>%
      select(name, frequency, district_list) %>%
      unnest(district_list) %>%
      mutate(
        district = toupper(trimws(district_list)),
        first_letter = toupper(substr(name, 1, 1))
      ) %>%
      filter(district == toupper(input$selected_district),
             first_letter == selected_letter_district()) %>%
      arrange(desc(frequency))
    
    datatable(df %>% select(Name = name, Frequency = frequency), options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  
  output$letter_selector_ui <- renderUI({
    df <- df_data()
    
    letters <- df %>%
      pull(name) %>%
      substr(1, 1) %>%
      toupper() %>%
      unique() %>%
      sort()
    
    selectInput("selected_letter", "Choose Letter:",
                choices = letters, selected = letters[1])
  })
  
  output$district_selector_ui <- renderUI({
    df <- df_data()
    districts <- df %>%
      select(name, district_list) %>%
      tidyr::unnest(district_list) %>%
      pull(district_list) %>%
      unique() %>%
      sort()
    selectInput("selected_district", "Choose District:",
                choices = districts, selected = districts[1])
  })
  
  output$letter_district_index <- renderDataTable({
    req(input$selected_district)
    df <- df_data()
    
    expanded <- df %>%
      select(name, district_list) %>%
      tidyr::unnest(district_list) %>%
      mutate(
        district = toupper(trimws(district_list)),
        first_letter = toupper(substr(name, 1, 1))
      )
    
    filtered <- expanded %>%
      filter(district == toupper(input$selected_district)) %>%
      group_by(first_letter, district) %>%
      summarise(name_count = n(), .groups = "drop") %>%
      arrange(first_letter)
    
    datatable(filtered,
              colnames = c("Letter", "District", "Number of Names"),
              options = list(pageLength = 26))
  })
  
  output$letter_district_histogram <- plotly::renderPlotly({
    req(input$hist_selected_district)
    df <- df_data()
    
    expanded <- df %>%
      select(name, frequency, district_list) %>%
      tidyr::unnest(district_list) %>%
      mutate(
        district = toupper(trimws(district_list)),
        first_letter = toupper(substr(name, 1, 1))
      )
    
    filtered <- expanded %>%
      filter(district == toupper(input$hist_selected_district)) %>%
      count(first_letter, name = "frequency")
    
    p <- ggplot(filtered, aes(x = first_letter, y = frequency)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(
        title = paste("Name Frequencies by First Letter in", input$hist_selected_district),
        x = "First Letter of Name",
        y = "Frequency"
      ) +
      theme_minimal()
    
    plotly::ggplotly(p, source = "letter_hist_district")
  })
  
  output$clicked_letter_names <- renderTable({
    req(input$hist_selected_district)
    
    # Get the click event
    click <- plotly::event_data("plotly_click", source = "letter_hist_district")
    if (is.null(click)) return(NULL)
    
    df <- df_data()
    
    # Prepare data again so we know the correct letter levels
    expanded <- df %>%
      select(name, frequency, district_list) %>%
      tidyr::unnest(district_list) %>%
      mutate(
        district = toupper(trimws(district_list)),
        first_letter = toupper(substr(name, 1, 1))
      )
    
    # Filter by district and compute frequency per letter
    filtered <- expanded %>%
      filter(district == toupper(input$hist_selected_district)) %>%
      count(first_letter, name = "frequency") %>%
      arrange(first_letter)
    
    # Map the x-value (numeric) back to letter label
    x_index <- round(as.numeric(click$x))
    if (x_index < 1 || x_index > nrow(filtered)) {
      return(data.frame(Message = paste("Invalid letter index:", x_index)))
    }
    
    clicked_letter <- filtered$first_letter[x_index]
    
    # Now filter expanded data using the correct clicked letter
    matched_names <- expanded %>%
      filter(
        district == toupper(input$hist_selected_district),
        first_letter == clicked_letter
      ) %>%
      group_by(name) %>%
      summarise(total_frequency = sum(frequency), .groups = "drop") %>%
      arrange(desc(total_frequency)) %>%
      head(20)
    
    if (nrow(matched_names) == 0) {
      return(data.frame(Message = paste("No names starting with", clicked_letter)))
    }
    
    matched_names
  })
  
  
  output$district_letter_histogram <- renderPlot({
    req(input$selected_letter)
    df <- df_data()
    
    expanded <- df %>%
      select(name, district_list) %>%
      tidyr::unnest(district_list) %>%
      mutate(
        district = toupper(trimws(district_list)),
        first_letter = toupper(substr(name, 1, 1))
      )
    
    filtered <- expanded %>%
      filter(first_letter == input$selected_letter) %>%
      count(district)
    
    ggplot(filtered, aes(x = reorder(district, -n), y = n)) +
      geom_bar(stat = "identity", fill = "darkorange") +
      labs(
        title = paste("Count of Names Starting with", input$selected_letter, "per District"),
        x = "District",
        y = "Number of Names"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  output$debug_columns <- renderPrint({
    df <- req(input$file)
    names(read.csv(df$datapath, nrows = 1))
  })
  
}

shinyApp(ui = ui, server = server)
