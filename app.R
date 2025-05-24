library(shiny)
library(dplyr)
library(stringdist)
library(DT)
library(readr)
library(tidyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Malawi Name-District Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = ".csv"),
      sliderInput("dist_threshold", "Clustering Threshold", min = 0, max = 1, value = 0.2),
      verbatimTextOutput("status")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", DTOutput("summary_table")),
        tabPanel("Name Index",
                 dataTableOutput("name_index")
        ),
        tabPanel("Letter Index by District",
                 fluidRow(
                   column(4,
                          uiOutput("district_selector_ui")
                   )
                 ),
                 dataTableOutput("letter_district_index")
        ),
        tabPanel("Histogram by Letter and District",
                 fluidRow(
                   column(4,
                          uiOutput("hist_district_selector_ui")
                   )
                 ),
                 plotOutput("letter_district_histogram", height = "500px")
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
  Sys.setenv(GITHUB_PAT = "ghp_xxx...")  # Do this in a secure `.Renviron` file or server environment
  
  df_data <- reactive({
    req(input$file)
    
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    colnames(df) <- tolower(colnames(df))
    
    if (!all(c("name", "frequency", "district") %in% colnames(df))) {
      stop("CSV must contain 'name', 'frequency', and 'district' columns.")
    }
    
    # Clean district list
    df <- df %>%
      mutate(
        frequency = as.integer(frequency),
        district_clean = gsub("[\\{\\}'\"]", "", district),
        district_list_raw = strsplit(district_clean, ",\\s*"),
        district_list = lapply(district_list_raw, function(dlist) {
          cleaned <- tolower(dlist)                              # lowercase for uniformity
          cleaned <- gsub("[-']", " ", cleaned)                  # replace dashes/apostrophes with space
          cleaned <- gsub("(east|west|north|south|urban|rural|city)$", "", cleaned)  # remove suffixes
          cleaned <- trimws(cleaned)                             # trim extra whitespace
          
          # Manually fix known inconsistencies
          cleaned <- gsub("nkhata ?bay", "nkhata bay", cleaned)
          cleaned <- gsub("mzimba.*", "mzimba", cleaned)
          cleaned <- gsub("blantyre.*", "blantyre", cleaned)
          cleaned <- gsub("zomba.*", "zomba", cleaned)
          cleaned <- gsub("lilongwe.*", "lilongwe", cleaned)
          cleaned <- gsub("mchinji.*", "mchinji", cleaned)
          
          unique(cleaned)
        }),
        num_districts = sapply(district_list, length)
      )
    
    return(df)
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
    
    datatable(name_index,
              colnames = c("Letter", "Number of Names"),
              options = list(pageLength = 26))  # one for each letter
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
  output$letter_district_histogram <- renderPlot({
    req(input$hist_selected_district)
    df <- df_data()
    
    expanded <- df %>%
      select(name, district_list) %>%
      tidyr::unnest(district_list) %>%
      mutate(
        district = toupper(trimws(district_list)),
        first_letter = toupper(substr(name, 1, 1))
      )
    
    filtered <- expanded %>%
      filter(district == toupper(input$hist_selected_district)) %>%
      count(first_letter)
    
    ggplot(filtered, aes(x = first_letter, y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(
        title = paste("Name Frequencies by First Letter in", input$hist_selected_district),
        x = "First Letter of Name",
        y = "Frequency"
      ) +
      theme_minimal()
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
