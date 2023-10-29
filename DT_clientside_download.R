.render_table <- function(output, data)
{
  download_file_prefix <- paste0("data_", toString(Sys.time()))
  download_file_prefix <- gsub(x = download_file_prefix, pattern = " |-|:", replacement = "_")
  
  output$mtcars_table <- DT::renderDataTable(
    server = FALSE,
    expr = {
      dt_options <- list(
        dom = "lftipB",
        searchHighlight = TRUE,
        scrollX = TRUE,
        search = list(regex = TRUE, caseInsensitive = TRUE, smart = FALSE),
        language = list(searchPlaceholder = "case insensitive search"),
        buttons = list(
          list(extend = "csv", filename = download_file_prefix),
          list(extend = "excel", filename = download_file_prefix)
        )
      )
      
      out <- DT::datatable(
        data = data,
        rownames = FALSE,
        filter = list(position = "top", clear = FALSE),
        selection = list(mode = "single"),
        options = dt_options,
        class = "compact stripe nowrap",
        extensions = "Buttons"
      ) 
      
      out <- DT::formatStyle(
        table = out,
        columns = colnames(data),
        fontSize = "90%"
      )
      
      return(out)
    }
  )
}

ui <- shiny::fluidPage(
  DT::dataTableOutput(outputId = "mtcars_table")
)

server <- function(input, output, session) 
{
  session$userData$data <- mtcars
  
  .render_table(output = output, data = session$userData$data)
}

shinyApp(ui, server)