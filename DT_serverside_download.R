.render_table <- function(output, data)
{
  output$mtcars_table <- DT::renderDataTable(
    server = TRUE,
    expr = {
      dt_options <- list(
        dom = "lftip",
        searchHighlight = TRUE,
        scrollX = TRUE,
        search = list(regex = TRUE, caseInsensitive = TRUE, smart = FALSE),
        language = list(searchPlaceholder = "case insensitive search")
      )
      
      out <- DT::datatable(
        data = data,
        rownames = FALSE,
        filter = list(position = "top", clear = FALSE),
        selection = list(mode = "single"),
        options = dt_options,
        class = "compact stripe nowrap"
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

.write_file <- function(file_path, data)
{
  data.table::fwrite(x = data, file = file_path)
}

ui <- shiny::fluidPage(
  DT::dataTableOutput(outputId = "mtcars_table"),
  
  shiny::downloadButton(
    outputId = "download_button", 
    label = "Download"
  )
)

server <- function(input, output, session) 
{
  session$userData$data <- mtcars
  
  .render_table(output = output, data = session$userData$data)
  
  output$download_button <- shiny::downloadHandler(
    filename = paste0("data-", Sys.Date(), ".csv"),
    content = function(file_path) 
    {
      .write_file(file_path = file_path, data = session$userData$data)
    }
  )
}

shinyApp(ui, server)