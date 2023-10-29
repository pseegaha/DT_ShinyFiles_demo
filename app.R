#https://r-critique.com/the_snake_and_the_buttons_for_downloading_data_in_R_shiny

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

.observe_save_button <- function(input, session)
{
  shiny::observeEvent(
    eventExpr = input$save,
    handlerExpr = {
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
      
      shinyFiles::shinyFileSave(
        input = input,
        id = "save",
        roots = volumes,
        session = session,
        restrictions = system.file(package = "base")
      )
      
      fileinfo <- shinyFiles::parseSavePath(
        roots = volumes, 
        selection = input$save
      )
      
      if (nrow(fileinfo) > 0) 
      {
        data.table::fwrite(x = session$userData$data, file = fileinfo$datapath)
      }
    }
  )
}

library(shiny)
library(DT)
library(data.table)
library(shinyFiles)

ui <- shiny::fluidPage(
  DT::dataTableOutput(outputId = "mtcars_table"),
  
  shinyFiles::shinySaveButton(
    id = "save",
    label = "save data",
    title = "save data as ...",
    filetype = list(csv = "csv"),
    viewtype = "icon"
  )
  
)

server <- function(input, output, session) 
{
  session$userData$data <- mtcars
  
  .render_table(output = output, data = session$userData$data)
  
  .observe_save_button(input = input, session = session)
}

shinyApp(ui, server)