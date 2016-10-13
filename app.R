# setwd("D:/programming/work")
# app.R is in folder "projectLibrary"
# runApp("projectLibrary", launch.browser = T)

library(shiny)
library(DT)
library(rdrop2)
#library(dplyr)
library(shinyjs)

# Define the fields we want to save from the form
#fields <- c("Project Name", "Project Type", "Project Description")

outputDir <- "tfoApps"

# global password
logged <- FALSE
masterPass <- "tfo$1"

# Get table metadata. For now, just the fields
# Further development: also define field types
# and create inputs generically
getTableMetadata <- function() {
  fields <- c(id = "Id",
              projPriority = "Priority",
              modDate = "Last Modified",
              projName = "Project Name",
              projType = "Project Type",
              perWork = "Working On",
              projLink = "Project Link",
              projDesc = "Project Description")
  
  result <- list(fields = fields)
  return (result)
}

# Find the next ID of a new record
# (in mysql, this could be done by an incremental index)
getNextId <- function() {
#  if (exists("responses") && nrow(responses) > 0) {
  if (exists("responses")) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return(1)
  }
}

createData <- function(data) {
  data <- castData(data)
  rownames(data) <- getNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, data)
    saveData(data)
  } else {
    responses <<- data
    saveData(data)
  }
}

readData <- function() {
#  if (exists("responses")) {
#    responses 
    allResponses <- loadData()
#    for(i in 1:length(allResponses[[1]])) {
#      allResponses[[1]]$modDate[i] <- as.character(as.Date(allResponses[[1]]$modDate[i], origin = "1970-01-01", format = "%Y-%m-%d"))
#      allResponses[[1]]$modDate[i] <- i
#    }
    responses <<- allResponses[[1]]
#  }
}

updateData <- function(data) {
  data <- castData(data)
  responses[row.names(responses) == row.names(data), ] <<- data
  
  allResponses <- loadData()
  changeFileName <- as.character(allResponses[[2]][as.numeric(row.names(data)), 2])
  filePath <- file.path(tempdir(), changeFileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = outputDir)
}

deleteData <- function(data) {
#  responses <<- responses[row.names(responses) != unname(data["id"]), ]
  
  data <- castData(data)
  
  allResponses <- loadData()
  delFileName <- as.character(allResponses[[2]][as.numeric(row.names(data)), 2])
  delPath <- paste0("/", outputDir, "/", delFileName)
  drop_delete(delPath)
}

# Functions to raise or lower priority
raisePriData <- function(data) {
  data[2] <- as.numeric(data[2]) + 1
  data <- castData(data)
  responses[row.names(responses) == row.names(data), ] <<- data
  
  allResponses <- loadData()
  changeFileName <- as.character(allResponses[[2]][as.numeric(row.names(data)), 2])
  filePath <- file.path(tempdir(), changeFileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = outputDir)
}

lowerPriData <- function(data) {
  data[2] <- as.numeric(data[2]) - 1
  data <- castData(data)
  responses[row.names(responses) == row.names(data), ] <<- data
  
  allResponses <- loadData()
  changeFileName <- as.character(allResponses[[2]][as.numeric(row.names(data)), 2])
  filePath <- file.path(tempdir(), changeFileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = outputDir)
}

# Cast from Inputs to a one-row data.frame
castData <- function(data) {
  datar <- data.frame(projPriority = data["projPriority"],
                      modDate = data["modDate"],
                      projName = data["projName"],
                      projType = data["projType"],
                      perWork = data["perWork"],
                      projLink = data["projLink"],
                      projDesc = data["projDesc"],
                      stringsAsFactors = FALSE)
  # datar <- data.frame(id = data["id"],
  #                     name = data["name"],
  #                     projType = data["projType"],
  #                     projDesc = data["projDesc"],
  #                     stringsAsFactors = FALSE)
  rownames(datar) <- data["id"]
  return(datar)
}

# Return an empty, new record
createDefaultRecord <- function() {
#  myDefault <- castData(list(id = "0", projPriority = "0", modDate = format(Sys.time(), "%Y-%m-%d"), projName = "",
  myDefault <- castData(list(id = "0", projPriority = "0", modDate = Sys.Date(), projName = "",
                             projType = "Productivity", perWork = "", projLink = "", projDesc = ""))
  return(myDefault)
}

# Fill the input fields with the values of the selected record in the table
updateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "projPriority", value = unname(data["projPriority"]))
#  updateDateInput(session, "modDate", value = format(Sys.time(), "%Y-%m-%d"))
  updateDateInput(session, "modDate", value = Sys.Date())
  updateTextInput(session, "projName", value = unname(data["projName"]))
  updateTextInput(session, "projType", value = unname(data["projType"]))
#                    choices = c("Productivity", "Profit", "Urgent", "Miscellaneous"))
  updateTextInput(session, "perWork", value = unname(data["perWork"]))
#                    choices = c("Andy", "Victor", "Nick", "Amit", "Colby"))
  updateTextInput(session, "projLink", value = unname(data["projLink"]))
  updateTextAreaInput(session, "projDesc", value = unname(data["projDesc"]))
}




ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  
  DT::dataTableOutput("responses", width = "90%"),
  actionButton("raisePri", "Raise Priority"),
  actionButton("lowerPri", "Lower Priority"),
# Update Button
#  actionButton("change", "Change Selected Value"),
# Back to working code
  tags$hr(),
  shinyjs::disabled(textInput("id", "Id", "0")),
  shinyjs::disabled(textInput("projPriority", "Priority", 0)),
  dateInput("modDate", "Last Modified", value = NULL),
  textInput("projName", "Project Name", ""),
  selectInput("projType", "Project Type", choices = c("Productivity", "Profit", "Urgent", "Miscellaneous")),
  selectInput("perWork", "Working On", choices = c("", "Nobody", "Andy", "Victor", "Nick", "Amit", "Colby")),
  textInput("projLink", "Project Link", ""),
  h3("Describe the Project"),
  tags$textarea(id = "projDesc", rows = 5, cols = 150),
  h3(""),
  actionButton("submit", "Save Entry"),
  actionButton("new", "Clear Entry"),
  actionButton("delete", "Delete Record"),

# Password Protect--Log In
  tags$hr(),
  passwordInput("userPass", "Enter your password here:"),
  actionButton("login", "Login")
  
  
#  rHandsontableOutput("responses", width = "85%"),
#        actionButton("save", "Save Changes"),
#        tags$hr(),
#        textInput("Project Name", "Name of Project", ""),
#        selectInput("Project Type", "Type of Project", choices = c("Productivity", "Profit")),
#        h3("Describe the Project"),
#        tags$textarea(id = "Project Description", rows = 5, cols = 150),
#        h3(""),
#        actionButton("submit", "Submit")
)

server <- function(input, output, session) {
  # Whenever a field is filled, aggregate all form data
  # formData <- reactive({
  #   data <- sapply(fields, function(x) input[[x]])
  #   data
  # })
  
  formData <- reactive({
    data <- sapply(names(getTableMetadata()$fields), function(x) input[[x]])
    #Add hyperlink formatting
    if(nchar(data[7]) > 1 && length(grep("href", data[7])) == 0) {
      data[7] <- paste0("<a href='https://", data[7], "' target = '_blank'>", data[7], "</a>")
    }
    data
  })
  
  # password login info
  userFlag <- reactiveValues(logged = FALSE)
  
  observeEvent(input$login, {
    if (input$userPass == masterPass) {
      userFlag$logged <- TRUE
    } else {
      userFlag$logged <- FALSE
    }
  })
  
#  newDat <- loadData()
#  values <- reactiveValues(dfWorking = newDat[[1]])
  
  # When the Submit button is clicked, save the form data
  # observeEvent(input$submit, {
  #   saveData(formData())
  #   newDat <- loadData()
  #   values <- reactiveValues(dfWorking = newDat[[1]])
  # })
  
# Click "Submit" button -> save data
# Add password features
  observeEvent(input$submit, {
    if (input$id != "0") {
      updateData(formData())
    } else {
      if (userFlag$logged == TRUE) {
        createData(formData())
      }
      updateInputs(createDefaultRecord(), session)
    }
  }, priority = 1)
  
  #   saveData(formData())
  #   newDat <- loadData()
  #   values <- reactiveValues(dfWorking = newDat[[1]])
#   # })
#   
#   # Show the previous responses
#   # (update with current response when Submit is clicked)
# #  output$responses <- DT::renderDataTable({
# #    input$submit
# #    dataInfo <- loadData()
# #    dataInfo[[1]]
# #  })
# 
# # New code
# #  dataInfo <- loadData()
# #  values <- reactiveValues(dfWorking = newDat[[1]])
#   
#   observeEvent(input$change, {
#     if (!is.null(input$responses_rows_selected)) {
#       values$dfWorking <- values$dfWorking[-as.numeric(input$responses_rows_selected),]
#       delFilePath <- as.character(allDataInfo[[2]][as.numeric(input$responses_rows_selected), 2])
#       drop_delete(delFilePath)
#     }
#   })
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    updateInputs(createDefaultRecord(), session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    deleteData(formData())
    updateInputs(createDefaultRecord(), session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- readData()[input$responses_rows_selected, ]
      updateInputs(data, session)
    }
  })
  
  # Section for raising/lowering priority of projects.
  observeEvent(input$raisePri, {
    if (input$id != "0") {
      raisePriData(formData())
    } else {
      updateInputs(createDefaultRecord(), session)
    }
  }, priority = 1)
  
  observeEvent(input$lowerPri, {
    if (input$id != "0") {
      lowerPriData(formData())
    } else {
      updateInputs(createDefaultRecord(), session)
    }
  }, priority = 1)
  

# Only display table if password is correct
  output$responses <- DT::renderDataTable({
    if (userFlag$logged == TRUE) {
      input$submit
      input$delete
      input$lowerPri
      input$raisePri
      DT::datatable(readData(), options = list(order = list(list(1, 'desc'), list(2, 'desc')),
                                               columnDefs = list(list(visible = FALSE, targets = 2))),
                    colnames = unname(getTableMetadata()$fields)[-1],
                    escape = c(T,T,T,T,T,T,F,T))
  #    datatable(readData()) %>% formatDate('modDate', 'toLocaleDateString')
    }
  }, server = FALSE, selection = "single",
  colnames = unname(getTableMetadata()$fields)[-1]
  )
  
  # output$responses <- DT::renderDataTable({
  #   values$dfWorking
  #   input$submit
  #   input$change
  #   dataInfo <- loadData()
  #   dataInfo[[1]]
  # })
  #   
#  values = reactiveValues()
  
#  newData = reactive({
#    if (!is.null(input$responses)) {
#      DF = hot_to_r(input$responses)
#    } else {
#      if (is.null(values[["DF"]]))
#        DF = loadData()
#      else
#        DF = values[["DF"]]
#    }

#    values[["DF"]] = DF
#    DF
#  })
  
#  observeEvent(input$save, {
#    saveData(newData())
#  })
  
#  output$responses <- renderRHandsontable({
#    DF = newData()
#    if (!is.null(DF))
#      rhandsontable(DF, stretchH = "all")
#  })
  
#  output$responses <- renderRHandsontable({
#    input$submit
#    DF = loadData()
#    if (!is.null(DF))
#      rhandsontable(DF, width = "85%")
#  })

}

saveData <- function(data) {
#  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = outputDir)
}

loadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path
  # test code
  if(!is.null(filePaths)) {
    dropFileSplit <- strsplit(filePaths, "/")
    dropFileCompVec <- unlist(dropFileSplit)
    dropFileNames <- dropFileCompVec[grep("csv", dropFileCompVec)]
    dfNames <- data.frame(seq(1:length(filePaths)), dropFileNames)
  } else {
    dfNames <- NULL
  }
  # end test code
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  dataInfo <- list(data, dfNames)
  dataInfo
}

shinyApp(ui = ui, server = server)