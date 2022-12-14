library(shiny)

ui <- fluidPage(
  titlePanel("A Conversion Tool for GC Peak Area Data"),
  h4("From MassHunter Export File to Operational Format"),
  wellPanel(fluidRow(
    column(5, 
           fileInput("infile", 
                     tags$div(
                       paste("MassHunter Export File"), 
                       tags$a("(Example)", href="https://github.com/qianjzhang/DMAC/blob/main/GCPeakArea.csv", target="_blank"), 
                       sep=" "
                     ), 
                     accept = c(".csv"), buttonLabel = "Select a File", placeholder = ".csv format only"),
           fileInput("ref", 
                     tags$div(
                       paste("Reference Data"), 
                       tags$a("(Example)", href="https://github.com/qianjzhang/DMAC/blob/main/transition2pcb.csv", target="_blank"), 
                       sep=" "
                     ), 
                     accept = c(".csv"), buttonLabel = "Select a File", placeholder = ".csv format only"),
           downloadButton("download", "Download Converted File"),
    ),
    column(7, 
           textOutput("output1"),
           textOutput("output2"),
    )
  )),
  fluidRow(
    column(12,
           h6("Iowa Superfund Research Program Data Management and Analysis Core, \uA9 2022")
    )
  )
)


masshunter2output = function(input, output){
  data <- reactive({
    req(input$infile)
    req(input$ref)
    validate(need(tools::file_ext(input$infile$datapath) == "csv", "Please select a .csv mass spectrometry file"))
    validate(need(tools::file_ext(input$ref$datapath) == "csv", "Please select a .csv reference file"))
    
    ref = read.csv(input$ref$datapath)
    raw = read.csv(input$infile$datapath, header=FALSE)[, c(2:7)]          # selected desired columns 
    index = with(raw, which(V2=="Peak " & V3 == "Start"))   # anchors of obs
    index = c(index, nrow(raw))                             # for the last obs
    
    result = NULL
    for (i in 1:(length(index)-1)){
      label = raw$V2[index[i]-1]
      ttt = strsplit(label, split = "\\(")
      ttt = strsplit(ttt[[1]][2], "\\) ")
      Transition = ttt[[1]][1]
      Sample.ID = strsplit(ttt[[1]][2], " ")[[1]][1]  # remove "Smooth" 
      
      value = NULL
      for (j in (index[i]+1):(index[i+1])){
        if(grepl('[1-9]', substr(raw$V2[j], 1, 1)) & grepl('[1-9]', substr(raw$V3[j], 1, 1))) 
          value = rbind(value, raw[j,])
      }
      result = rbind(result, data.frame(Sample.ID, Transition, value, label)) 
    }
    
    names(result) = c("Sample_ID", "MS.Transition", "Peak", "Start", "RT", "End", "Height", "Area", "Label")
    result$Peak = as.numeric(result$Peak)
    result = subset(result, !grepl("TIC", Label, fixed=TRUE) & grepl("Smooth", Label, fixed=TRUE))    # remove "TIC" or !"Smooth"
    result = result[, 1:8]           # remove Label. Shiny does not workl with "next" in a loop
    
    for (k in unique(result$Sample_ID)){
      tmp = subset(result, Sample_ID == k)[, c("MS.Transition", "Peak", "RT", "Area")]
      names(tmp) = c("MS.Transition", "Peak", paste("RT", k, sep="_"), paste("Area", k, sep="_")) 
      ref = merge(ref, tmp, by = c("MS.Transition", "Peak"), all = TRUE)
    }
    ref[,1:3] = ref[, c(3, 1, 2)]
    names(ref)[1:3] = c("PCB Congener", "MS Transition", "Peak")
    
    ref
  })
  
  output$output1 <- renderText({
    paste("# of NAs in Rows:", toString(rowSums(is.na(data()))))
  })
  
  output$output2 <- renderText({
    paste("# of NAs in Cols:", toString(colSums(is.na(data()))))
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste('output-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
}


shinyApp(ui, server = masshunter2output)
