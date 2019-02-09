#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(rpart)
library(visNetwork)
library(sparkline)
library(tidyverse)
library(httpuv)
# ============================================================

# ============================================================

RandomHash <- function() {
  v = c(sample(LETTERS, 5, replace = TRUE),
        sample(0:9, 4, replace = TRUE),
        sample(LETTERS, 1, replace = TRUE))
  return(paste0(v,collapse = ""))
}

Csv_file <- function(rand_hash){
  return(paste0("www/", rand_hash, ".csv"))
}

create_survey <- function(qns){
  
  nodes <- qns %>% select(id, label, group, shape, image, size)
  nodes$shadow <- TRUE
  
  ids <- as.numeric(qns$id)
  num_branches <- as.numeric(as.character(sapply(as.character(qns$connect_to), function(x){ length(strsplit(x, ";")[[1]]) })))
  froms <- rep(ids, num_branches)
  tos <- trimws(paste0(gsub(";", " ", qns$connect_to), collapse=" "))
  tos <- as.numeric(strsplit(tos," ")[[1]])
  
  edges <- data.frame(from = froms, to = tos)
  edges$title <- ""
  edges$label <- ""
  edges$length <- 100                           
  head(edges)
  
  # footer <- "Digging deeper to enable us to change Readify for the better"
  visNet <- visNetwork(nodes, edges, width = "100%") %>% 
    #  visNetwork(nodes, edges, width = "100%") %>% 
    visEdges(arrows = "from", smooth = list(enabled = TRUE, type = "cubicBezier")) %>% 
    visHierarchicalLayout() %>%
    visEvents(selectNode = "function(properties) {
        document.activeElement.blur();
        var my_node = this.body.data.nodes.get(properties.nodes[0]);
        var message = {id: my_node.id, label:my_node.label};
        Shiny.onInputChange('nodeInfo', message);
    }")
  
  return (visNet)
}
# ============================================================
# Define UI for application that draws a histogram
ui <- fluidPage(
   shinyjs::useShinyjs(),
   # Application title
   titlePanel("Readify Data Team Satisfaction"),
   h6("Once you are satisfied with your responses, clicking on the Readify logo will save the data against a random hash for this session."),
   h6("(Note: save will only work once all questions have a response, so don't navigate away or reload the page!)"),
   visNetworkOutput("treePlot", height="300px"),
   hr(),
   fluidRow(
     column(6,
            wellPanel(
            htmlOutput("qn_heading"),
            htmlOutput("qn_text"),
            hr(),
              sliderInput("myScore",
                          "In your opinion, how well (1-10) does Readify score on this qn?",
                          min = 0,
                          max = 10,
                          value = 0)
            )
     ),
   column(6,
            wellPanel(
              textAreaInput("the_good", "The GOOD: In a few words, What do you like about it?", value = "", width = "100%",
                            cols = NULL, rows = NULL, placeholder = NULL, resize = "none"),
              textAreaInput("the_bad", "The BAD: What do you dislike about it?", value = "", width = "100%",
                            cols = NULL, rows = NULL, placeholder = NULL, resize = "none")
            )
            
   )
   )
   
   
)
# ============================================================
server <- function(input, output, session) {
  
  values <- reactiveValues()
  values$qns <- read.csv("https://raw.githubusercontent.com/pg-readify/Your_Voice2/master/files/questionaire.csv", stringsAsFactors = F)
  values$qns_orig <- isolate(values$qns)
  values$node_id <- ""
  values$node_label <- ""
  values$visNet <- create_survey(isolate(values$qns))
  

   
   output$treePlot <- renderVisNetwork({
     isolate(values$visNet)
   })
   
   observeEvent(input$nodeInfo, {
     # print(paste0("[",isolate(values$node_id),"]"))
      print(isolate(values$qns))
     if(!(isolate(values$node_id) == "")){
       ii <- which(isolate(values$qns$id) == isolate(values$node_id))
       values$qns$score[ii] <- isolate(input$myScore)
       values$qns$good_comment[ii] <- isolate(input$the_good)
       values$qns$bad_comment[ii] <- isolate(input$the_bad)
       if((isolate(values$qns$good_comment[ii]) != "") && (isolate(values$qns$bad_comment[ii]) != "") && (isolate(values$qns$score[ii]) != 0)){
         values$qns$completed[ii] <- 1
         values$qns$label[ii] <- paste0(isolate(values$qns_orig$label[ii]), "-done")
         values$visNet <- create_survey(isolate(values$qns))
         output$treePlot <- renderVisNetwork({
           isolate(values$visNet)
         })
         
       }
     }
     values$node_id <- isolate(input$nodeInfo$id)
     values$node_label <- isolate(input$nodeInfo$label)
     ii <- which(isolate(values$qns$id) == isolate(values$node_id))
     qn_id    <- as.character(isolate(values$qns$qn_id[ii]))
     qn_text  <- as.character(isolate(values$qns$qn_text[ii]))
     the_good <- as.character(isolate(values$qns$good_comment[ii]))
     the_bad  <- as.character(isolate(values$qns$bad_comment[ii]))
     my_score <- as.numeric(isolate(values$qns$score[ii]))
     # print(paste0("ii: ", ii))
     # print(paste0("my score: ", my_score))
     qn_status <- c("<span style='color:red'>Incomple</span>", "<span style='color:green'>Complete</span>")[isolate(values$qns$completed[ii])+1]
     output$qn_heading <- renderText({HTML(paste0("<b>Qn ",qn_id,":</b> [status: ",qn_status,"]"))})
     output$qn_text <- renderText(qn_text)
     updateTextInput(session,   "the_good", value = the_good)
     updateTextInput(session,   "the_bad",  value = the_bad)
     updateSliderInput(session, "myScore",  value = my_score)
     
     if(isolate(values$node_id) == 1){ # Clicked on the Readify logo
       jj <- which(isolate(values$qns$group) == "Question")
       if(sum(isolate(values$qns$complete[jj])) == length(jj)){
         survey_output <- isolate(values$qns[jj,])
         survey_output <- survey_output %>% select(qn_id, qn_text, score, good_comment, bad_comment)
         rand_hash <- RandomHash()
         survey_output$rand_hash <- rand_hash
         write.csv(survey_output, Csv_file(rand_hash) , row.names = F)
         shinyjs::alert("Thank you for your help in making Readify a better place to work!")
       } else {
         shinyjs::alert("There are some incomplete questions... Please check and try again.")
       }
     }
   })
   

}
# ============================================================
# Run the application 
shinyApp(ui = ui, server = server)
# ============================================================
