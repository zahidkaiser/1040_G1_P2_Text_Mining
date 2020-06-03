library(shiny)
library(tm)
library(wordcloud)
library(rsconnect)

function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix <- input$selection
        if(!is.null(getTermMatrix)){
          text <- readLines(getTermMatrix$datapath)  
        }
        else
        {
          text <- "A graphic representation of words, typically those used in a 
          document or website, in which the words are arranged artistically in 
          close proximity and the size of each word's type is proportional to the 
          word's frequency or to the size of a numeric variable associated with the 
          word, such as the population associated with the name of a country."
        }
        
        myCorpus = Corpus(VectorSource(text))
        myCorpus = tm_map(myCorpus, content_transformer(tolower))
        myCorpus = tm_map(myCorpus, removePunctuation)
        myCorpus = tm_map(myCorpus, removeNumbers)
        myCorpus = tm_map(myCorpus, removeWords,
                          c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
        
        myDTM = TermDocumentMatrix(myCorpus,
                                   control = list(minWordLength = 0))
        
        m = as.matrix(myDTM)
        
        sort(rowSums(m), decreasing = TRUE)
        })
      })
    })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"),
                  random.order = input$random)
  })
  
  #output$word.list <- renderTable({
   # x <- terms()
    #x <- tolower(x)
    #words <- unlist (strsplit (x, split = "[[:space:]]+|[[:punct:]]+"))
    #Word <- words[words !=""]
    #Word.freq <- as.data.frame(table (Word))
    #Word.sorted <- Word.freq[order(Word.freq$Freq, decreasing = TRUE), ]
    #return(Word.sorted)
  #})
}