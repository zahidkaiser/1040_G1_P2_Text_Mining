#if (!require("pacman")) install.packages("pacman")
#pacman::p_load_gh("trinker/textstem")
#pacman::p_load(textstem, dplyr)

library(shiny)
library(rsconnect)
#library(mallet) #install package mallet for topic modeling
library(wordcloud)
library(textclean)
library(tm)
library(data.table)
library(stopwords)
library(textstem)
library(dplyr)

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
          text.v <- readLines(getTermMatrix$datapath)  
        }#endif
        else
        {
          text.v <- "A graphic representation of words, typically those used in a 
          document or website, in which the words are arranged artistically in 
          close proximity and the size of each word's type is proportional to the 
          word's frequency or to the size of a numeric variable associated with the 
          word, such as the population associated with the name of a country."
        }#endelse
        
        #convert to lowercase
        text.tolower.v <- tolower(text.v)
        
        # initialize stuff
        
        play.b <- FALSE
        gutenberg.b <- FALSE
        novel.b <- FALSE
        starttext.v <-  grep("chapter 1", text.tolower.v)
        if (is.na(starttext.v[1])) { 
          starttext.v <-  grep("chapter i", text.tolower.v)
          novel.b <- FALSE
        }
        endtext.v <-  length(text.tolower.v)
        
        
        pos = grep("gutenberg", text.tolower.v)
        
        if (!is.na(pos[1])) {
          # A Gutenberg file - we have to strip the standard Gutenberg preface and 
          # end of file so it does not distort our results
          gutenberg.b <- TRUE
        }#endif
        
        if (!is.na(starttext.v[1])) {
          # it is a novel with chapters
          novel.b <- TRUE
        } else {
          # it might be a play
          starttext.v<-NULL
          starttext.v <- grep("act 1", text.tolower.v)
          if (!is.na(starttext.v[1])) {
            # it is a play!
            play.b <- TRUE
          } else {
            #maybe they use Roman numerals
            starttext.v <- grep("act ii", text.tolower.v)
            if (!is.na(starttext.v[1])) {
              # it is a play after all!
              play.b <- TRUE
            }
          }#endif
        }#end else
        
        if (play.b)
        {
          # since it is a play, we will try and extract the list of dramatic personae 
          # names to add them to stoplist as the names of the characters repeat more 
          # often than any other word in the play and our word cloud will consist 
          # mostly of names, which is undesirable
          startpersonalist.v <- grep("dramatis", text.tolower.v)
          
          if (!is.na(startpersonalist.v[1])) { #found the list of dramatic personae
            
            personas.v <- text.tolower.v[(startpersonalist.v[1]+1):(length(text.tolower.v)-(startpersonalist.v[1]+1))]
            
            endpersonalist.v <- grep("act i", personas.v)
            personas.v <- personas.v[1:(endpersonalist.v[1])]
            personas1.v <- personas.v
            j <- 1
            for (val in personas.v) {
              pos_comma<-regexpr(',', personas1.v[j])
              if (pos_comma >=1){
                personas1.v[j] <- substr(personas1.v[j], 1, pos_comma-1)
              }#endif
              pos_space <- regexpr(' ', personas1.v[j])
              if (pos_space >=1){
                personas1.v[j] <- substr(personas1.v[j], 1, pos_space-1)
              }#endif
              j <- j+1
            }#endfor
            not.blanks.v <- which(personas1.v!="")
            personas.v <- personas1.v [not.blanks.v]
          } else {
            # list of dramatic personae not found, do nothing
            message("list of dramatic personae not found, do nothing")
          }
          
          
        }
        
        if (gutenberg.b) {
          #starttext remains as is
          endtext.v <- grep("end of the project gutenberg ebook", text.tolower.v)
        } else{
          #not Gutenberg file, do not strip start or end of file
          starttext.v <- 1
          endtext.v[1] <- (length(text.tolower.v) -1)
        }#endelse
        
        if (is.na(starttext.v[1])) {
          #do not strip the first part of the file
          starttext.v[1] <-1
        }#endif
        
        if (is.na(endtext.v[1])) {
          endtext.v[1] <- (length(text.tolower.v)-1)
        }
        
        
        novel.lines.v <- text.tolower.v[starttext.v[1]+1:endtext.v[1]]
        novel.v <- paste(novel.lines.v, collapse=" ")
        
        book.words.l <- strsplit(novel.v, "\\W")
        book.word.v <- unlist(book.words.l)
        not.blanks.v <- which(book.word.v!="")
        book.word.v <- book.word.v[not.blanks.v]
        book.word.v <- lemmatize_strings(book.word.v)
        words.df <- do.call("rbind", lapply(book.word.v, as.data.frame))
        
        dim(words.df)
        
        
        
        myCorpus <- Corpus(VectorSource(words.df))
        # remove punctuation
        myCorpus <- tm_map(myCorpus, removePunctuation)
        # remove numbers
        myCorpus <- tm_map(myCorpus, removeNumbers)
        
        myCorpus <- tm_map(myCorpus, stripWhitespace)
                           
        
        #############################################
        # stopwords removal code
        ############################################
        myStopwords <- c(stopwords(language="en", source="smart"), "available", "via", "the")
        
        if (play.b){ #add list of personas to stoplist 
          personas2 <- unlist(personas.v)
          myCorpus <- tm_map(myCorpus, removeWords, personas2)
        }
        myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
        #some additional stopwords
        morestopwords.v <- c("jane", "elizabeth", "bennet", "lady", "make", "wickham", 
                             "emma", "darcy", "bingley", "miss",  "mrs", "ye", "can", "thou", 
                             "thy", "thee", "thine", "hath",  "sir", "lear", "kent", 
                             "rom",  "sergius", "kasatsky", "churchill", "hartfield", "elton", "weston", "glou" , "edg", "edm", "harriet", "woodhouse", "fairfax", "knightley", "NA")
        myCorpus <- tm_map(myCorpus, removeWords, morestopwords.v)
        
        #############################################
        
        #############################################
        
        
        book.words.l = strsplit (myCorpus[1]$content, "\\s")
        not.blanks.v <- which(book.words.l!="")
        book.word.v <- unlist(book.words.l)
        not.blanks.v <- which(book.word.v!="")
        book.word.v <- book.word.v[not.blanks.v]
        
        dtm <- TermDocumentMatrix(myCorpus)
        m <- as.matrix(dtm)
        v <- sort(rowSums(m),decreasing=TRUE)
        #d <- data.frame(word = names(v),freq=v)
        
      })#end withProgress
    })#end isolate
  })#end reactive
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"),
                  random.order = input$random)
  })#end renderPlot
}#endfunction