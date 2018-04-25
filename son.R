library(rvest)
library(omdbapi)
library(qdap)
library(tm)  # The tm package let's us create a Corpus
library(wordcloud)
library(RColorBrewer)
library(rtweet)
library(tm)  # The tm package let's us create a Corpus
library(SentimentAnalysis)
library(RWeka)


url <- "https://en.wikipedia.org/wiki/World_population"

ten_most_populous <- read_html(url)

str(ten_most_populous)

ten_most_populous %>% 
   html_nodes(xpath='//*[@id="mw-content-text"]/div/table[5]') %>% 
   html_table() -> ten_most_df



(ten_most_df[[1]] %>% select(2:4) -> ten_most_df)

names(ten_most_df)



names(ten_most_df) <- c("Country_Territory","Population",
                        "Date")
ten_most_df[1:3,]

ten_most_df %>% mutate(Population=gsub(",","",Population)) %>%  
   mutate(Population=round(as.numeric(Population)/1e+06))  %>%
   ggplot(aes(x=Country_Territory,y=Population)) + geom_point() + 
   labs(y = "Population / 1,000,000") + coord_flip() + 
   ggtitle("Top 10 Most Populous Countries")


scraping_wiki <- read_html("https://en.wikipedia.org/wiki/Web_scraping")

scraping_wiki %>%   html_nodes("h1")

scraping_wiki %>% html_nodes("h1") %>% html_text()


scraping_wiki %>%
         html_nodes("h2") %>%
         html_text()
         
# Store web url

lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

# Scrape the website for the movie rating

rating <- lego_movie %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
  
rating

lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

# Scrape the website for the Story Line

storyline <- lego_movie %>% 
         html_nodes(css="#titleStoryLine p") %>% 
         html_text() %>% gsub("\n","",.) %>% gsub('"',"",.)
         
storyline

url <- "https://coinmarketcap.com/all/views/all/"

bc <- url %>% read_html()

path <- '//*[@id="currencies-all"]'

bc_table <- bc %>% html_nodes(xpath=path) %>% html_table()

# We get back a one element list that is a data frame

str(bc_table,0)

bc_table <- bc_table[[1]]

head(bc_table[,3:5])


# The data is "dirty" and has characers in it that need cleaning

bc_table <- bc_table %>% select(Name,Symbol,Price)
bc_table <- bc_table %>% mutate(Name=gsub("\n"," ",Name)) 
bc_table <- bc_table %>% mutate(Name=gsub("\\.+","",Name))
bc_table <- bc_table %>% mutate(Price=gsub("\\$","",Price))
bc_table <- bc_table %>% mutate(Price=round(as.numeric(Price),2))

# There are four rows wherein the Price is missing NA

bc_table <- bc_table %>% filter(complete.cases(bc_table))

# Let's get the Crypto currencies with the Top 10 highest prices

top_10 <- bc_table %>% arrange(desc(Price)) %>% head(10)


# Next we want to make a barplot of the Top 10

ylim=c(0,max(top_10$Price)+10000)
main="Top 10 Crypto Currencies in Terms of Price"
bp <- barplot(top_10$Price,col="aquamarine",
              ylim=ylim,main=main)
axis(1, at=bp, labels=top_10$Symbol,  cex.axis = 0.8)
grid()

# Let's take the log of the price

ylim=c(0,max(log(top_10$Price))+5)
main="Top 10 Crypto Currencies in Terms of log(Price)"
bp <- barplot(log(top_10$Price),col="aquamarine",
              ylim=ylim,main=main)
axis(1, at=bp, labels=top_10$Symbol,  cex.axis = 0.8)
grid()


str(movie)

movie$Plot
     
# Get the ratings from the three services

sapply(movie$Ratings,unlist)
url <- "http://www.omdbapi.com/?apikey=f7c004c&t=Game%20of%20Thrones&Season=1"
movie <- fromJSON(url)
str(movie,1)

episodes <- data.frame(do.call(rbind,movie$Episodes),stringsAsFactors = FALSE)

library(omdbapi)

# The first time you use this you will be prompted to enter your
# API key

search_by_title("Star Wars", page = 2)

(gf <- find_by_title("The GodFather"))

get_actors((gf))

url <- "https://millercenter.org/the-presidency/presidential-speeches/march-4-1865-second-inaugural-address"
lincoln_doc <- read_html(url) %>% 
                    html_nodes(".view-transcript") %>% 
                    html_text()
                    
lincoln_doc

lincoln_doc <- read_html(url) %>% 
                    html_nodes(".view-transcript") %>% 
                    html_text()
                    
word_vec <- unlist(strsplit(lincoln_doc," "))
sort(table(word_vec),decreasing = TRUE)[1:10]

# Remove all punctuation marks
word_vec <- gsub("[[:punct:]]","",word_vec)
stop_words <- c("the","to","and","of","the","for","in","it",
                "a","this","which","by","is","an","hqs","from",
                "that","with","as")

for (ii in 1:length(stop_words)) {
    for (jj in 1:length(word_vec)) {
      if (stop_words[ii] == word_vec[jj]) {
          word_vec[jj] <- ""
      }
  }
}

word_vec <- word_vec[word_vec != ""]
sort(table(word_vec),decreasing = TRUE)[1:10]
word_vec
  war   all    be    we   but   God shall   was    do   let 
   11     8     8     6     5     5     5     5     4     4 

# Find the URL for Lincoln's March 4, 1865 Speech:

url <- "https://millercenter.org/the-presidency/presidential-speeches/march-4-1865-second-inaugural-address"
library(rvest)
lincoln_doc <- read_html(url) %>% 
                    html_nodes(".view-transcript") %>% 
                    html_text()
                    
(frequent_terms <- freq_terms(lincoln_doc,10))


# Not very helpful because there are lots of "filler" words. These are also known as 
# "stop words".

library(qdap)
# There is a function that can remove the sotp words for us
# cleaned_up_terms <- rm_stopwords(lincoln_doc,stopwords=Top100Words)

freq_terms(cleaned_up_terms,10)
   



# Language is imperfect and especially so with written stuff:
stopwords()[1:20]
 
# Cleanup is ususally done right after reading in a Corpus.


# Here is a function that contains most of the important cleaning functions
# including stop word removal:

tm_cleaner <- function(corpus, stop=stopwords("en"), rm_num=TRUE) {
  require(tm)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stop)
   corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("http\\w+", "", x)))
  return(corpus)
}

st <- "They do such a great job preserving our freedom.  It 
was Malia's birthday, on the Fourth of July, and she's now 14."

clean_st <- tm_cleaner(VCorpus(VectorSource(st)))
clean_st[[1]][1]

st <- "They do such a great job preserving our freedom.  It 
was Malia's birthday, on the Fourth of July, and she's now 14."

mystop <- c("now",stopwords("en"))

clean_st <- tm_cleaner(VCorpus(VectorSource(st)),stop=mystop)

clean_st[[1]][1]

# Let's go back to the \texttt{lincoln\_doc}
library(tm)  # The tm package let's us create a Corpus

lincoln_vec_source <- VectorSource(lincoln_doc)
lincoln_vcorpus <- VCorpus(lincoln_vec_source)

str(lincoln_vcorpus)

cleaned_lincoln_corp <- tm_cleaner(lincoln_vcorpus)


# Next create a tdm

lincoln_tdm <- TermDocumentMatrix(cleaned_lincoln_corp)
lincoln_tdm_m <- as.matrix(lincoln_tdm)

# Get the term frequencies and sort them

terms_freq <- rowSums(lincoln_tdm_m)
terms_freq_sorted <- sort(terms_freq, decreasing=TRUE)
terms_freq_sorted[1:10]

barplot(terms_freq_sorted[1:10],col="aquamarine",las=2,
        main="Ten Highest Word Frequencies Lincoln Speech")

# Create a Wordcloud with Colors

library(wordcloud)
library(RColorBrewer)

df <- data.frame(word=names(terms_freq_sorted),
                 freq=terms_freq_sorted,
                 stringsAsFactors = FALSE)

color_palette <- brewer.pal(5,"Dark2")
wordcloud(df$word,df$freq,max.words = 10,colors = color_palette)


# Multiple Speeches Are Possible
url <- "https://raw.githubusercontent.com/pittardsp/info550_spring_2018/master/SUPPORT/obama.zip"
download.file(url,"obama.zip")
system("unzip obama.zip")
source <- DirSource("OBAMA/")
obama_corp <- Corpus(source, readerControl=list(reader=readPlain))

# We have 53 Speeches from Obama

obama_corp[[2]][1]
   
cleaned_obama_corp <- tm_cleaner(obama_corp)

obama_tdm <- TermDocumentMatrix(cleaned_obama_corp)
obama_tdm_m <- as.matrix(obama_tdm)

# Get the term frequencies and sort them

terms_freq <- rowSums(obama_tdm_m)
terms_freq_sorted <- sort(terms_freq, decreasing=TRUE)

df <- data.frame(word=names(terms_freq_sorted),
                 freq=terms_freq_sorted,
                 stringsAsFactors = FALSE)

color_palette <- brewer.pal(5,"Dark2")
wordcloud(df$word,df$freq,max.words = 100,colors = color_palette)

# rtweet

appname <- "rtweetpit"
key <- "your_consumer_key_here"
secret <- "your_secret_consumer_key_here"

# Get authenticated

library(rtweet)

twitter_token <- create_token(
  app = "appname"rtweetpit,
  consumer_key = key,
  consumer_secret = secret)

# Running this code will contact Twitter and load a web page. 
# If authentication is successful then you will see a web page 
# with the contents of


# Create a Function to Pull in Some Tweets

my_search_tweets <- function(string="Katy Perry",n=3000) {
  require(rtweet)
  tweet_table <- search_tweets(string,n=n,
                              retryonratelimit = TRUE,
                              include_rts = FALSE,
                              lang = "en")
  return(plain_tweets(tweet_table$text))
}

raw_tweets <- my_search_tweets("Katy Perry")
katy_raw_tweets <- raw_tweets # save for later


# Next we create a Vector Source from which we will create a Volatile Corpus:
library(tm)  # The tm package let's us create a Corpus

cleaned_tweets_vec  <- VectorSource(raw_tweets)
cleaned_tweets_corp <- VCorpus(cleaned_tweets_vec)

# Next we have to clean the Corpus. tm allows us to map predefined
# functions from its own library of cleanup functions. And we can 
# define our own

kp_stopwords <- c(stopwords("english"),
                  "katy","perry","official","video","hey","perrys",
                  "now","katyperry","youtube","american")

cleaned_tweets_corp <- tm_cleaner(cleaned_tweets_corp,
                                   stop=kp_stopwords)
          
cleaned_tweets_tdm <- TermDocumentMatrix(cleaned_tweets_corp)

# Save a copy of the cleaned tdm for later use
katy_cleaned_tweets_tdm <- cleaned_tweets_tdm 

cleaned_tweets_tdm_m <- as.matrix(cleaned_tweets_tdm)

terms_freq <- rowSums(cleaned_tweets_tdm_m)


terms_freq_sorted <- sort(terms_freq,decreasing=T)

terms_freq_sorted[1:10]
    
# But it would be better to look at a graph

barplot(terms_freq_sorted[1:10],
        col="aquamarine",
        las=2, 
        main="Ten Highest Word Frequencies - Katy Perry Tweets")


# Next we can create a barplot of the 10 most frequently appearing words

# But a Word Cloud would look better:
library(wordcloud)
library(RColorBrewer)

# We create a data frame of frequent terms and their 
# respective frequencies

df <- data.frame(word=names(terms_freq_sorted),
                 freq=terms_freq_sorted,
                 stringsAsFactors = FALSE)

# We'll pick a nice color palette for a word cloud
# We need darker colors but not too dark

color_palette <- brewer.pal(5,"Dark2") 

# Finally draw the word cloud
wordcloud(df$word,df$freq,max.words = 90,colors = color_palette)





# Let's look at a generating a word cluster using the \textbf{hclust} function:
# Compute the distance between the rows of mtcars

dist_m <- dist(mtcars)

# Create a clustered grouping using the matrix

clust <- hclust(dist_m)

# Plot a Dendrogram to visualize relationships

plot(clust,hang=-1)


# We can do this with a Term Document Matrix which can have many more rows / tweets

cleaned_tweets_vec  <- VectorSource(katy_raw_tweets)
cleaned_tweets_corp <- VCorpus(cleaned_tweets_vec)

kp_stopwords <- c(stopwords("english"),"katy","perry","katyperry")

# Clean the Katy Perry tweets and create a TDM

cleaned_tweets_corp <- tm_cleaner(cleaned_tweets_corp, stop=kp_stopwords)
cleaned_tweets_tdm <- TermDocumentMatrix(cleaned_tweets_corp)

# Remove the sparse terms from the TDM 
tweets_tdm <- removeSparseTerms(cleaned_tweets_tdm,sparse=0.975)

# Create a matrix from the TDM and then a data frame
# which is then used to create a distance object 

tdm_m <- as.matrix(tweets_tdm)
tdm_df <- as.data.frame(tdm_m)
tweets_dist <- dist(tdm_df)
hc <- hclust(tweets_dist)

plot(hc,hang=-1,main="Katy Perry Tweet Dendrogram")


# We can decorate the dendrogram:
# Remove the sparse terms from the TDM 
tweets_tdm <- removeSparseTerms(cleaned_tweets_tdm,sparse=0.975)

# Create a matrix from the TDM and then a data frame
# which is then used to create a distance object 

tdm_m <- as.matrix(tweets_tdm)
tdm_df <- as.data.frame(tdm_m)
tweets_dist <- dist(tdm_df)
hc <- hclust(tweets_dist)

plot(hc,hang=-1,main="Katy Perry Tweet Dendrogram")

# Put rectangles for ech cluster
rect.hclust(hc,k=5)

#
(cleaned_tweets_tdm <- TermDocumentMatrix(cleaned_tweets_corp))


# Remove the sparse terms from the TDM 

(tweets_tdm <- removeSparseTerms(cleaned_tweets_tdm,sparse=0.975))

# We can use the \texttt{SentimentAnalysis} package to help us

# library(SentimentAnalysis)

text <- c("Gee Delta. Thanks for losing my luggage today",
          "I had a great flight and experienced few problems",
          "Delta. Never again will I fly using your services.")

sentiment <- analyzeSentiment(text)

convertToBinaryResponse(sentiment$SentimentQDAP)

convertToBinaryResponse(sentiment$SentimentLM)

convertToBinaryResponse(sentiment$SentimentGI)

sentiment <- analyzeSentiment(katy_cleaned_tweets_tdm)

str(sentiment)

binary <- convertToBinaryResponse(sentiment$SentimentQDAP)

binary[1:10]

(binary_table <- table(binary))

grep("negative",binary)[1:35]


raw_tweets[c(29,30,32)]

# You can look at pairs of words
library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

cleaned_tweets_tdm <- TermDocumentMatrix(cleaned_tweets_corp,
control = list(tokenize = BigramTokenizer))



myCleaner <- function(tweets, wordcloud=TRUE, mystop=stopwords(),bigram=TRUE) {
  dyn.load(paste0(system2('/usr/libexec/java_home', stdout = TRUE), '/jre/lib/server/libjvm.dylib'))
  x <- c("tm","wordcloud","RColorBrewer","rtweet","RWeka")
  lapply(x, require, character.only = TRUE)
  
  plain_tweets_corp <- VCorpus(VectorSource(plain_tweets(tweets)))
  cleaned_tweets_corp <- tm_cleaner(plain_tweets_corp,stop=mystop)
  
  # CREATE THE TDM and TDM MATRIX 
  
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  
  if (bigram) {
    cleaned_tweets_tdm <- TermDocumentMatrix(cleaned_tweets_corp, control = list(tokenize = BigramTokenizer))
  } else {
    cleaned_tweets_tdm <- TermDocumentMatrix(cleaned_tweets_corp)
  }

  cleaned_tweets_tdm_m <- as.matrix(cleaned_tweets_tdm)
  
  if (wordcloud) {
    terms_freq <- rowSums(cleaned_tweets_tdm_m)
    terms_freq_sorted <- sort(terms_freq,decreasing=T)
    
    df <- data.frame(word=names(terms_freq_sorted),freq=terms_freq_sorted,
                     stringsAsFactors = FALSE)
    
    color_palette <- brewer.pal(5,"Dark2") 
    wordcloud(df$word,df$freq,max.words = 90,colors = color_palette)
  }
  
  # RETURN THE TDM OBJECT 
  return(cleaned_tweets_tdm)
}


raw_tweets <- my_search_tweets("Taylor Swift")
stp <- c("taylor","swift","youtube",stopwords("en"))
myCleaner(raw_tweets,mystop=bigram=FALSE)

