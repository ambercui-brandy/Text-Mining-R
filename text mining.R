# 1. read data
# library: text mining
install.packages("tm")
library(tm)
text <- readLines(file.choose())

# 2. build corpus, converting to second format first
corpus <- iconv(text, to = 'utf-8-mac')
corpus <- Corpus(VectorSource(corpus))
# look at the first document, corpus has 467 documents.
inspect(corpus[1])

# 3. clean text: convert everything to lower case
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:7])
# remove punctuation
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:7])
# replace text: 'claims' -> 'claim'

# finally, remove stopwords (but leave extra whites where removed)
stopwords('english') # see what's in stopwords
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[8])
cleanset <- tm_map(cleanset,stripWhitespace) # strip extra white space

# 4. Term Document Matrix
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm) # convert to matrix
tdm[1:10,1:20] # inspect first 10 rows and first 20 columns

# 5. Bar plot
w <- rowSums(tdm) # frenquency of words
barplot(w[w>10]) # or w10 <- subset(w,w>10), barplot(w10)
barplot(w10,
        las = 2, # making words vertical
        col = c(rainbow(300)))

# 6. WordCloud
library(wordcloud)
wordcloud(words = (names(w)),
          freq = w,
          max.words = 100,
          min.freq = 10,
          scale = c(4,0.3),
          rot.per = 0.5)
# WordCloud2
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,
           shape = 'triangle', # star, circle...
           size = 0.5, # warranty is visible now
           color = "random-light", # random-dark...
           backgroundColor = 'black')
letterCloud(w,
            word="R",
            color = "random-light", # random-dark...
            backgroundColor = 'black')getwd()
wordcloud2(w,
            figPath = "/Users/amber/Downloads/14.png",
            color = "black",
            size = 0.9)

# Sentiment Analysis
library(syuzhet)
apple.text <- read.csv("apple.csv", header = TRUE)
str(apple.text)
# run one of the following two lines
apple.text <- apple.text$text[apple.text$isRetweet == FALSE]
apple.text <- apple.text$text # only use first column: tweet content

corpus <- iconv(apple.text, to = "utf-8-mac")     # convert to corpus
s <- get_nrc_sentiment(corpus) # capture word counts that reflect each sentiment
head(s)
apple.text[2] # shows anger and fear
# create new column showing no +/- comment
s$neutral <- ifelse(s$positive + s$negative == 0,1,0) # if sum is 0, set value as 1, if else, as 0
# Barplot: column sums for total anger/anticipation...tweets
barplot(colSums(s),
        las = 2, # vertical x-axis lable
        col = rainbow(10))
table(apple.text$isRetweet)

get_nrc_sentiment("bearish")
