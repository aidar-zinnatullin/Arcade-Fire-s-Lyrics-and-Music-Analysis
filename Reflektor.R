library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("quanteda")

reflektor <- read.csv("reflektor.csv")

reflektor[which.max(reflektor$likes),c("author", "text", "likes", "time", "numReplies")]
max(reflektor$likes)
reflektor[which.max(reflektor$likes), "text"]

comments <- reflektor$text

ddd <- unlist(comments, recursive = TRUE, use.names = TRUE)

ddddd <- as.vector(ddd)
is.vector(ddddd)

text.tmp <- system2("/Users/aidarzinnatullin/Downloads/mystem", c("-c", "-l", "-d"), input = ddddd, stdout = TRUE)

text.tmp

write.table(text.tmp, "text.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)



# Choose necessary file
text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))



inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
#docs <- tm_map(docs, toSpace, "\")
docs <- tm_map(docs, toSpace, "—")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "–")

docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove russian common stopwords
docs <- tm_map(docs, removeWords, stopwords("russian"))
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("you","the","que","and","your","muito","this","that","are","for","cara",
                                    "from","very","like","have","voce","man","one","nao","com","with","mais",
                                    "was","can","uma","but","ficou","meu","really","seu","would","sua","more",
                                    "it's","it","is","all","i'm","mas","como","just","make","what","esse","how",
                                    "por","favor","sempre","time","esta","every","para","i've","tem","will",
                                    "you're","essa","not","faz","pelo","than","about","acho","isso",
                                    "way","also","aqui","been","out","say","should","when","did","mesmo",
                                    "minha","next","cha","pra","sei","sure","too","das","fazer","made",
                                    "quando","ver","cada","here","need","ter","don't","este","has","tambem",
                                    "una","want","ate","can't","could","dia","fiquei","num","seus","tinha","vez",
                                    "ainda","any","dos","even","get","must","other","sem","vai","agora","desde",
                                    "dessa","fez","many","most","tao","then","tudo","vou","ficaria","foi","pela",
                                    "see","teu","those","were")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

docs_matrix <- TermDocumentMatrix(docs)
m <- as.matrix(docs_matrix)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

head(d, 10)

wordcloud(words = d$word, freq = d$freq,scale = c(4, .2),  min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=.35, 
          colors=brewer.pal(8, "Dark2"))


barplot(d[1:10,]$freq, las = 1, axes = T, names.arg = d[1:10,]$word,
        col = c("green", "red", "blue", "yellow", "orange", "lightblue", "lavender", "cornsilk", "lavender", "lightcyan"), main ="Наиболее употребляемые слова",
        xlab = "Частота слов", horiz = T, cex.names = 0.8, cex.axis = 0.8, xlim= c(0,2000))



#### Для словосочетаний

docs <- unlist(docs, recursive = TRUE, use.names = TRUE)
docs

write.table(docs, "text_colloc.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)


text <- readLines(file.choose())
collocations <- textstat_collocations(text, size = 2:3)
wordcloud(words = collocations$collocation, freq = collocations$count,
          scale=c(4,.6), min.freq = 50, max.words=Inf, 
          random.order=FALSE, rot.per=0.1, ordered.colors=FALSE, 
          random.color=TRUE, colors=brewer.pal(8, "Dark2"))
