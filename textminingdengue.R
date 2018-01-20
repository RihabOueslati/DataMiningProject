filePath <- ("E:/4BI4/Data/Projet DM/documentairedengue.txt")
filePath = readLines("E://4BI4//Data//Projet DM//documentaire dengue.txt")

text <- readLines(filePath)
library(tm)
library(NLP)
docs <- Corpus(VectorSource(text))
inspect(docs)

toSpace <- content_transformer(function(x,pattern) gsub(pattern,"", x))
docs <- tm_map(docs,toSpace,"/")
docs <- tm_map(docs,toSpace,"@")
docs <- tm_map(docs,toSpace,"\\|")
# Convert text to lowercase
docs <- tm_map(docs,content_transformer(tolower))
# Delete numbers
docs <- tm_map(docs,removeNumbers)
# Delete punctuations
docs <- tm_map(docs,removePunctuation)
# Remove blank english words
docs <- tm_map(docs,removeWords,stopwords("English"))
# Remove additional empty spaces
docs <- tm_map(docs,stripWhitespace)
# Text stemming
docs <- tm_map(docs,stemDocument)
# Delete unused Words
docs <- tm_map(docs, removeWords,c("rien","can","also","use","jai","quon","non","tres","trs","nest","dheur","monday","tuesday","wednesday","friday","thursday","saturday","sunday","January","February","May","March","April","June","July","August","September","October","November","December","hill","will","octob","der","und","cest","quil","quelqu","chaqu","faut","dune","entr","dun","parfoi","comm"))
adocs <- tm_map(docs,stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word=names(v),freq=v)
head(v)
install.packages("wordcloud")
library(wordcloud	)

library(RColorBrewer)
head(d,10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,max.words = 300, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))


----------------
Vous pouvez voir les mots les plus fréquents comme suit. L’exemple, ci-dessous,
 montre les mots qui sont fréquents au moins 40 fois dans le texte :
findFreqTerms(dtm, lowfreq = 40)
-----------------------------------
Vous pouvez analyser l’association entre les mots (leur corrélation) en utilisant
la fonction findAssocs(). Le code R ci-dessous identifie les mots qui sont
le plus fréquemment associés à “freedom” dans le texte I have a dream :
findAssocs(dtm, "dengu", 0.5)
findAssocs(dtm, "diseas" , 0.1)

assoc<-findAssocs(dtm, c("case","dengu","virus","infect") , 0.4)
assoc




barplot(d[1:30,]$freq, las = 2, names.arg = d[1:30,]$word,
col ="springgreen4", main ="Most frequent words",
ylab = "Word frequencies")



library(dplyr)
library(igraph)
require(ggplot2)
test <- data.frame(corr = findAssocs(dtm,"virus",0.1)[[1]],terms = names(findAssocs(dtm,"virus",0.1)[[1]]))
test$terms <- factor(test$terms ,levels = test$terms)
ggplot(test, aes( y = terms  ) ) + geom_point(aes(x = corr), data = test)


test <- data.frame(corr = findAssocs(dtm,"mauvais",0.10)[[1]],terms = names(findAssocs(dtm,c("mauvais","bon"),0.10)[[1]]))
test$terms <- factor(test$terms ,levels = test$terms)
ggplot(test, aes( y = terms  ) ) + geom_point(aes(x = corr), data = test) 


-----------
terms <- c("bon","mauvais")
probs <- c(0.15,0.15)
df <- data.frame(terms = terms, probs = probs)
g <- graph.data.frame(df, directed = TRUE)
plot(g)
-----------