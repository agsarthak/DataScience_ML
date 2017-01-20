# load libraries
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")

install.packages(Needed, dependencies=TRUE)
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

# load file
cname <- file.path("D:/GRAD_SCHOOL/Fall2016/Lecture3", "texts")
cname
dir(cname)

# load text in R
library(tm)
docs <- Corpus(DirSource(cname))
summary(docs)
inspect(docs[1])

########################## Preprocessing data ################################
# Removing punctuation
docs <- tm_map(docs, removePunctuation)
inspect(docs[1]) # Check to see if it worked

for(j in seq(docs))
{
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
}
inspect(docs[1]) # You can check a document (in this case the first) to see if it worked.


# Removed numbers
docs <- tm_map(docs, removeNumbers)
inspect(docs[2]) # Check to see if it worked


# Convert to lowercase
docs <- tm_map(docs, tolower)
inspect(docs[2]) # Check to see if it worked

# Remove stopwords
# For a list of the stopwords, see:
length(stopwords("english"))
stopwords("english")
docs <- tm_map(docs, removeWords, stopwords("english"))
inspect(docs[1]) # Check to see if it worked.

# Remove word endings - stemming
library(SnowballC)
docs <- tm_map(docs, stemDocument)
inspect(docs[1]) # Check to see if it worked.


# Remove whitespaces
docs <- tm_map(docs, stripWhitespace)
inspect(docs[1]) # Check to see if it worked.

# Tell R to treat your pre-processed doc as text doc
docs <- tm_map(docs, PlainTextDocument)


##################################### Stage the data ################################

# Create a document term matrix
dtm <- DocumentTermMatrix(docs)
dtm

mat <- as.matrix(dtm)
cb <- data.frame(illiad = mat[1,], odyssey = mat[2,])
head(cb)

cb1 <- subset(cb, cb$illiad != '0')
cb2 <- subset(cb1, cb1$odyssey != '0')


ggplot(cb2, aes(illiad, odyssey)) +
  geom_text(label = rownames(cb2),
            position=position_jitter())


inspect(dtm)
inspect(dtm[1, ])
dim(dtm)

dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.


######################### Data Exporation ##################################
freq.doc1 <- sort(colSums(as.matrix(dtm[1,])), decreasing=TRUE)
freq.doc2 <- sort(colSums(as.matrix(dtm[2,])), decreasing=TRUE)

wf.doc1 <- data.frame(word=names(freq.doc1), freq=freq.doc1)
head(wf.doc1)

wf.doc2 <- data.frame(word=names(freq.doc2), freq=freq.doc2)
head(wf.doc2)

library(ggplot2)
p.doc1 <- ggplot(subset(wf.doc1, freq>440), aes(word, freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p.doc1

p.doc2 <- ggplot(subset(wf.doc2, freq>440), aes(word, freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p.doc2


########### Word Cloud
library(wordcloud)

set.seed(142)
wordcloud(names(freq.doc1), freq.doc1, min.freq=60, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq.doc2), freq.doc2, max.words=100, rot.per=0.2, colors=dark2)


# Clustering
dtms <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space.
library(cluster)
d.doc1 <- dist(t(dtms[1,as.vector(dtms[1,] > 200)]), method="euclidian") # First calculate distance between words
d.doc2 <- dist(t(dtms[2,as.vector(dtms[2,] > 200)]), method="euclidian") # First calculate distance between words
fit.doc1 <- hclust(d=d.doc1, method="ward")
fit.doc2 <- hclust(d=d.doc2, method="ward")

plot.new()
plot(fit.doc1, hang=-1)
groups <- cutree(fit.doc1, k=5) # "k=" defines the number of clusters you are using
rect.hclust(fit.doc1, k=5, border="red") # draw dendogram with red borders around the 5 clusters

plot.new()
plot(fit.doc2, hang=-1)
groups <- cutree(fit.doc2, k=5) # "k=" defines the number of clusters you are using
rect.hclust(fit.doc2, k=5, border="red") # draw dendogram with red borders around the 5 clusters

# Relationship between Illiad and Odyssey
d.all <- dist(t(dtms[,as.vector(dtms[,] > 200)]), method="euclidian")
fit.all <- hclust(d=d.all, method="ward")
plot.new()
plot(fit.all, hang= -1)
groups <- cutree(fit.all, k=5) # "k=" defines the number of clusters you are using
rect.hclust(fit.all, k=5, border="red") # draw dendogram with red borders around the 5 clusters

#Terms higher in the plot appear more frequently within the corpus; 
#terms grouped near to each other are more frequently found together
