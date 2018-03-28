#Charge the wordcloud library
#install.packages("wordcloud")
library(wordcloud)

#Create a list of words (Random words concerning my work)
a=c("EPTC","Transito","Mobilidade","Ruas Completas","Ruas","Completas","WRI", 
    "Porto Alegre","Acessibilidade","Mobilidade","Compartilhado","planejamento","EPV", 
    "GPTC","Pessoas","Transito Ativo","Joao Alfredo","Ciclorota","Transito Compartilhado","Acessibilidade","EPTC", "EPTC",
    "Planejamento","Urbanismo","Vocacao","Comercio Local","Totens","Acessibilidade", "Mobilidade",
    "Ruas Completas","WRI","Ativo","Transporte","segurança")

#I give a frequency to each word of this list 
b=sample(seq(0,1,0.01) , length(a) , replace=TRUE) 

#The package will automatically make the wordcloud ! (I add a black background)
par(bg="black") 
wordcloud(a , b , col=terrain.colors(length(a) , alpha=0.9) , rot.per=0.3 )
wordcloud(a , b , col=rainbow(length(a) , alpha=0.9) , rot.per=0.3 )




######################


# Packages
library(reshape)
library(tm)
library(wordcloud)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# -- STEP 1 : GET THE DATA
# A dataset with 5485 lines, each line has several words.
dataset=read.delim("https://raw.githubusercontent.com/TATABOX42/text-mining-in-r/master/dataset.txt", header=FALSE)

# The labels of each line of the dataset file
dataset_labels <- read.delim("https://raw.githubusercontent.com/TATABOX42/text-mining-in-r/master/labels.txt",header=FALSE)
dataset_labels <- dataset_labels[,1]
dataset_labels_p <- paste("class",dataset_labels,sep="_")
unique_labels <- unique(dataset_labels_p)

# merge documents that match certain class into a list object
dataset_s <- sapply(unique_labels,function(label) list( dataset[dataset_labels_p %in% label,1] ) )


# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# -- STEP2 : COMPUTE DOCUMENT CORPUS TO MAKE TEXT MINING
# convert each list content into a corpus
dataset_corpus <- lapply(dataset_s, function(x) Corpus(VectorSource( toString(x) ))) 

# merge all documents into one single corpus
dataset_corpus_all <- dataset_corpus[[1]]
for (i in 2:length(unique_labels)) { dataset_corpus_all <- c(dataset_corpus_all,dataset_corpus[[i]]) }

# remove punctuation, numbers and stopwords
dataset_corpus_all <- tm_map(dataset_corpus_all, removePunctuation)
dataset_corpus_all <- tm_map(dataset_corpus_all, removeNumbers)
dataset_corpus_all <- tm_map(dataset_corpus_all, function(x) removeWords(x,stopwords("english")))

#remove some unintersting words
words_to_remove <- c("said","from","what","told","over","more","other","have","last","with","this","that","such","when","been","says","will","also","where","why","would","today")
dataset_corpus_all <- tm_map(dataset_corpus_all, removeWords, words_to_remove)

# compute term matrix & convert to matrix class --> you get a table summarizing the occurence of each word in each class.
document_tm <- TermDocumentMatrix(dataset_corpus_all)
document_tm_mat <- as.matrix(document_tm)
colnames(document_tm_mat) <- unique_labels
document_tm_clean <- removeSparseTerms(document_tm, 0.8)
document_tm_clean_mat <- as.matrix(document_tm_clean)
colnames(document_tm_clean_mat) <- unique_labels

# remove words in term matrix with length < 4
index <- as.logical(sapply(rownames(document_tm_clean_mat), function(x) (nchar(x)>3) ))
document_tm_clean_mat_s <- document_tm_clean_mat[index,]

# Have a look to the matrix you are going to use for wordcloud !
head(document_tm_clean_mat_s)


# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# -- STEP 3 : make the graphics !

# Graph 1 : first top 500 discriminant words
png("#102_1_comparison_cloud_top_500_words.png", width = 480, height = 480)
comparison.cloud(document_tm_clean_mat_s, max.words=500, random.order=FALSE,c(4,0.4), title.size=1.4)
dev.off()

# Graph 2 : first top 2000 discriminant words
png("#102_1_comparison_cloud_top_2000_words.png", width = 480, height = 480)
comparison.cloud(document_tm_clean_mat_s,max.words=2000,random.order=FALSE,c(4,0.4), title.size=1.4)
dev.off()

# Graph 3: commonality word cloud : first top 2000 common words across classes
png("#103_commonality_wordcloud.png", width = 480, height = 480)
commonality.cloud(document_tm_clean_mat_s, max.words=2000, random.order=FALSE)
dev.off()




# library
library(wordcloud2) 
library(xlsx)
dados <- read.xlsx("wordfreq.xlsx", 1, encoding="UTF-8")


# have a look to the example dataset
demoFreq


head(dados, 30)
wordcloud2(dados, size=1.6)
wordcloud2(demoFreq, size=1.6)


# Gives a proposed palette
wordcloud2(demoFreq, size=1.6, color='random-dark')

# or a vector of colors. vector must be same length than input data
wordcloud2(demoFreq, size=1.6, color=rep_len( c("green","blue"), nrow(demoFreq) ) )

# Change the background color
wordcloud2(demoFreq, size=1.6, color='random-light', backgroundColor="black")


# Change the shape:
wordcloud2(demoFreq, size = 0.7, shape = 'cloud')

# Change the shape using your image
wordcloud2(demoFreq, figPath = "peace.png", size = 1.5, color = "skyblue", backgroundColor="black")



ww=wordcloud2(demoFreq, size = 2.3, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)
ww

wordcloud2(demoFreqC, size = 2, fontFamily = "????????????", color = "random-light", backgroundColor = "grey")



letterCloud( demoFreq, word = "R", color='random-light' , backgroundColor="black")
letterCloud( demoFreq, word = "PEACE", color="white", backgroundColor="pink")
