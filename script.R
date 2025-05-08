install.packages("tm")
library(tm)

book <- readLines("APrincessOfMars.txt")
chapter_starts <- grep("^CHAPTER [IVXLCDM]+", book)
dir.create("chapters", showWarnings = FALSE)
num_chapters_to_write <- 14

for (i in seq_len(num_chapters_to_write)) 
{
    start <- chapter_starts[i]
    
    if (i < length(chapter_starts)) {
         end <- chapter_starts[i + 1] - 1
    } 
    else {
         end <- length(book)
    }
     
    chapter_text <- book[start:end]
     
    chapter_filename <- paste0("chapters/chapter_", i, ".txt")
    writeLines(chapter_text, chapter_filename)
}

princess<-VCorpus(DirSource(".",ignore.case = TRUE,mode = "text"))
str(princess)
princess

ptext<-princess[[1]]
ptext
ptext[1]

princessDTM<-DocumentTermMatrix(princess)
princessDTM
inspect(princessDTM)
str(princessDTM)

princessTDM<-TermDocumentMatrix(princess)
princessTDM

princessdf<-data.frame(ptext[1])
princessdf[1]

chapter_dir <- "chapters"
chapter_files <- list.files(chapter_dir, full.names = TRUE)[1:14]
dir.create("chapter_analysis", showWarnings = FALSE)

for (i in seq_along(chapter_files)) {
  
  chapter_text <- readLines(chapter_files[i])
  
  chapter_full_text <- paste(chapter_text, collapse = " ")
  
  words <- unlist(strsplit(chapter_full_text, "\\W+"))
  
  words <- words[words != ""]
  
  longest_words <- words[order(nchar(words), decreasing = TRUE)][1:10]
  
  sentences <- unlist(strsplit(chapter_full_text, "(?<=[.!?])\\s+", perl=TRUE))
  
  longest_sentences <- sentences[order(nchar(sentences), decreasing = TRUE)][1:10]
  
  output_filename <- paste0("chapter_analysis/chapter_", i, "_analysis.txt")
  
  cat("Top 10 Longest Words:\n", file = output_filename)
  cat(paste(longest_words, collapse = "\n"), file = output_filename, append = TRUE)
  
  cat("\n\nTop 10 Longest Sentences:\n", file = output_filename, append = TRUE)
  cat(paste(longest_sentences, collapse = "\n\n"), file = output_filename, append = TRUE)

}

removeNumPunct<-function(x) gsub("[^[:alpha:][:space:]]*","",x)
removeNumPunct

princesscl<-tm::tm_map(princess,content_transformer(removeNumPunct))
princesscl
str(princesscl)
inspect(princesscl)

princesslow<-tm_map(princesscl, tm::content_transformer(tolower))
princesslow
str(princesslow)
inspect(princesslow)

princesslowDTM<-DocumentTermMatrix(princesslow)
princesslowDTM
str(princesslowDTM)
inspect(princesslowDTM)

as.matrix(princesslowDTM)

mystopwords<-c(tm::stopwords("english"))
mystopwords

princessstop<-tm::tm_map(princesslow,tm::removeWords,mystopwords)
tm::inspect(princessstop[[1]])

princessstopTDM<-tm::TermDocumentMatrix(princessstop)
princessstopTDM

freqterms<-tm::findFreqTerms(princessstopTDM,lowfreq = 5)
freqterms

nchar(freqterms[7])
freqterms[7]

princesstf<-tm::termFreq(princessstop[[1]])
princesstf

tm::inspect(princessstopTDM)

princessdfnew<-as.data.frame(princessstopTDM[[1]])
princessdist<-dist(princessdfnew)
princessDG<-hclust(princessdist,method="ward.D2")
str(princessDG)
plot(princessDG)

custom_stopwords <- c(
  "a", "an", "the", "and", "or", "but", "if", "then", "than", "so", "thus", 
  "yet", "still", "also", "very", "just", "even", "only", "ever", "never",
  "in", "on", "at", "by", "for", "with", "to", "from", "of", "out", "up", 
  "down", "over", "under", "about", "around", "before", "after", "between", 
  "among", "through", "into", "onto", "upon", "without", "within",
  "he", "she", "it", "they", "we", "you", "i", "me", "him", "her", "us", "them", 
  "my", "your", "his", "their", "our", "its", "mine", "yours", "ours", "theirs",
  "can", "could", "may", "might", "shall", "should", "will", "would", "must",
  "now", "soon", "then", "today", "tonight", "tomorrow", "yesterday", "again",
  "always", "sometimes", "often", "never", "ever",
  "one", "two", "three", "few", "many", "much", "more", "most", "some", "any", 
  "none", "all", "several", "each", "every", "both",
  "say", "said", "says", "go", "goes", "went", "come", "comes", "came", "make", 
  "makes", "made", "get", "gets", "got", "know", "knows", "knew", "see", "sees", 
  "saw", "think", "thinks", "thought", "take", "takes", "took", "give", "gives", 
  "gave", "look", "looks", "looked",
  "good", "bad", "better", "best", "great", "small", "large", "little", "long", 
  "short", "old", "new", "young", "first", "last", "next",
  "thou", "thee", "thy", "hath", "doth", "art", "o", "ye", "thus", "hence", 
  "whence", "wherefore",
  "king", "queen", "prince", "princess", "lord", "lady", "master", "miss", 
  "sir", "mr", "mrs", "ms", "child", "children", "boy", "girl", "man", "woman",
  "indeed", "therefore", "meanwhile", "perhaps", "however", "besides", "whose", 
  "where", "when", "while", "because", "although", "though", "whether",
  "away", "back", "forward", "near", "far", "left", "right", "behind", "beside",
  "began", "continued", "replied", "answered", "cried", "called", "asked",
  "thing", "things", "something", "anything", "everything", "nothing"
)
custom_stopwords
princessstopnew <- tm_map(princessstop, removeWords, custom_stopwords)
princessnewTDM <- TermDocumentMatrix(princessstopnew)
princessnewTDM_sparse <- removeSparseTerms(princessnewTDM, sparse = 0.5)
princess_mat <- as.matrix(princessnewTDM_sparse)
princess_df2 <- as.data.frame(princess_mat)
princess_dist2 <- dist(princess_df2)
princess_DG2 <- hclust(princess_dist2, method = "ward.D2")
plot(princess_DG2, main = "Dendrogram after stopword and sparse term removal")

install.packages("wordcloud")
library(wordcloud)

words<-names(princesstf)
words
wordsfreq<-as.numeric(princesstf)
pal <- brewer.pal(8, "Dark2")
str(pal)
princessWC<-wordcloud(words,wordsfreq,colors = pal[1:4],random.order = FALSE)
pal <- brewer.pal(9, "Spectral")
princessWC2<-wordcloud(words,wordsfreq,colors = pal[1:4],random.order = FALSE)
str(princessWC2)

install.packages("quanteda")
library(quanteda)

princesstext<-princesscl[[1]]
princesstext$content[1:10]

princesstokens<-quanteda::tokens(princesstext$content)
str(princesstokens)

princessdfm<-quanteda::dfm(princesstokens)
str(princessdfm)

princessdocfreq<-quanteda::docfreq(princessdfm)
str(princessdocfreq)
princessdocfreq

princessweights<-quanteda::dfm_weight(princessdfm)
str(princessweights)
princessweights

princesstfidf<-quanteda::dfm_tfidf(princessdfm,scheme_tf = "count",scheme_df = "inverse")
str(princesstfidf)
princesstfidf

install.packages("syuzhet")
library(syuzhet)

princesstextdf<-as.data.frame(princesstext$content)
princesstextdf

princessasstring<-get_text_as_string("APrincessOfMars.txt")
princessasstring

ps<-get_sentences(princessasstring)
ps
str(ps)

pssyuzhet<-get_sentiment(ps, "syuzhet")
pssyuzhet

psbing<-get_sentiment(ps,"bing")
psbing

psdictionary<-get_sentiment_dictionary()
psdictionary

psdictionarybing<-get_sentiment_dictionary("bing")
psdictionarybing

pssum<-sum(pssyuzhet)
pssum
psbingsum<-sum(psbing)
psbingsum

psmean<-mean(pssyuzhet)
psmean
psbingmean<-mean(psbing)
psbingmean

summary(pssyuzhet)
summary(psbing)

pssyuzhetnrc<-get_nrc_sentiment(ps)
pssyuzhetnrc
pssumnrc<-sum(pssyuzhetnrc)
pssumnrc
psmeannrc<-colMeans(pssyuzhetnrc)
psmeannrc
summary(pssyuzhetnrc)

plot(pssyuzhet,main="Plot Trajectory",xlab="Narrative",ylab="Emotional Valence")
plot(psbing,main="Plot Trajectory Bing",xlab="Narrative",ylab="Emotional Valence")

pssentimentpctvalue<-get_percentage_values(pssyuzhet,bins=10)
structure(pssentimentpctvalue)
plot(pssentimentpctvalue,main="% Value (10 Bins)",xlab="Narrative",ylab="Emotional Valence",col="red")

pssentimentpctvalue<-get_percentage_values(pssyuzhet,bins=20)
structure(pssentimentpctvalue)
plot(pssentimentpctvalue,main="% Value (20 Bins)",xlab="Narrative",ylab="Emotional Valence",col="blue")

install.packages("topicmodels")
install.packages("textmineR")
library(topicmodels)
library(textmineR)

princess_quanteda <- corpus(princess)
texts=corpus_reshape(princess_quanteda, to = "paragraphs")
tokens_texts <- tokens(texts, remove_punct = TRUE)
tokens_stemmed <- tokens_wordstem(tokens_texts)
par_dtm <- dfm(tokens_stemmed)
par_dtm <- dfm_remove(par_dtm, stopwords("english"))
par_dtm <- dfm_trim(par_dtm)
par_dtm <- convert(par_dtm, to = "topicmodels")
set.seed(1)
lda_model <- topicmodels::LDA(par_dtm, method="Gibbs", k = 5)
terms(lda_model, 5)

chapter_dir <- "chapters"
chapter_files <- list.files(chapter_dir, full.names = TRUE)[1:14]
texts <- sapply(chapter_files, function(x) paste(readLines(x), collapse = " "))
dtm <- CreateDtm(
     doc_vec = texts,
     doc_names = paste0("chapter_", seq_along(texts)),
     ngram_window = c(1, 1),
     stopword_vec = tm::stopwords("english"))
lda_model <- FitLdaModel(
     dtm = dtm,
     k = 5,
     iterations = 500,
     burnin = 200,
     alpha = 0.1,
     beta = 0.05)
top_terms <- GetTopTerms(lda_model$phi, M = 5)
print(top_terms)
lsa_model <- FitLsaModel(dtm = dtm, 
                          k = 5)
lsa_top_terms <- GetTopTerms(phi = lsa_model$phi, M = 5)
print(lsa_top_terms)