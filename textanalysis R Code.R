###############################################################
#             Invoke required library                         #
###############################################################

require("tm")||install.packages("tm")
require("rJava")||install.packages("rJava")
require("wordcloud")||install.packages("wordcloud")
require("textir")||install.packages("textir")
require("RWeka")||install.packages("RWeka")
require("qdap")||install.packages("qdap")
require("maptpx")||install.packages("maptpx")

library("tm")
library("rJava")
library("wordcloud")
library("textir")
library("RWeka")
library("qdap")
library("maptpx")

#############################################################
#         Attach userdefined functions                      #
#############################################################

source(file.choose())  #    Select text_functions.R

############################################################
#           Read the Text Data in R                        # 
############################################################
text  = readLines (file.choose())     # Select txt file you want to analyse
head(text)

Doc.id=seq(1:length(text))            # Assign Document no for each Document 
calib=data.frame(Doc.id,text)         # Create a dataframe for text documents with document ID

stpw = readLines(file.choose())      # Select stopwords.txt file
stpw1 = stopwords('english')         # tm package stop word list
comn  = unique(c(stpw, stpw1))       # Union of two list
stopwords = unique(c(gsub("'","",comn),comn)) # final stop word lsit after removing punctuation
head (stopwords)


#############################################################
#                        Text Cleaning                      #
#############################################################

test = text.clean(text)                         # basic HTML Cleaning etc
test  =  removeWords(test,stopwords)            # removing stopwords created above
head(test)                                      # print top documents

clean_text = test
write.table(clean_text, file='clean_text.txt', row.names=F, sep="\t")

########################################################
#             Create Document Term Matrix              #
########################################################

x1 = Corpus(VectorSource(test))          # Create the corpus
x1 = n.gram(x1,"bi",3)                   # Encoding bi-gram with atleast frequency 3 as uni-gram

dtm1 = custom.dtm(x1,"tf")               # Document Term Frequency 
dtm2 = custom.dtm(x1,"tfidf")            # Term Frequency Inverse Document Frequency Scheme


######################################################
#         Basic Analysis                             #

#   1- Using Term frequency(tf)             

freq1 = (sort(apply(dtm1,2,sum), decreasing =T)) # Calcualte term frequency
freq1[1:80]                                     # View top 80 terms 

windows()  # New plot window
wordcloud(names(freq1), freq1, scale=c(4,0.5),1, max.words=200,colors=brewer.pal(8, "Dark2")) # Plot results in a word cloud 
title(sub = "Term Frequency - Wordcloud")


#   2- UsingTerm Frequency Inverse Document Frequency (tfidf)             
freq2 = (sort(apply(dtm2,2,sum), decreasing =T)) # Calcualte term frequency
freq2[1:80]                                     # View top 80 terms 

windows()  # New plot window
wordcloud(names(freq2), freq2, scale=c(4,0.5),1, max.words=200,colors=brewer.pal(8, "Dark2")) # Plot results in a word cloud 
title(sub = "Term Frequency Inverse Document Frequency - Wordcloud")

###########################################################
#         Sentiment Analysis                              #
###########################################################

clean_text0 = clean_text[clean_text != ""]    # Remove Blank Document for sentiment Analysis

pol = polarity(clean_text0)       # Calculate the polarity from qdap dictionary
wc = pol$all[,2]                  # Word Count in each doc
val = pol$all[,3]                 # average polarity score
p  = pol$all[,4]                  # Positive words info
n  = pol$all[,5]                  # Negative Words info  

positive_words = unique(setdiff(unlist(p),"-"))  # Positive words list
negative_words = unique(setdiff(unlist(n),"-"))  # Negative words list

print(positive_words)       # Print results
print(negative_words)       # Print results

########################################################
# Create Postive Words wordcloud                      #
########################################################

tdm_temp = t(TermDocumentMatrix(Corpus(VectorSource(clean_text0))))
pos.tdm = tdm_temp[,(match(positive_words,colnames(tdm_temp)))]
m = as.matrix(pos.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows()
wordcloud(names(v), v, scale=c(4,0.5),1, max.words=100,colors=brewer.pal(8, "Dark2"))
title(sub = "Positive Words - Wordcloud")


########################################################
# Create Negative Words wordcloud                      #
########################################################

neg.tdm = tdm_temp[,(match(negative_words,colnames(tdm_temp)))]
m = as.matrix(neg.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows()
wordcloud(names(v), v, scale=c(4,0.5),1, max.words=100,colors=brewer.pal(8, "Dark2"))         
title(sub = "Negative Words - Wordcloud")


#######################################################
#        Positive words vs Negative Words plot        #
#######################################################

len = function(x){
  if ( x == "-" && length(x) == 1)  {return (0)} 
  else {return(length(unlist(x)))}
}


pcount = unlist(lapply(p, len))
ncount = unlist(lapply(n, len))
doc_id = seq(1:length(wc))

windows()
plot(doc_id,pcount,type="l",col="green",xlab = "Document ID", ylab= "Word Count")
lines(doc_id,ncount,type= "l", col="red")
title(main = "Positive words vs Negative Words" )
legend("topright", inset=.05, c("Positive Words","Negative Words"), fill=c("green","red"), horiz=TRUE)

#################################################
## --- model based text analytics ------ ###
#################################################

K = 6
## Bayes Factor model selection (should choose K or nearby)

summary(simselect <- topics(dtm1, K=K+c(-4:4)), nwrd=0)
K = simselect$K;   # Change simselect$K to any the number of topics you want to fit in model

# -- run topic model for selected K -- #
summary( simfit <- topics(dtm1,  K=K, verb=2), nwrd = 12 )
rownames1 = gsub(" ", ".", rownames(simfit$theta));  rownames(simfit$theta) = rownames1;  

#######################################################
### compute lift for all terms across all topics ###

theta = simfit$theta

lift = theta*0;  sum1 = sum(dtm1)

for (i in 1:nrow(theta)){  
  for (j in 1:ncol(theta)){
    
    ptermtopic = 0; pterm = 0;
    
    ptermtopic = theta[i, j]
    
    pterm = sum(dtm1[,i])/sum1
    
    lift[i, j] = ptermtopic/pterm
    
  }}


######################################
# Plot Wordcloud for each topic
for   (my_i in 1:ncol(lift)) {
  freq = as.matrix((theta)[(match(rownames((lift)[((lift)[,my_i] > 1),]),rownames((theta)))),][,my_i])
  freq = as.matrix(freq[(order(freq[,1], decreasing=T)),])
{if (nrow(freq) >= 100) {n = 100}
 else {n = nrow(freq)}
  }
top_word = as.matrix(freq[1:n,])
#plot.new()
windows()
wordcloud(rownames(top_word), top_word,  scale=c(4,0.5), 1, , random.order=FALSE, random.color=FALSE, colors=brewer.pal(8, "Dark2"));
mtext(paste("Latent Topic",my_i), side = 3, line = 2, cex=2)
}

##########################################
# Calculate Document proportion in topics

  eta = function(mat, dtm) {
  mat1 = mat/mean(mat);  terms1 = rownames(mat1);
  eta.mat = matrix(0, 1, ncol(mat1))
  
  for (i in 1:nrow(dtm)){
    a11 = as.data.frame(matrix(dtm[i,]));  rownames(a11) = colnames(dtm)
    a12 = as.matrix(a11[(a11>0),]);  rownames(a12) = rownames(a11)[(a11>0)];  rownames(a12)[1:4]
    a13 = intersect(terms1, rownames(a12));  	a13[1:15];	length(a13)
    a14a = match(a13, terms1); 		# positions of matching terms in mat1 matrix
    a14b = match(a13, rownames(a12))		
    a15 = mat1[a14a,]*matrix(rep(a12[a14b,], ncol(mat1)), ncol = ncol(mat1))
    eta.mat = rbind(eta.mat, apply(a15, 2, mean))	
    rm(a11, a12, a13, a14a, a14b, a15)
  }
  eta.mat = eta.mat[2:nrow(eta.mat), ] 	# remove top zeroes row
  row.names(eta.mat)=row.names(dtm)
  return(eta.mat)
}

twc = eta(lift,dtm1)

##############################################
# Find Document by each topic

eta.file = function(mat,calib,n) {
  s = list()
  
  for (i in  1: ncol(mat))
  {read_doc = mat[order(mat[,i], decreasing= T),]
   read_names = row.names(read_doc[1:n,])  
   s[[i]] = calib[(match(read_names, calib$Doc.id)),2]
  }
  return(s)
}

temp=eta.file(twc,calib,5)

for (j in 1:length(temp)){
  cat("\n")
  cat("################################# \n")
  cat("Top ", 5, "documents for topic ",j,"\n")
  cat("################################# \n")
  cat("\n")
  #print(paste("Top 10 documents heavily loading on topic",j) )
  print(temp[[j]])
}

##################
# Scroll up and see the top document by each topic


