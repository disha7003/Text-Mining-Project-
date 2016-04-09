
require("tm")||install.packages("tm")
require(stringr)||install.packages("stringr"); 
require(utils)||install.packages("utils"); 
require(tm.plugin.webmining)||rminstall.packages("tm.plugin.webmining")
library("tm")
library("tm.plugin.webmining")
library("utils") 
library("stringr")  
#####################################################################

GoogleNews= function(query) {           
  
  text = WebCorpus(GoogleNewsSource("Playing it my way", params = list(hl = "en", q = "Playing it my way", ie = "utf-8", num= 10, output = "rss")))
  
  # strip relevant content from text 
  text = unlist(lapply(text, PlainTextDocument))

  
  
  # remove newline chars
  text = gsub("\n", "", text)  
  
  # write reviews to text
  write.table(text, file="xyz.txt", row.names=F, col.names=F)
  
}

#####################################################################
searchString = "Playing it my way"
GoogleNews(searchString)

#####################################################################

