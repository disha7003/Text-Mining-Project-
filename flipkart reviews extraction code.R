#####################################################################
# Function for downloading product reviews from flipkart
# Usage:
# url = "http://www.flipkart.com/apple-iphone-5s/product-reviews/ITMDV6F75DYXHMT4?pid=MOBDPPZZDX8WSPAT&type=all"
# pr_review = flipkart(url,10)
#
# url should be product reviews url (Make sure first you click on top reviews or show all)
# refer to the ppt for  getting the url
#
#####################################################################

require(stringr)||install.packages("stringr"); library(stringr)  
require(utils)||install.packages("utils"); library(utils)  


flipkart = function(url,        # Flipkart review URL. first click on show all or top review
                    n )       # Number of pages to extarct
  
{           
  
  text_page=character(0)   # define blank file
  
  pb <- txtProgressBar(min = 1, max = n, style = 3)    # define progress bar
  url = unlist(str_split(url,"&"))[1]
  
  for(i in 0:n){           # loop for url
    
    p =i*10	
    e = "&rating=1,2,3,4,5&reviewers=all&type=all&sort=most_helpful&start="
    url0 = paste(url,e,p,sep="")           # Create flipkart url in correct format
    
    text = readLines(url0)     # Read URL       
    
    text_start = grep("<span class=\"review-text\">",text)   # review start marker
    
    text_stop = grep("<div class=\"tpadding10 line feedback-container\">", text)  # review end marker
    
    
    setTxtProgressBar(pb, i)             # print progress bar
    
    if (length(text_start) == 0) break    # check for loop termination, i.e valid page found      
    
    for(j in 1:length(text_start))        # Consolidate all reviews     
    {
      text_temp = paste(paste(text[(text_start[j]+1):(text_stop[j]-2)]),collapse=" ")
      text_page = c(text_page,text_temp)
    }
    #Sys.sleep(1)
  }
  
  text_page =gsub("<.*?>", "", text_page)       # regex for Removing HTML character 
  text_page = gsub("^\\s+|\\s+$", "", text_page) # regex for removing leading and trailing white space
  return(text_page)       # return reviews
}
