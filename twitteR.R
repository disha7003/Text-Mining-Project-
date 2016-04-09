#################################################################
#                   twitter Authentication                      #
#################################################################


require("RCurl")||install.packages("RCurl"); library(RCurl)
require("twitteR")||install.packages("twitteR"); library(twitteR)

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
apiKey <- "t5og3MdNd7Z2xErbB5vBSyvc7"                     # Change the api key  with your api key
apiSecret <- "GDMZR8xb9XVKUweMmGtW2SG8pRAFZeXhM9JYRbETEIxrCp2y12"  # Change the api Secret  with your api Secret

twitCred <- OAuthFactory$new(consumerKey = apiKey, consumerSecret = apiSecret,requestURL = reqURL,
                             accessURL = accessURL, authURL = authURL)

twitCred$handshake(
  cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")
)

#enter the pin

registerTwitterOAuth(twitCred)



##############################################################
#           Extracting  Tweets                               #
##############################################################


require("twitteR")||install.packages("twitteR"); library(twitteR)

tweets = searchTwitter('#narendramodi', n=1000 )     # hash tag for tweets search and number of tweets

tweets = twListToDF(tweets)    # Convert from list to dataframe

tweets.df = tweets[,1]  # assign tweets for cleaning

tweets.df = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df);head(tweets.df) 

tweets.df = gsub("@\\w+", "", tweets.df);head(tweets.df) # regex for removing @user
tweets.df = gsub("[[:punct:]]", "", tweets.df);head(tweets.df) # regex for removing punctuation mark
tweets.df = gsub("[[:digit:]]", "", tweets.df);head(tweets.df) # regex for removing numbers
tweets.df = gsub("http\\w+", "", tweets.df);head(tweets.df) # regex for removing links
tweets.df = gsub("\n", " ", tweets.df);head(tweets.df)  ## regex for removing new line (\n)
tweets.df = gsub("[ \t]{2,}", " ", tweets.df);head(tweets.df) ## regex for removing two blank space
tweets.df =  gsub("[^[:alnum:]///' ]", " ", tweets.df)     # keep only alpha numeric 
tweets.df =  iconv(tweets.df, "latin1", "ASCII", sub="")   # Keep only ASCII characters
tweets.df = gsub("^\\s+|\\s+$", "", tweets.df);head(tweets.df)  # Remove leading and trailing white space

tweets[,1] = tweets.df # save in Data frame

write.table(tweets, file='NaMo.txt', row.names=F, sep="\t")  # export to txt tab delemited file

