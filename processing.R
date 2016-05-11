set.seed(1)

##### processing  of data

######## remove duplicates from forum dictionaries
rb_dictionary = rb_dictionary[!duplicated(rb_dictionary$word),]
andrey_dictionary = andrey_dictionary[!duplicated(andrey_dictionary$word),]
matveev_dictionary = matveev_dictionary[!duplicated(matveev_dictionary$word),]
silogram_dictionary= silogram_dictionary[!duplicated(silogram_dictionary$word),]
yang_dictionary = yang_dictionary[!duplicated(yang_dictionary$word),]
rb_replacements = rb_replacements[!duplicated(rb_replacements$word),]


########## process train and test data

labels = train[,c('id','relevance', 'rel1', 'rel2', 'rel3')]
train = train[,!colnames(train) %in% c('relevance', 'rel1', 'rel2', 'rel3')]

#pull out brand from attributes data
brand <- attrib[attrib$name == "MFG Brand Name",] 
brand$name <- NULL
colnames(brand)[colnames(brand) =='value'] <- 'brand'

# collapse attributes name and value into text strings per product id
attr_name <-  aggregate(name~product_uid,paste,collapse=" ",data=attrib)
attr_value <-  aggregate(value~product_uid,paste,collapse=" ",data=attrib)

# combine train and test
alldata = rbind(train, test)

# merge description and brand data with alldata
alldata$sortid <- 1:nrow(alldata)
alldata <- merge(alldata, desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
alldata <- merge(alldata, brand, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
alldata <- merge(alldata, attr_name, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
alldata <- merge(alldata, attr_value, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
alldata <- alldata[order(alldata$sortid), ]
alldata$sortid <- NULL
names(alldata)[names(alldata)=="name"] <- "attr_name"
names(alldata)[names(alldata)=="value"] <- "attr_value"

rm(attr_name, attr_value, attrib, desc) # remove large variables to save space


####### replace missing brands  ################
id <- alldata$id %in% missing_brands$id
alldata$brand[id] <- missing_brands$brand 


############spell check google dictionary from forum ##################

replace_word <-  function(x) {
  x_new = as.character(spell_check_google[which(spell_check_google$V1==x),2])
  return(x_new)
}

index = alldata$search_term %in% spell_check_google$V1
alldata$search_term[index] = sapply(alldata$search_term[index], replace_word)




########## rb-dictionary vectorized spell replacement in search term

pattern = unlist(rb_dictionary[,-1])
names(pattern)  = stri_replace_all(rb_dictionary$word, "(?i)", regex = "^") 
names(pattern)  = stri_replace_all(names(pattern), "\\\\b", regex = "^(\\s+)*|(\\s+)*$") 
alldata$search_term = str_replace_all(alldata$search_term, pattern )



################## vectorized general replacements/corrections rb-replacements 

pattern = unlist(rb_replacements[,-1])
names(pattern)  = stri_replace_all(rb_replacements$word, "(?i)", regex = "^") 
names(pattern)  = stri_replace_all(names(pattern), "\\\\b", regex = "^(\\s+)*|(\\s+)*$") 

alldata$search_term = str_replace_all(alldata$search_term, pattern )
alldata$product_title = str_replace_all(alldata$product_title, pattern )
alldata$product_description = str_replace_all(alldata$product_description, pattern )
alldata$brand = str_replace_all(alldata$brand, pattern )
alldata$attr_name = str_replace_all(alldata$attr_name, pattern )
alldata$attr_value = str_replace_all(alldata$attr_value, pattern )



################################################


########### add search analogs columns derived from rword2vec iterations (calculated elsewhere) ############

alldata$search_analogs_stem = search_analogs$search_analogs_stem
alldata$search_analogs_stem1 = search_analogs$search_analogs_stem1


alldata[is.na(alldata)] <- -999


########### functions and related constants ########################
#stop_w = c('xbi', 'th','sku','less','er','ing', 'and', 'for', 'in', 'on', 'th', 'sku', 'with', 'what', 'from', 'that')
stop_w = c("xbi" )
#stop_w = c("" )
strnum <- c('zero','one','two','three','four','five','six','seven','eight','nine' )

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

numbers_to_text <- function(x) { return(strnum[x+1]) }



######################## clean brand term ###################################
brand_stem <- function(s) { 
  s = tolower(s)
  s = gsub("([0-9]+)( *)(gauge )", "\\1gauge ", s)
  s = gsub("-", " ", s)
  s = gsub(".n/a", "", s)
  s = gsub("n/a", "", s)
  s = gsub(" & ", " and ", s)
  s = gsub("&", " and ", s)
  s = gsub("'", "", s)
  s = gsub("/", " ", s)
  s = gsub("b.b.begonia", "b.b. begonia", s)  # the only special case for a dot
  s = gsub("\\.", "", s)  # the only special case for a dot
  s = gsub("!", "", s)
  s = gsub(":", "", s)
  s = gsub(",", "", s)
  s = gsub("\\+", " and ", s)
  
  review_text <- as.data.frame(sapply(s,function(row) iconv(row, "latin1", "ASCII", sub="")) )
  corpus = Corpus(VectorSource(review_text))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"),stop_w)  )
  corpus <- tm_map(corpus, stripWhitespace)
  dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")),  stringsAsFactors=F)
  colnames(dataframe) = "stem"
  return(dataframe)
}

alldata = cbind(alldata, brand_stem(alldata$brand), stringsAsFactors=F)
names(alldata)[names(alldata) == 'stem'] <- 'brand_stem'



############################### clean search term #################################
search_stem <- function(s) { 
  s = tolower(s)
  s = gsub("\\."," \\. ", s)
  s = gsub("([0-9])( *)/( *)([0-9])", "\\1\\4 ", s)
  s = gsub("/", " ", s)
  
  #### new additions apr5 #######
  s = gsub("([0-9]+[[:punct:]]*\\s*)(by)(?=\\s*[0-9]+)", "\\1 x ", s, perl = TRUE)
  s = gsub("([0-9]+[[:punct:]]*\\s*)(\\*)(?=\\s*[0-9]+)", "\\1 x ", s, perl = TRUE)
  s = gsub("([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*)", "\\1inch \\1foot \\3inch \\3foot \\5inch \\5foot ", s, perl = TRUE)
  s = gsub("([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*)", "\\1inch \\1foot \\3inch \\3foot ", s, perl = TRUE)
  
  ########################
  
  s = gsub("([0-9]+)( *)(ga |-ga )", "\\1gauge ", s)
  s = gsub("-", " ", s)
  s = gsub(" & ", " and ", s)
  s = gsub("&", " and ", s)
  
  s = gsub("(\\.|/)$", "", s)
  s = gsub("([0-9])([a-z])", "\\1 \\2", s)
  s = gsub("([a-z])([0-9])", "\\1 \\2", s)
  s = gsub("([a-z])( *)\\.( *)([ a-z])", "\\1 \\4", s)
  
  s = gsub(" inx ", " in x ", s) # inx = in x = inches into conversion
  s = gsub("([0-9]+)( *)(inches|inch|in |in\\b|in\\.|')\\.?", "\\1inch ", s)
  s = gsub("([0-9]+)( *)(foot|feet|ft |ft\\b|ft\\.|'')\\.?", "\\1foot ", s)
  s = gsub("([0-9]+)( *)(pounds|pound|lbs |lbs\\b|lbs\\.|lb |lb\\b|lb\\.)\\.?", "\\1pound ", s)
  s = gsub("([0-9]+)( *)(square|sq |sq\\.) ?\\.?(feet|foot|ft |ft\\b|ft\\.)\\.?", "\\1sqft ", s)
  s = gsub("([0-9]+)( *)(cubic|cu |cu\\.) ?\\.?(feet|foot|ft |ft\\b|ft\\.)\\.?", "\\1cuft ", s)
  s = gsub("([0-9]+)( *)(gallons|gallon|gal |gal\\b|gal\\.)\\.?", "\\1gallon ", s)
  s = gsub("([0-9]+)( *)(ounces|ounce|oz |oz\\b|oz\\.)\\.?", "\\1ounce ", s)
  s = gsub("([0-9]+)( *)(centimeters|cm |cm\\b|cm\\.)\\.?", "\\1centimeter ", s)
  s = gsub("([0-9]+)( *)(milimeters|millimeters|mm |mm\\b|mm\\.)\\.?", "\\1millimeter ", s)
  s = gsub("°"," degrees ", s)
  s = gsub("([0-9]+)( *)(degrees|degree)\\.?", "\\1degree ", s)
  s = gsub(" v "," volts ", s)
  s = gsub("([0-9]+)( *)(volts|volt)\\.?", "\\1volt. ", s)
  s = gsub("([0-9]+)( *)(watts|watt|w )\\.?", "\\1watt. ", s)
  s = gsub("([0-9]+)( *)(amperes|ampere|amps|a |amp |amp\\b|amp\\.)\\.?", "\\1ampere ", s)
  
  s = gsub("\\bx\\b"," xbi ", s)
  s = gsub("\\*", " xbi ", s)
  s = gsub("\\bby\\b ", " xbi ", s)
  
  s = gsub("'", "", s) # take out remaining aposthropes after the feet/inches conversion above
  s = gsub("\\.", " ", s) # take out remaining dots
  s = gsub("\\?", " ", s) # take out ?
   #######new addition apr5
  s = gsub("([0-9]+)(inch|foot|pound|sqft|cuft|gallon|ounce|centimeter|millimeter|degree|volt|watt|ampere)", "\\1 \\1\\2", s)
   
  
  review_text <- as.data.frame(sapply(s,function(row) iconv(row, "latin1", "ASCII", sub="")) )
  corpus = Corpus(VectorSource(review_text))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"),stop_w)  )
  corpus <- tm_map(corpus, stripWhitespace)
  dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")),  stringsAsFactors=F)
  colnames(dataframe) = "stem"
  return(dataframe)
}

alldata = cbind(alldata, search_stem(alldata$search_term ), stringsAsFactors=F)
names(alldata)[names(alldata) == 'stem'] <- 'search_term_stem'


############################### clean description term #################################
desc_stem <- function(s) { 

  s = gsub("([a-z])([A-Z])", "\\1 \\2", s) # this is special for description to separate joined sentences. Do this before tolower
  s = tolower(s)
  s = gsub("([0-9]+)( *)(gauge |-gauge )", "\\1gauge ", s)
  s = gsub("-", " ", s)
  s = gsub("&nbsp", " ", s)
  s = gsub("<br", " ", s)
  s = gsub(" & ", " and ", s)
  s = gsub("&", " and ", s)
  s = gsub("\\+", " and ", s)
  s = gsub("\\."," \\. ", s)
  s = gsub("([0-9])( *)/( *)([0-9])", "\\1\\4 ", s)
  s = gsub("/", " ", s)
  
  #### new additions apr5 #######
  s = gsub("([0-9]+[[:punct:]]*\\s*)(by)(?=\\s*[0-9]+)", "\\1 x ", s, perl = TRUE)
  s = gsub("([0-9]+[[:punct:]]*\\s*)(\\*)(?=\\s*[0-9]+)", "\\1 x ", s, perl = TRUE)
  s = gsub("([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*)", "\\1inch \\1foot \\3inch \\3foot \\5inch \\5foot ", s, perl = TRUE)
  s = gsub("([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*)", "\\1inch \\1foot \\3inch \\3foot ", s, perl = TRUE)
  
  ########################
  
  s = gsub("(\\.|/)$", "", s)
  s = gsub("([0-9])([a-z])", "\\1 \\2", s)
  s = gsub("([a-z])([0-9])", "\\1 \\2", s)
  s = gsub("([a-z])( *)\\.( *)([ a-z])", "\\1 \\4", s)
  s = gsub(" inx ", " in x ", s) # inx = in x = inches into conversion
  s = gsub("([0-9]+)( *)(inches|inch|in |in\\b|in\\.|')\\.?", "\\1inch ", s)
  s = gsub("([0-9]+)( *)(foot|feet|ft |ft\\b|ft\\.|'')\\.?", "\\1foot ", s)
  s = gsub("([0-9]+)( *)(pounds|pound|lbs |lbs\\b|lbs\\.|lb |lb\\b|lb\\.)\\.?", "\\1pound ", s)
  s = gsub("([0-9]+)( *)(square|sq |sq\\.) ?\\.?(feet|foot|ft |ft\\b|ft\\.)\\.?", "\\1sqft ", s)
  s = gsub("([0-9]+)( *)(cubic|cu |cu\\.) ?\\.?(feet|foot|ft |ft\\b|ft\\.)\\.?", "\\1cuft ", s)
  s = gsub("([0-9]+)( *)(gallons|gallon|gal |gal\\b|gal\\.)\\.?", "\\1gallon ", s)
  s = gsub("([0-9]+)( *)(ounces|ounce|oz |oz\\b|oz\\.)\\.?", "\\1ounce ", s)
  s = gsub("([0-9]+)( *)(centimeters|cm |cm\\b|cm\\.)\\.?", "\\1centimeter ", s)
  s = gsub("([0-9]+)( *)(milimeters|millimeters|mm |mm\\b|mm\\.)\\.?", "\\1millimeter ", s)
  s = gsub("°"," degrees ", s)
  s = gsub("([0-9]+)( *)(degrees|degree)\\.?", "\\1degree ", s)
  s = gsub(" v "," volts ", s)
  s = gsub("([0-9]+)( *)(volts|volt)\\.?", "\\1volt. ", s)
  s = gsub("([0-9]+)( *)(watts|watt|w )\\.?", "\\1watt. ", s)
  s = gsub("([0-9]+)( *)(amperes|ampere|amps|a |amp |amp\\b|amp\\.)\\.?", "\\1ampere ", s)
  s = gsub("\\bx\\b"," xbi ", s)
  s = gsub("\\*", " xbi ", s)
  s = gsub("\\bby\\b", " xbi ", s)
  s = gsub("'", "", s) # take out remaining aposthropes after the feet/inches conversion above
  s = gsub("\\.", " ", s) # take out remaining dots
  s = gsub("[^[:alnum:]]", " ", s) 
 ####### new addition apr5
  s = gsub("([0-9]+)(inch|foot|pound|sqft|cuft|gallon|ounce|centimeter|millimeter|degree|volt|watt|ampere)", "\\1 \\1\\2", s)

  
  review_text <- as.data.frame(sapply(s,function(row) iconv(row, "latin1", "ASCII", sub="")) )
  corpus = Corpus(VectorSource(review_text))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"),stop_w)  )
  corpus <- tm_map(corpus, stripWhitespace)
  dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")),  stringsAsFactors=F)
  colnames(dataframe) = "stem"
  return(dataframe)
}

alldata = cbind(alldata, desc_stem(alldata$product_description ), stringsAsFactors=F)
names(alldata)[names(alldata) == 'stem'] <- 'product_description_stem'



############################### clean attr name columns #################################
attrname_stem <- function(s) { 
  
  s <- sapply(s,function(row) iconv(row, "latin1", "ASCII", sub="")) 
  s = tolower(s)
  s = gsub("([0-9]+)( *)(gauge |-gauge )", "\\1gauge ", s)
  s = gsub("-", " ", s)
  s = gsub("&nbsp", " ", s)
  s = gsub("<br", " ", s)
  s = gsub(" & ", " and ", s)
  s = gsub("&", " and ", s)
  s = gsub("bullet[0-9]+", " ", s)
  s = gsub("\\+", " and ", s)
  s = gsub("\\."," \\. ", s)
  s = gsub("([0-9])( *)/( *)([0-9])", "\\1\\4 ", s)
  s = gsub("/", " ", s)
  
  #### new additions apr5 #######
  s = gsub("([0-9]+[[:punct:]]*\\s*)(by)(?=\\s*[0-9]+)", "\\1 x ", s, perl = TRUE)
  s = gsub("([0-9]+[[:punct:]]*\\s*)(\\*)(?=\\s*[0-9]+)", "\\1 x ", s, perl = TRUE)
  s = gsub("([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*)", "\\1inch \\1foot \\3inch \\3foot \\5inch \\5foot ", s, perl = TRUE)
  s = gsub("([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*)", "\\1inch \\1foot \\3inch \\3foot ", s, perl = TRUE)
  
  ########################
  
  s = gsub("(\\.|/)$", "", s)
  s = gsub("([0-9])([a-z])", "\\1 \\2", s)
  s = gsub("([a-z])([0-9])", "\\1 \\2", s)
  s = gsub("([a-z])( *)\\.( *)([ a-z])", "\\1 \\4", s)
  s = gsub(" inx ", " in x ", s) # inx = in x = inches into conversion
  s = gsub("([0-9]+)( *)(inches|inch|in |in\\b|in\\.|')\\.?", "\\1inch ", s)
  s = gsub("([0-9]+)( *)(foot|feet|ft |ft\\b|ft\\.|'')\\.?", "\\1foot ", s)
  s = gsub("([0-9]+)( *)(pounds|pound|lbs |lbs\\b|lbs\\.|lb |lb\\b|lb\\.)\\.?", "\\1pound ", s)
  s = gsub("([0-9]+)( *)(square|sq |sq\\.) ?\\.?(feet|foot|ft |ft\\b|ft\\.)\\.?", "\\1sqft ", s)
  s = gsub("([0-9]+)( *)(cubic|cu |cu\\.) ?\\.?(feet|foot|ft |ft\\b|ft\\.)\\.?", "\\1cuft ", s)
  s = gsub("([0-9]+)( *)(gallons|gallon|gal |gal\\b|gal\\.)\\.?", "\\1gallon ", s)
  s = gsub("([0-9]+)( *)(ounces|ounce|oz |oz\\b|oz\\.)\\.?", "\\1ounce ", s)
  s = gsub("([0-9]+)( *)(centimeters|cm |cm\\b|cm\\.)\\.?", "\\1centimeter ", s)
  s = gsub("([0-9]+)( *)(milimeters|millimeters|mm |mm\\b|mm\\.)\\.?", "\\1millimeter ", s)
  s = gsub("°"," degrees ", s)
  s = gsub("([0-9]+)( *)(degrees|degree)\\.?", "\\1degree ", s)
  s = gsub(" v "," volts ", s)
  s = gsub("([0-9]+)( *)(volts|volt)\\.?", "\\1volt. ", s)
  s = gsub("([0-9]+)( *)(watts|watt|w )\\.?", "\\1watt. ", s)
  s = gsub("([0-9]+)( *)(amperes|ampere|amps|a |amp |amp\\b|amp\\.)\\.?", "\\1ampere ", s)
  s = gsub("\\bx\\b"," xbi ", s)
  s = gsub("\\*", " xbi ", s)
  s = gsub("\\bby\\b", " xbi ", s)
  s = gsub("'", "", s) # take out remaining aposthropes after the feet/inches conversion above
  s = gsub("\\.", " ", s) # take out remaining dots
  s = gsub("[^[:alnum:]]", " ", s) 
  #######new addition apr5
  s = gsub("([0-9]+)(inch|foot|pound|sqft|cuft|gallon|ounce|centimeter|millimeter|degree|volt|watt|ampere)", "\\1 \\1\\2", s)
  
  review_text <- as.data.frame(sapply(s,function(row) iconv(row, "latin1", "ASCII", sub="")) )
  corpus = Corpus(VectorSource(review_text))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"),stop_w)  )
  corpus <- tm_map(corpus, stripWhitespace)
  dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")),  stringsAsFactors=F)
  colnames(dataframe) = "stem"
  return(dataframe)
}

alldata = cbind(alldata, attrname_stem(alldata$attr_name ), stringsAsFactors=F)
names(alldata)[names(alldata) == 'stem'] <- 'attr_name_stem'


############################### clean product title columns #################################
prod_title_stem <- function(s) { 
  
  s <- sapply(s,function(row) iconv(row, "latin1", "ASCII", sub="")) 
  s = gsub("([0-9]+)( *)(gauge |Gauge |-gauge |-Gauge )", "\\1gauge ", s)
  s = gsub("-", " ", s)
  s = gsub("&nbsp", " ", s)
  s = gsub("<br", " ", s)
  s = gsub(" & ", " and ", s)
  s = gsub("&", " and ", s)
  s = gsub("\\+", " and ", s)
  s = gsub("\\."," \\. ", s)
  s = gsub("([0-9])( *)/( *)([0-9])", "\\1\\4 ", s)
  s = gsub("/", " ", s)
  
  #### new additions apr5 #######
  s = gsub("([0-9]+[[:punct:]]*\\s*)(by)(?=\\s*[0-9]+)", "\\1 x ", s, perl = TRUE)
  s = gsub("([0-9]+[[:punct:]]*\\s*)(\\*)(?=\\s*[0-9]+)", "\\1 x ", s, perl = TRUE)
  s = gsub("([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*)", "\\1inch \\1foot \\3inch \\3foot \\5inch \\5foot ", s, perl = TRUE)
  s = gsub("([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*)", "\\1inch \\1foot \\3inch \\3foot ", s, perl = TRUE)
  
  ########################
  
  s = gsub("(\\b[A-Z]\\w*)(\\s+)(?=\\b[A-Z]\\w*)" , "\\1 _ ", s, perl = TRUE) #\\s+\\w[A-Z]+\\w
  s = tolower(s)
  s = gsub("(\\.|/)$", "", s)
  s = gsub("([0-9])([a-z])", "\\1 \\2", s)
  s = gsub("([a-z])([0-9])", "\\1 \\2", s)
  s = gsub("([a-z])( *)\\.( *)([a-z])", "\\1 \\4", s)
  s = gsub(" inx ", " in x ", s) # inx = in x = inches into conversion
  s = gsub("([0-9]+)( *)(inches|inch|in |in\\b|in\\.|')\\.?", "\\1inch ", s)
  s = gsub("([0-9]+)( *)(foot|feet|ft |ft\\b|ft\\.|'')\\.?", "\\1foot ", s)
  s = gsub("([0-9]+)( *)(pounds|pound|lbs |lbs\\b|lbs\\.|lb |lb\\b|lb\\.)\\.?", "\\1pound ", s)
  s = gsub("([0-9]+)( *)(square|sq |sq\\.) ?\\.?(feet|foot|ft |ft\\b|ft\\.)\\.?", "\\1sqft ", s)
  s = gsub("([0-9]+)( *)(cubic|cu |cu\\.) ?\\.?(feet|foot|ft |ft\\b|ft\\.)\\.?", "\\1cuft ", s)
  s = gsub("([0-9]+)( *)(gallons|gallon|gal |gal\\b|gal\\.)\\.?", "\\1gallon ", s)
  s = gsub("([0-9]+)( *)(ounces|ounce|oz |oz\\b|oz\\.)\\.?", "\\1ounce ", s)
  s = gsub("([0-9]+)( *)(centimeters|cm |cm\\b|cm\\.)\\.?", "\\1centimeter ", s)
  s = gsub("([0-9]+)( *)(milimeters|millimeters|mm |mm\\b|mm\\.)\\.?", "\\1millimeter ", s)
  s = gsub("°"," degrees ", s)
  s = gsub("([0-9]+)( *)(degrees|degree)\\.?", "\\1degree ", s)
  s = gsub(" v "," volts ", s)
  s = gsub("([0-9]+)( *)(volts|volt)\\.?", "\\1volt. ", s)
  s = gsub("([0-9]+)( *)(watts|watt|w )\\.?", "\\1watt. ", s)
  s = gsub("([0-9]+)( *)(amperes|ampere|amps|a |amp |amp\\b|amp\\.)\\.?", "\\1ampere ", s)
  s = gsub("\\bx\\b"," xbi ", s)
  s = gsub("\\*", " xbi ", s)
  s = gsub("\\bby\\b", " xbi ", s)
  s = gsub("'", "", s) # take out remaining aposthropes after the feet/inches conversion above
  s = gsub("\\.", " ", s) # take out remaining dots
  s = gsub("[^_[:alnum:]]", " ", s, perl=TRUE) 
  ####### new addition apr5
  s = gsub("([0-9]+)(inch|foot|pound|sqft|cuft|gallon|ounce|centimeter|millimeter|degree|volt|watt|ampere)", "\\1 \\1\\2", s)
  
  review_text <- as.data.frame(sapply(s,function(row) iconv(row, "latin1", "ASCII", sub="")) )
  corpus = Corpus(VectorSource(review_text))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"),stop_w)  )
  corpus <- tm_map(corpus, stripWhitespace)
  dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")),  stringsAsFactors=F)
  colnames(dataframe) = "stem"
  return(dataframe)
}

alldata = cbind(alldata, prod_title_stem(alldata$product_title ), stringsAsFactors=F)
names(alldata)[names(alldata) == 'stem'] <- 'product_title_stem'


############################### clean other columns #################################
other_stem <- function(s) { 
  
  s <- sapply(s,function(row) iconv(row, "latin1", "ASCII", sub="")) 
  s = tolower(s)
  s = gsub("([0-9]+)( *)(gauge |-gauge )", "\\1gauge ", s)
  s = gsub("-", " ", s)
  s = gsub("&nbsp", " ", s)
  s = gsub("<br", " ", s)
  s = gsub(" & ", " and ", s)
  s = gsub("&", " and ", s)
  s = gsub("\\+", " and ", s)
  s = gsub("\\."," \\. ", s)
  s = gsub("([0-9])( *)/( *)([0-9])", "\\1\\4 ", s)
  s = gsub("/", " ", s)
  
  #### new additions apr5 #######
  s = gsub("([0-9]+[[:punct:]]*\\s*)(by)(?=\\s*[0-9]+)", "\\1 x ", s, perl = TRUE)
  s = gsub("([0-9]+[[:punct:]]*\\s*)(\\*)(?=\\s*[0-9]+)", "\\1 x ", s, perl = TRUE)
  s = gsub("([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*)", "\\1inch \\1foot \\3inch \\3foot \\5inch \\5foot ", s, perl = TRUE)
  s = gsub("([0-9]+)([[:punct:]]*\\s*x\\s*)([0-9]+)([[:punct:]]*)", "\\1inch \\1foot \\3inch \\3foot ", s, perl = TRUE)
  
  ########################

  s = gsub("(\\.|/)$", "", s)
  s = gsub("([0-9])([a-z])", "\\1 \\2", s)
  s = gsub("([a-z])([0-9])", "\\1 \\2", s)
  s = gsub("([a-z])( *)\\.( *)([ a-z])", "\\1 \\4", s)
  s = gsub(" inx ", " in x ", s) # inx = in x = inches into conversion
  s = gsub("([0-9]+)( *)(inches|inch|in |in\\b|in\\.|')\\.?", "\\1inch ", s)
  s = gsub("([0-9]+)( *)(foot|feet|ft |ft\\b|ft\\.|'')\\.?", "\\1foot ", s)
  s = gsub("([0-9]+)( *)(pounds|pound|lbs |lbs\\b|lbs\\.|lb |lb\\b|lb\\.)\\.?", "\\1pound ", s)
  s = gsub("([0-9]+)( *)(square|sq |sq\\.) ?\\.?(feet|foot|ft |ft\\b|ft\\.)\\.?", "\\1sqft ", s)
  s = gsub("([0-9]+)( *)(cubic|cu |cu\\.) ?\\.?(feet|foot|ft |ft\\b|ft\\.)\\.?", "\\1cuft ", s)
  s = gsub("([0-9]+)( *)(gallons|gallon|gal |gal\\b|gal\\.)\\.?", "\\1gallon ", s)
  s = gsub("([0-9]+)( *)(ounces|ounce|oz |oz\\b|oz\\.)\\.?", "\\1ounce ", s)
  s = gsub("([0-9]+)( *)(centimeters|cm |cm\\b|cm\\.)\\.?", "\\1centimeter ", s)
  s = gsub("([0-9]+)( *)(milimeters|millimeters|mm |mm\\b|mm\\.)\\.?", "\\1millimeter ", s)
  s = gsub("°"," degrees ", s)
  s = gsub("([0-9]+)( *)(degrees|degree)\\.?", "\\1degree ", s)
  s = gsub(" v "," volts ", s)
  s = gsub("([0-9]+)( *)(volts|volt)\\.?", "\\1volt. ", s)
  s = gsub("([0-9]+)( *)(watts|watt|w )\\.?", "\\1watt. ", s)
  s = gsub("([0-9]+)( *)(amperes|ampere|amps|a |amp |amp\\b|amp\\.)\\.?", "\\1ampere ", s)
  s = gsub("\\bx\\b"," xbi ", s)
  s = gsub("\\*", " xbi ", s)
  s = gsub("\\bby\\b", " xbi ", s)
  s = gsub("'", "", s) # take out remaining aposthropes after the feet/inches conversion above
  s = gsub("\\.", " ", s) # take out remaining dots
  s = gsub("[^[:alnum:]]", " ", s) 
  ####### new addition apr5
  s = gsub("([0-9]+)(inch|foot|pound|sqft|cuft|gallon|ounce|centimeter|millimeter|degree|volt|watt|ampere)", "\\1 \\1\\2", s)
  
  review_text <- as.data.frame(sapply(s,function(row) iconv(row, "latin1", "ASCII", sub="")) )
  corpus = Corpus(VectorSource(review_text))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"),stop_w)  )
  corpus <- tm_map(corpus, stripWhitespace)
  dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")),  stringsAsFactors=F)
  colnames(dataframe) = "stem"
  return(dataframe)
}


alldata = cbind(alldata, other_stem(alldata$attr_value ), stringsAsFactors=F)
names(alldata)[names(alldata) == 'stem'] <- 'attr_value_stem'



############ other spell check corrections #######################
rm(attr)
gc()

stem_column <- function(s) { 
  
  corpus = Corpus(VectorSource(s))
  corpus.copy = corpus
  corpus <- tm_map(corpus, stemDocument)
  dataframe<-data.frame(unlist(sapply(corpus, `[`, "content")),  stringsAsFactors=F)

}

# brand, title, description dictionary
dictA = cbind(alldata$brand,alldata$product_title_stem, alldata$product_description_stem)
dictA1 = bag_o_words(dictA)

# attributes dictionary
dictB = cbind(alldata$attr_name_stem, alldata$attr_value_stem)
dictB1 = bag_o_words(dictB)

#combined dictionary - the dictionaries were split earlier due to memory issues
alldict = c(dictA1, dictB1)
rm(dictA)
rm(dictB)
rm(dictA1)
rm(dictB1)
gc()
alldict_freq = as.matrix(table(alldict)) # freq table
colnames(alldict_freq) = "freq"
alldict = as.vector(rownames(alldict_freq)) # final dictionary

alldict_freq_stem = alldict_freq
alldict_freq_stem = cbind(alldict_freq_stem, as.character(rownames(alldict_freq_stem))  )
alldict_freq_stem = cbind(alldict_freq_stem, stem_column(alldict_freq_stem[,2]))
alldict_freq_stem1 = alldict_freq_stem[,c(1,3)]
colnames(alldict_freq_stem1)[1] = "freq"
colnames(alldict_freq_stem1)[2] = "searchwords"
alldict_freq_stem1$freq = as.numeric(alldict_freq_stem1$freq)
alldict_freq_stem2 = ddply(alldict_freq_stem1, .(searchwords), summarize, freq=sum(freq))
rownames(alldict_freq_stem2) = alldict_freq_stem2$searchwords
alldict_freq_stem2$searchwords <- NULL
alldict_stem1 = as.vector(rownames(alldict_freq_stem2))

dict_search = bag_o_words(alldata$search_term_stem)
search_freq = as.matrix(table(dict_search))
dict_search = unique(dict_search)

########################### vectorized spelling replacement from forum list owl1 ####################
pattern = unlist(spelling[,-1])
names(pattern)  = stri_replace_all(spelling$word, "\\\\b", regex = "^(\\s+)*|(\\s+)*$") 
alldata$search_term_stem = str_replace_all(alldata$search_term_stem, pattern)


alldata[is.na(alldata)] <- -999

write_csv(alldata,"C:/R/homedepot/data/alldata1.csv")
#alldata <- read_csv('C:/R/homedepot/data/alldata1.csv')

############################################ stemming functions ######################

stem_column <- function(s) { 
  
  corpus = Corpus(VectorSource(s))
  corpus.copy = corpus
  corpus <- tm_map(corpus, stemDocument)
  dataframe<-data.frame(unlist(sapply(corpus, `[`, "content")),  stringsAsFactors=F)

}

alldata = cbind(alldata, stem_column(alldata$brand_stem ), stringsAsFactors=F)
colnames(alldata)[ncol(alldata)] <- "brand_stem1"

alldata = cbind(alldata, stem_column(alldata$search_term_stem ), stringsAsFactors=F)
colnames(alldata)[ncol(alldata)] <-  'search_term_stem1'

alldata = cbind(alldata, stem_column(alldata$product_description_stem ), stringsAsFactors=F)
colnames(alldata)[ncol(alldata)] <- 'product_description_stem1'

alldata = cbind(alldata, stem_column(alldata$product_title_stem ), stringsAsFactors=F)
colnames(alldata)[ncol(alldata)] <- 'product_title_stem1'

alldata = cbind(alldata, stem_column(alldata$attr_name_stem ), stringsAsFactors=F)
colnames(alldata)[ncol(alldata)] <- 'attr_name_stem1'

alldata = cbind(alldata, stem_column(alldata$attr_value_stem ), stringsAsFactors=F)
colnames(alldata)[ncol(alldata)] <- 'attr_value_stem1'


# ########### separate search text and search units ##################

separate_num <- function(x) {
  searchstring = array("",2)
  wordarray = unlist(strsplit(x, "\\s+"))
  index = grepl("[0-9]",wordarray)
  searchstring[1] = paste(wordarray[index], sep = " ", collapse = " ")
  searchstring[2] = paste(wordarray[!index], sep = " ", collapse = " ")
  return(searchstring)
}
split_search = t(sapply(alldata$search_term_stem1, function(x) separate_num(x) ) )
alldata$search_num = split_search[,1]
alldata$search_text = split_search[,2]

rm(split_search)

write_csv(alldata,"C:/R/homedepot/data/alldata2.csv")
#alldata <- read_csv('C:/R/homedepot/data/alldata2.csv')
