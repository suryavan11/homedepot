## this file has a restart at around line 364 due to memory requirements (i7 8gb ram). 
## The code after restart has to be run separately

############## feature generation
set.seed(1)


######################### tf idf transformations for all data ##########################
cosine_dist <- function(a,b) {
  a = t(a) # transpose of search array. b is the text array
  gc()
  num <- rowSums(a*t(b)) #equivalent to diag(x %*% y) - cross-product of vectors (numerator)
  srss1 <- sqrt(rowSums(a^2)) # square root of square sum of each vector (used for denominator)
  srss2 <- sqrt(rowSums(t(b)^2))
  den <- srss1 * srss2 # denominator
  return(num/den) # cosine similarity   sdiv(num, den)       
}

#### not used
pearson_dist <- function(a,b) { 
  sa = colSums(a)
  sb = colSums(b)
  sab = colSums(a*b)
  sa2 = colSums(a^2)
  sb2 = colSums(b^2)
  n = length(a[,1])
  pearson = (n*sab-sa*sb)/sqrt((n*sa2-sa^2)*(n*sb2-sb^2))
}




svdscales_cosine_dist <- function(a,b, scales) {
  a = sqrt(scales)*a
  b = sqrt(scales)*b
  a = t(a) # transpose of search array. b is the text array
  gc()
  num <- rowSums(a*t(b)) #equivalent to diag(x %*% y) - cross-product of vectors (numerator)
  srss1 <- sqrt(rowSums(a^2)) # square root of square sum of each vector (used for denominator)
  srss2 <- sqrt(rowSums(t(b)^2))
  den <- srss1 * srss2 # denominator
  return(num/den) # cosine similarity   sdiv(num, den)       
}


cosine_transformation <- function(text, search) {
 
  combined = text 
  combined1 = as.data.frame(combined)
  srch_temp = as.data.frame(search)
  
  colnames(srch_temp) = "combined"
  combined2 = rbind(combined1, srch_temp)
  combined2$counter = as.factor(1:length(combined2$combined))
  combined2$combined = as.character(combined2$combined)
  
  gc()
  tdm = dfm(combined2$combined)
  gc()
  
  tdm = convert(tdm, to = "tm" )
  tdm = as.TermDocumentMatrix(tdm)
  tdm = weightTfIdf(tdm, normalize = FALSE)
  
  tdm_sparse <-  sparseMatrix(i=tdm$i, j=tdm$j, x=tdm$v, dims=c(tdm$nrow, tdm$ncol), dimnames = (tdm$dimnames))
  tdm_text = tdm_sparse[,1:240760]
  tdm_search = tdm_sparse[,240761:481520]
  
  cosine_distance =  cosine_dist(tdm_search, tdm_text)
  print("cosine_distance calculation complete")
  
  return (as.data.frame(cosine_distance))
  
}


transformations <- function(text, search, svd_kval) {
  

  combined = text 
  combined1 = as.data.frame(combined)
  srch_temp = as.data.frame(search)
  
  colnames(srch_temp) = "combined"
  combined2 = rbind(combined1, srch_temp)
  combined2$counter = as.factor(1:length(combined2$combined))
  combined2$combined = as.character(combined2$combined)
  
  gc()
  tdm = dfm(combined2$combined)
  gc()
  
  tdm = convert(tdm, to = "tm" )
  tdm = as.TermDocumentMatrix(tdm)
  tdm = weightTfIdf(tdm, normalize = FALSE)
  
  tdm_sparse <-  sparseMatrix(i=tdm$i, j=tdm$j, x=tdm$v, dims=c(tdm$nrow, tdm$ncol), dimnames = (tdm$dimnames))
  tdm_text = tdm_sparse[,1:240760]
  tdm_search = tdm_sparse[,240761:481520]
  
  cosine_distance =  cosine_dist(tdm_search, tdm_text)
  print("cosine_distance calculation complete")
  
  
  find_centroid <- function(x) {
    centroid = colSums(df_svd[rn %in% unlist(strsplit(x, "\\s+")), -c("rn"),with=FALSE])
  }
  
  tdm_text_svd = svds(tdm_text, k = svd_kval )
  rownames(tdm_text_svd$u)= rownames(tdm_text)
  df_svd = as.data.table(tdm_text_svd$u, keep.rownames = TRUE)
  centroid = sapply(search, find_centroid)
  
  svd_dotproduct = rowSums(t(centroid) * tdm_text_svd$v * tdm_text_svd$d)
  print("svd_dotproduct calculation complete")
  
  #svd_cosinedist = svdscales_cosine_dist(centroid, t(tdm_text_svd$v), tdm_text_svd$d ) # did not improve rsme, slightly increased it
  
  
  return (as.data.frame(cbind(cosine_distance, svd_dotproduct)))
  
}

k = 50


index = sapply(alldata$search_term_stem1, function(x) length(unlist(strsplit(x, "\\s+"))))

text = str_c(alldata$product_title_stem1, alldata$product_description_stem1, alldata$brand_stem1, alldata$attr_name_stem1, alldata$attr_value_stem1, sep = " " )
temp = transformations(text, alldata$search_term_stem1, svd_kval = k)
alldata$cosine_dist = temp$cosine_distance
alldata$svd5dot = temp$svd_dotproduct

alldata$svd5dot = alldata$svd5dot/index

temp = transformations(alldata$product_title_stem1, alldata$search_term_stem1, svd_kval = k)
alldata$cosine_dist_title = temp$cosine_distance
alldata$svd5dot_title = temp$svd_dotproduct

alldata$svd5dot_title = alldata$svd5dot_title/index


temp = transformations(alldata$product_description_stem1, alldata$search_term_stem1, svd_kval = k)
alldata$cosine_dist_desc = temp$cosine_distance
alldata$svd5dot_desc = temp$svd_dotproduct

alldata$svd5dot_desc = alldata$svd5dot_desc/index


temp = transformations(alldata$attr_name_stem1, alldata$search_term_stem1, svd_kval = k)
alldata$cosine_dist_attrname = temp$cosine_distance
alldata$svd5dot_attrname = temp$svd_dotproduct

alldata$svd5dot_attrname = alldata$svd5dot_attrname/index


temp = transformations(alldata$attr_value_stem1, alldata$search_term_stem1, svd_kval = k)
alldata$cosine_dist_attrval = temp$cosine_distance
alldata$svd5dot_attrval = temp$svd_dotproduct

alldata$svd5dot_attrval = alldata$svd5dot_attrval/index


temp = transformations(alldata$brand_stem1, alldata$search_term_stem1, svd_kval = k)
alldata$cosine_brand_title = temp$cosine_distance
alldata$svd5dot_brand = temp$svd_dotproduct

alldata$svd5dot_brand = alldata$svd5dot_brand/index


temp = transformations(alldata$product_title_stem1, alldata$search_analogs_stem1, svd_kval = k)
alldata$cosine_analogs_title = temp$cosine_distance
alldata$svd5dot_analogs_title = temp$svd_dotproduct

index = sapply(alldata$search_analogs_stem1, function(x) length(unlist(strsplit(x, "\\s+"))))
alldata$svd5dot_analogs_title = alldata$svd5dot_analogs_title/index

temp = transformations(alldata$product_description_stem1, alldata$search_analogs_stem1, svd_kval = k)
alldata$cosine_analogs_desc = temp$cosine_distance
alldata$svd5dot_analogs_desc = temp$svd_dotproduct

alldata$svd5dot_analogs_desc = alldata$svd5dot_analogs_desc/index


temp = transformations(alldata$brand_stem1, alldata$search_analogs_stem1, svd_kval = k)
alldata$cosine_analogs_brand = temp$cosine_distance
alldata$svd5dot_analogs_brand = temp$svd_dotproduct

alldata$svd5dot_analogs_brand = alldata$svd5dot_analogs_brand/index


##### cosine product title with search text and search num
combined = alldata$product_title_stem1 
combined1 = as.data.frame(combined)

srch_temp_num = as.data.frame(alldata$search_num)
colnames(srch_temp_num) = "combined"
srch_temp_txt = as.data.frame(alldata$search_text)
colnames(srch_temp_txt) = "combined"
srch_temp = rbind(srch_temp_num, srch_temp_txt)

colnames(srch_temp) = "combined"
combined2 = rbind(combined1, srch_temp)
combined2$counter = as.factor(1:length(combined2$combined))
combined2$combined = as.character(combined2$combined)

gc()
tdm = dfm(combined2$combined ) # ,ngrams = 1, concatenator = " "
gc()

tdm = convert(tdm, to = "tm" )
tdm = as.TermDocumentMatrix(tdm)
tdm = weightTfIdf(tdm, normalize = FALSE)

tdm_sparse <-  sparseMatrix(i=tdm$i, j=tdm$j, x=tdm$v, dims=c(tdm$nrow, tdm$ncol))
tdm_text = tdm_sparse[,1:240760]
tdm_search_num = tdm_sparse[,240761:481520]
tdm_search_text = tdm_sparse[,481521:722280]
alldata$cosine_dist_title_num =  cosine_dist(tdm_search_num, tdm_text)
alldata$cosine_dist_title_text =  cosine_dist(tdm_search_text, tdm_text)

alldata$cosine_dist_title_num[is.nan(alldata$cosine_dist_title_num) ] = 0
alldata$cosine_dist_title_text[is.nan(alldata$cosine_dist_title_text) ] = 0


################## product description cosine with search text and search num

combined = alldata$product_description_stem1 
combined1 = as.data.frame(combined)

srch_temp_num = as.data.frame(alldata$search_num)
colnames(srch_temp_num) = "combined"
srch_temp_txt = as.data.frame(alldata$search_text)
colnames(srch_temp_txt) = "combined"
srch_temp = rbind(srch_temp_num, srch_temp_txt)

colnames(srch_temp) = "combined"
combined2 = rbind(combined1, srch_temp)
combined2$counter = as.factor(1:length(combined2$combined))
combined2$combined = as.character(combined2$combined)

gc()
tdm = dfm(combined2$combined ) # ,ngrams = 1, concatenator = " "
gc()

tdm = convert(tdm, to = "tm" )
tdm = as.TermDocumentMatrix(tdm)
tdm = weightTfIdf(tdm, normalize = FALSE)
tdm_sparse <-  sparseMatrix(i=tdm$i, j=tdm$j, x=tdm$v, dims=c(tdm$nrow, tdm$ncol))
tdm_text = tdm_sparse[,1:240760]
tdm_search_num = tdm_sparse[,240761:481520]
tdm_search_text = tdm_sparse[,481521:722280]

alldata$cosine_dist_desc_num =  cosine_dist(tdm_search_num, tdm_text)
alldata$cosine_dist_desc_text =  cosine_dist(tdm_search_text, tdm_text)

alldata$cosine_dist_desc_num[is.nan(alldata$cosine_dist_desc_num) ] = 0
alldata$cosine_dist_desc_text[is.nan(alldata$cosine_dist_desc_text) ] = 0

#alldata$pearson_dist_desc = pearson_dist(tdm_search, tdm_text)


############################ wordnet cosines with tdf if #################################

dict_search = bag_o_words(alldata$search_term_stem)
search_freq = as.matrix(table(dict_search))
dict_search = rownames(search_freq)

t = Sys.time()
counter1 <- sapply(dict_search, function(x) synonyms(x, "NOUN"))
Sys.time()-t

counter2 = sapply(counter1, function(x) paste(x, sep = " ", collapse = " "))
counter2 = as.data.frame(counter2)


wordnetsynonyms <- function(x) {
  wordarray = unlist(strsplit(x, "\\s+"))
  combined = paste(counter2[dict_search %in% wordarray,1], sep = " ", collapse = " ")
}

alldata$wordnetsyn = sapply(alldata$search_term_stem, function(x) wordnetsynonyms(x))

alldata = cbind(alldata, search_stem(alldata$wordnetsyn ), stringsAsFactors=F)
names(alldata)[names(alldata) == 'stem'] <- 'wordnetsyn_stem'


alldata = cbind(alldata, stem_column(alldata$wordnetsyn_stem ), stringsAsFactors=F)
colnames(alldata)[ncol(alldata)] <-  'wordnetsyn_stem1'


################## hypernyms and hyponyms from wordnet

hyponyms <- function(x) { 

out <- tryCatch(
  
  {
  filter <- getTermFilter("ExactMatchFilter", x, TRUE)
  temp = getIndexTerms("NOUN", 3, filter)
  synsets = sapply(temp, getSynsets)
  hyper_synsets <- sapply(synsets, function(x) getRelatedSynsets(x, "@") )
  hypo_synsets <- sapply(hyper_synsets, function(x) getRelatedSynsets(x, "~") )
  hypo_synsets1 = unlist(hypo_synsets, recursive = FALSE )
  temp2 = c(unlist(sapply(hyper_synsets, getWord)), unlist(sapply(hypo_synsets1, getWord)) ) 
  wordlist = paste(as.character(temp2), sep = " ", collapse = " ")
  },

error=function(cond) {
  return(NA)
},
warning=function(cond) {
  return(NA)
},
finally={}
)

return(out)
}


t = Sys.time()
temp = sapply(dict_search, function(x) hyponyms(x)) #alldata$wordnethypo
Sys.time()-t
counter2 = as.data.frame(temp)
counter2$words = as.character(rownames(counter2))
counter2 = cbind(counter2, stem_column(counter2$words), stringsAsFactors=F)
colnames(counter2)[ncol(counter2)] <- "words_stem1"
counter3 =  ddply(counter2,.(words_stem1),
                  summarise,
                  temp = paste(temp ,collapse = ' ') )
counter2$temp1 = counter3$temp[match(counter2$words_stem1,counter3$words_stem1)]

wordnethyponyms <- function(x) {
  wordarray = unlist(strsplit(x, "\\s+"))
  combined = paste(counter2[dict_search %in% wordarray,4], sep = " ", collapse = " ")
}

alldata$wordnethyp = sapply(alldata$search_term_stem, function(x) wordnethyponyms(x))


alldata = cbind(alldata, search_stem(alldata$wordnethyp ), stringsAsFactors=F)
names(alldata)[names(alldata) == 'stem'] <- 'wordnethyp_stem'


alldata = cbind(alldata, stem_column(alldata$wordnethyp_stem ), stringsAsFactors=F)
colnames(alldata)[ncol(alldata)] <-  'wordnethyp_stem1'

textinalldictstem1 <- function(x) {
  textarray = unlist(strsplit(x, "\\s+"))
  temp = textarray[textarray %in% alldict_stem1]  
  temp = str_c(temp, sep = " ", collapse = " ")
}

alldata$wordnethyp_stem1 = sapply(alldata$wordnethyp_stem1,textinalldictstem1)
alldata$wordnethyp_stem1 = sapply(alldata$wordnethyp_stem1, unique)
alldata$wordnethyp_stem1 = as.character(alldata$wordnethyp_stem1)



rm(combined)
rm(tdm, tdm_search_text, tdm_search_num, tdm_text, tdm_sparse, text)
gc()

.rs.restartR() #### restart is needed because the next calculation needs a lot of memory

### the portion below has to be manually rerun after restart

library(readr)
library(tm)
library(qdap)
library(SnowballC)
library(xgboost)
library(stringr)
library(caret)
library(car)
library(Metrics)
library(plyr)
library(ggplot2)
library(Ckmeans.1d.dp)
library(data.table)
library(tidyr)
library(Matrix)
library(SparseM)
library(stringdist)
library(stringi)
library(NLP)
library(openNLP)
library(openNLPdata)
library(slam)
library(koRpus)
library(lsa)
library(quanteda)
library(microbenchmark)
library(parallel)
library(rword2vec)
library(wordnet)
setDict("C:/Program Files (x86)/WordNet/2.1/dict")
library(RSpectra)

set.seed(1)


####### wordnet syn hyp cosines - title  - (desc syn cosine did not improve rsme)

temp = transformations(alldata$product_title_stem1, alldata$wordnetsyn_stem1, svd_kval = k)
alldata$cosine_wordnetsyn_title = temp$cosine_distance
alldata$svd5dot_wordnetsyn_title = temp$svd_dotproduct    # did not improve rsme

index = sapply(alldata$wordnetsyn_stem1, function(x) length(unlist(strsplit(x, "\\s+"))))
alldata$svd5dot_wordnetsyn_title = alldata$svd5dot_wordnetsyn_title/index

temp = cosine_transformation(alldata$product_title_stem1, alldata$wordnethyp_stem1)
alldata$cosine_wordnethyp_title = temp$cosine_distance
#alldata$svd5dot_wordnethyp_title = temp$svd_dotproduct # too big to compute


#################### search term clustering giving scaled cosines for desc title and all #########################
set.seed(1)

search_dict = as.data.frame(unique(alldata$search_text) ) #search text column used for scaled cosines
colnames(search_dict) = "search_text"
counter = as.data.table(cbind(1:dim(alldata)[1], match(alldata$search_text, search_dict$search_text), alldata$cosine_dist_desc, alldata$cosine_dist_title, alldata$cosine_dist  ) )
setkey(counter, "V2") 
scale_zero_one <- function(x){(x-min(x))/(max(x)-min(x))}
counter1 <- counter[, list(sortkey = V1, desc_cosine_scaled=scale_zero_one(V3), title_cosine_scaled=scale_zero_one(V4), all_cosine_scaled=scale_zero_one(V5)), by=key(counter)]
counter1 = counter1[order(sortkey)]
alldata$desc_cosine_scaled = counter1$desc_cosine_scaled
alldata$title_cosine_scaled = counter1$title_cosine_scaled
alldata$all_cosine_scaled = counter1$all_cosine_scaled


############################### capitalization ##########################

capitalization <- function(searchstring,text) {
  if (searchstring == "") {searchstring = " "}
  text = gsub("\\s*_\\s* ","_",text)
  text = gsub("_+","_",text)
  
  wordarray = unique(unlist(qdap::ngrams(searchstring, n = 3)) )
  wordarray = sapply(wordarray, function(x) gsub(" ", paste0("\\\\W+(?:\\\\w+\\\\W+){0,2}?"), x, perl = TRUE) )
  textarray = unlist(strsplit(text, "\\s+"))
  textarray = textarray[grepl("_",textarray)]
  textarray = gsub("_", " ", textarray)
  count=1:length(textarray)
  count_end=1:length(textarray)
  if (length(textarray)==0 ) {
    count = 0
    count_end = 0 } else { 
      for(i in 1:length(textarray)){
        count[i] <- sum( sapply(wordarray, function(x) grepl(paste("(^| )",x,"($| )",sep=""),textarray[i],perl=TRUE,ignore.case=TRUE) ) )
        count_end[i] <- sum( sapply(wordarray, function(x) grepl(paste("(^| )",x,"($)",sep=""),textarray[i],perl=TRUE,ignore.case=TRUE) ) )
        
      }
      
    }
  return(c(sum(count), sum(count_end)))
}

cap = as.data.frame(t(mapply(capitalization,alldata$search_term_stem1,alldata$product_title_stem1)))  
alldata$cap = cap[,1]
alldata$cap_end = cap[,2]  
  

#################### generate more features #######################################



 # convert brand name to brand numbers
 brand_freq_fullnames = as.data.frame(table(alldata$brand))
 rownames(brand_freq_fullnames) = brand_freq_fullnames$Var1
 brand_freq_fullnames$replacement = 1:length(brand_freq_fullnames$Var1)
 alldata$brand_num = sapply(alldata$brand, function(x) brand_freq_fullnames[x,colnames(brand_freq_fullnames) %in% "replacement"] ) 
 alldata$brand_num = as.factor(alldata$brand_num)
 
 ############################################################ fuzzy matching of search with title brand and vice versa
 
 text = stri_replace_all(alldata$product_title_stem, " ", regex = "(\\s+)*_+(\\s+)*") 
 pattern_brand = paste(alldata$brand_stem, text, sep = " ")
 pattern_brand  = str_replace_all(pattern_brand, "\\s+", "|") 
 
 pattern_search = alldata$search_term_stem
 pattern_search  = str_replace_all(pattern_search, "\\s+", "") 

 alldata$brand_match = as.numeric(str_count(pattern_search, pattern_brand))
 
 text = stri_replace_all(alldata$product_title_stem, " ", regex = "(\\s+)*_+(\\s+)*") 
 pattern_brand = paste(alldata$brand_stem, text, sep = " ")
 pattern_brand  = str_replace_all(pattern_brand, "\\s+", "") 
 
 pattern_search = alldata$search_term_stem
 pattern_search  = str_replace_all(pattern_search, "\\s+", "|") 
 
 
 alldata$search_match = as.numeric(str_count(pattern_brand, pattern_search))
 

 ################ generate feature of words not in dictionary
 dict_search = bag_o_words(alldata$search_term_stem)
 search_freq = as.matrix(table(dict_search))
 dict_search = rownames(search_freq)
 dict_search = str_replace_all(dict_search, '\\b\\w{1,3}\\b', "") ##### remove 1 and 2 character words
 dict_search = dict_search[dict_search != ""]
 
 not_in_dict = dict_search[!dict_search %in% alldict]
 pattern = stri_replace_all(not_in_dict, "\\\\b", regex = "^(\\s+)*|(\\s+)*$") 
 alldata$not_in_alldict = as.numeric(str_detect(alldata$search_term_stem, pattern))

############################# find word matches in columns #################################
t <- Sys.time()

 # this function ignores repeats in search but considers repeats in text, and is much faster!
 
 words = stri_replace_all(alldata$search_term_stem1, "", regex = "\\?")
 words = str_trim(words, side = c("both", "left", "right"))
 words = stri_replace_all(words, "\\\\b|\\\\b", regex = " ") 
 words = stri_replace_all(words, "\\\\b", regex = "^(\\s+)*|(\\s+)*$") 
 
 text = stri_replace_all(alldata$product_title_stem1, " ", regex = "(\\s+)*_+(\\s+)*") 
 alldata$nmatch_title = str_count(text, pattern = regex(paste0(words)))
 
 alldata$nwords <- str_count(alldata$search_term_stem1, pattern = regex("\\w+") )
 alldata$nmatch_desc = str_count(alldata$product_description_stem1, pattern = regex(paste0(words)))
 alldata$nmatch_brand = str_count(alldata$brand_stem1, pattern = regex(paste0(words)))
 alldata$nmatch_attrname = str_count(alldata$attr_name_stem1, pattern = regex(paste0(words)))
 alldata$nmatch_attrval = str_count(alldata$attr_value_stem1, pattern = regex(paste0(words)))

 ############ commented apr18
#  alldata$nmatch_misspelled = str_count(alldata$misspelled_stem1, pattern = regex(paste0(words)))
 

 words1 = stri_replace_all(alldata$search_term_stem1, "", regex = "\\?")
 words1 = str_trim(words1, side = c("both", "left", "right"))
 words1 = word(words1,-1)
 words1 = stri_replace_all(words1, "\\\\b", regex = "^(\\s+)*|(\\s+)*$") 
 
 
 alldata$last_word_title = str_count(text, pattern = regex(paste0(words1)))
   alldata$last_word_desc = str_count(alldata$product_description_stem1, pattern = regex(paste0(words1)))
   alldata$ratio_title = alldata$nmatch_title/alldata$nwords
   alldata$ratio_desc = alldata$nmatch_desc/alldata$nwords
   alldata$ratio_brand = alldata$nmatch_brand/alldata$nwords
   
  alldata$title_length = str_count(alldata$product_title_stem1, '\\s+')+1
 alldata$desc_length = str_count(alldata$product_description_stem1, '\\s+')+1
 alldata$brand_length = str_count(alldata$brand_stem1, '\\s+')+1
 


########## average character length in stemmed search string ########################
avg_char_length <- function(x) {
  wordarray = strsplit(x, "\\s+")
  avgchar = mean(sapply(wordarray,nchar) )
  return(avgchar)
}

alldata$avg_ch_len = sapply(alldata$search_term_stem1, avg_char_length)   



########## average distance between search words in product title, description, attr value, attr name ########################
find_dist <- function(searchstring,text) {
  if (searchstring == "") {searchstring = " "}
  wordarray = unlist(strsplit(searchstring, "\\s+"))
  textarray = unlist(strsplit(text, "\\s+"))
  
  dist <- array(-999,c(1,4)) # 1-2, 1-3, 1-4, avg of all terms wrt 1
  
  
  if ( any(wordarray[1]==textarray) ) {
    for(i in 1:length(wordarray)){
      if (i<=4) {
        dist[i] = ifelse(any(wordarray[i]==textarray), which(wordarray[i] == textarray)[1]- which(wordarray[1] == textarray)[1], -999 )  
      }
    } 
  }
  
  return(dist)
}


alldata_words <- as.data.frame(t(mapply(find_dist,alldata$search_term_stem1,alldata$product_title_stem1)))
lhs = paste("alldata$title_dist1",1:ncol(alldata_words),sep="")
rhs = paste("alldata_words[,",1:ncol(alldata_words), "]", sep="")
eq = paste(lhs, rhs, sep="<-", collapse=";")
eval(parse(text=eq))

for (i in 2:ncol(alldata_words) ) {
  for (j in i:ncol(alldata_words)) {
    eq = paste(  "alldata$title_dist",i,j, "[alldata_words[,",i,"]!=-999 & alldata_words[,",j,",]!=-999] <- (alldata_words[,",j,"] - alldata_words[,",i,"])[alldata_words[,",i,"]!=-999 & alldata_words[,",j,"]!=-999]", sep=""  )
    eval(parse(text=eq))
     }
}



alldata_words <- as.data.frame(t(mapply(find_dist,alldata$search_term_stem1,alldata$product_description_stem1)))
lhs = paste("alldata$desc_dist1",1:ncol(alldata_words),sep="")
rhs = paste("alldata_words[,",1:ncol(alldata_words), "]", sep="")
eq = paste(lhs, rhs, sep="<-", collapse=";")
eval(parse(text=eq))

for (i in 2:ncol(alldata_words) ) {
  for (j in i:ncol(alldata_words)) {
    eq = paste(  "alldata$desc_dist",i,j, "[alldata_words[,",i,"]!=-999 & alldata_words[,",j,",]!=-999] <- (alldata_words[,",j,"] - alldata_words[,",i,"])[alldata_words[,",i,"]!=-999 & alldata_words[,",j,"]!=-999]", sep=""  )
    eval(parse(text=eq))
  }
}



alldata_words <- as.data.frame(t(mapply(find_dist,alldata$search_term_stem1,alldata$brand_stem1)))
lhs = paste("alldata$brand_dist1",1:ncol(alldata_words),sep="")
rhs = paste("alldata_words[,",1:ncol(alldata_words), "]", sep="")
eq = paste(lhs, rhs, sep="<-", collapse=";")
eval(parse(text=eq))

for (i in 2:ncol(alldata_words) ) {
  for (j in i:ncol(alldata_words)) {
    eq = paste(  "alldata$brand_dist",i,j, "[alldata_words[,",i,"]!=-999 & alldata_words[,",j,",]!=-999] <- (alldata_words[,",j,"] - alldata_words[,",i,"])[alldata_words[,",i,"]!=-999 & alldata_words[,",j,"]!=-999]", sep=""  )
    eval(parse(text=eq))
  }
}




alldata_words <- as.data.frame(t(mapply(find_dist,alldata$search_term_stem1,alldata$attr_name_stem1)))
lhs = paste("alldata$attrname_dist1",1:ncol(alldata_words),sep="")
rhs = paste("alldata_words[,",1:ncol(alldata_words), "]", sep="")
eq = paste(lhs, rhs, sep="<-", collapse=";")
eval(parse(text=eq))

for (i in 2:ncol(alldata_words) ) {
  for (j in i:ncol(alldata_words)) {
    eq = paste(  "alldata$attrname_dist",i,j, "[alldata_words[,",i,"]!=-999 & alldata_words[,",j,",]!=-999] <- (alldata_words[,",j,"] - alldata_words[,",i,"])[alldata_words[,",i,"]!=-999 & alldata_words[,",j,"]!=-999]", sep=""  )
    eval(parse(text=eq))
  }
}




alldata_words <- as.data.frame(t(mapply(find_dist,alldata$search_term_stem1,alldata$attr_value_stem1)))
lhs = paste("alldata$attrval_dist1",1:ncol(alldata_words),sep="")
rhs = paste("alldata_words[,",1:ncol(alldata_words), "]", sep="")
eq = paste(lhs, rhs, sep="<-", collapse=";")
eval(parse(text=eq))

for (i in 2:ncol(alldata_words) ) {
  for (j in i:ncol(alldata_words)) {
    eq = paste(  "alldata$attrval_dist",i,j, "[alldata_words[,",i,"]!=-999 & alldata_words[,",j,",]!=-999] <- (alldata_words[,",j,"] - alldata_words[,",i,"])[alldata_words[,",i,"]!=-999 & alldata_words[,",j,"]!=-999]", sep=""  )
    eval(parse(text=eq))
  }
}



#############################################################

alldata[is.na(alldata)] <- -999
alldata$product_uid = as.factor(alldata$product_uid)

X = alldata[alldata$id %in% train$id,]
#X = cbind(X, labels[,-1])
Xtest = alldata[alldata$id %in% test$id,]
y = labels[,2]


alldatabkup = alldata

write_csv(alldata,"C:/R/homedepot/data/alldata3.csv")

