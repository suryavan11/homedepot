set.seed(1)
setwd("C:/R/homedepot")

#########Read data
train <- read_csv('data/train.csv')
test <- read_csv('data/test.csv')
desc <- read_csv('data/product_descriptions.csv')
attrib <- read_csv('data/attributes.csv')

#### read custom data generated via preprocessing or from forum
search_analogs <- read.csv('data/search_dict_unique_nonumbers2.csv', stringsAsFactors = FALSE)
missing_brands <- read_csv('data/cleaned data backup/missing_brands.csv')
spell_check_google <- read.table('data/google spellcheck results.csv', header = FALSE, sep = "")
spell_check_google = spell_check_google[,c(1,3)]

######### read forum dictionaries
rb_dictionary <- read.csv('data/forum data/randombishop-dictionary_corrected.csv', stringsAsFactors = FALSE)
andrey_dictionary <- read.csv('data/forum data/speldic-andrey kiryasov dictionary-corrected.csv', stringsAsFactors = FALSE)
matveev_dictionary <- read.csv('data/forum data/alexey matveev-words_to_correct-corrected.csv', stringsAsFactors = FALSE)
silogram_dictionary <- read.csv('data/forum data/silogram-spelling-corrected.csv', stringsAsFactors = FALSE)
yang_dictionary <- read.csv('data/forum data/spell_check_yang-corrected.csv', stringsAsFactors = FALSE)
rb_replacements <- read.csv('data/forum data/randombishop-replacements-corrected.csv', stringsAsFactors = FALSE)
spelling <- read_delim("data/forum data/spelling.txt", delim="|")
