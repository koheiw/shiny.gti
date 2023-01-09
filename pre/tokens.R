require(quanteda)
require(stringi)
source("settings.R")

month <- c('January', 'February', 'March', 'April', 'May', 'June',
           'July', 'August', 'September', 'October', 'November', 'December')
month <- c(month, stri_sub(month, 1, 3))
day <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
title <- c('Mr', 'Ms', 'Mrs', "Dr", "Gen")

corp <- readRDS("pre/corpus.RDS")

# remove by-line
texts(corp) <- stri_replace_all_regex(texts(corp), "^[\\p{Lu}\\p{Z}]+(.{0,30}?)(\\(.{0,50}?\\))?(--)", "")

# normalize old typograpy
texts(corp) <- stri_replace_all_regex(texts(corp), "\\w+\\.\\.\\.$", "") # truncated word
texts(corp) <- stri_replace_all_regex(texts(corp), "([A-Z])\\.", "$1") # acronyms
texts(corp) <- stri_replace_all_fixed(texts(corp), "-", " ") # old multi-words expression
texts(corp) <- stri_replace_all_fixed(texts(corp), "'", " ") # posession

toks <- tokens(corp) %>% 
    tokens_remove(c(stopwords(), stri_trans_totitle(stopwords())), 
                  case_insensitive = FALSE, padding = TRUE) %>% 
    tokens_remove(c(month, day, title), padding = TRUE) %>% 
    tokens_select("^[a-z0-9]+$", valuetype = "regex", padding = TRUE) %>% 
    tokens_remove("^[0-9]+$", valuetype = "regex", padding = TRUE)

col_acro <- toks %>% 
    tokens_select('^[A-Z]$', valuetype = 'regex', case_insensitive = FALSE, padding = TRUE) %>% 
    textstat_collocations(min_count = 50, tolower = FALSE)

col_multi <- toks %>% 
    tokens_select('^[A-Z][A-Za-z0-9]+', valuetype = 'regex', case_insensitive = FALSE, padding = TRUE) %>% 
    textstat_collocations(min_count = 50, tolower = FALSE)

toks_comp <- toks %>% 
    tokens_compound(col_acro, concatenator = "", join = FALSE) %>% 
    tokens_compound(col_multi, join = FALSE) %>%
    tokens_select(min_nchar = 2)

saveRDS(toks_comp, "pre/tokens.RDS")
