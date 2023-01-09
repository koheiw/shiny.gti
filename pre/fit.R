require(quanteda)
require(newsmap)
require(LSX)
source("settings.R")
options(lss_cache_dir = DIR_CACHE)

toks <- readRDS("pre/tokens.RDS")

# newsmap -----------------------------------------------

dict_newsmap <- dictionary(file = 'pre/historical.yml')
toks_cap <- tokens_select(toks, '^[A-Z][A-Za-z1-2]+', valuetype = 'regex', 
                          case_insensitive = FALSE)

dfmt_dict <- toks_cap %>% 
    tokens_lookup(dict_newsmap, levels = 3, nested_scope = "dictionary") %>% 
    dfm()
dfmt_feat <- toks_cap %>% 
    dfm(tolower = FALSE) %>% 
    dfm_trim(min_termfreq = 100)
map <- textmodel_newsmap(dfmt_feat, dfmt_dict)

pred <- predict(map, confidence = TRUE)
pred$class[pred$class %in% c("cn", "tw", "mo")] <- "cn"
pred$class[pred$confidence.fit <= 1] <- "us"
toks$class <- pred$class

# LSS -----------------------------------------------

dict_seed <- dictionary(file = 'seedwords.yml')
dfmt <- toks %>% 
    tokens_remove("^[A-Z]", valuetype = "regex", case_insensitive = FALSE) %>% 
    dfm() %>% 
    dfm_trim(min_termfreq = 10)

seed <- as.seedwords(dict_seed$Hostility)
lss <- textmodel_lss(dfmt, seed, cache = TRUE, weight = "logcount", include_data = TRUE)

saveRDS(lss, "lss.RDS")

