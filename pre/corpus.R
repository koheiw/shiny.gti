require(quanteda)
require(stringi)
source("settings.R")

dat <- rbind(readRDS(paste0(DIR_DATA, '/data_corpus_nytimes_summary_1861-2017.RDS')),
             readRDS(paste0(DIR_DATA, '/data_corpus_nytimes_summary_2018-2022.RDS')))

tid <- sprintf("%s_%04d", dat$date, ave(dat$date == dat$date, dat$date, FUN = cumsum))
dat <- cbind(tid = tid, dat)
corp <- corpus(dat, text_field = 'snippet', docid_field = "tid")

saveRDS(corp, "pre/corpus.RDS")
