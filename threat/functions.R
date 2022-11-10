require(quanteda)
require(stringi)
require(ggplot2)
require(ggrepel)

get_country <- function(x) {
    cc <- levels(x$data$class)
    m <- countrycode::countrycode(cc, "iso2c", "country.name")
    cc <- cc[!is.na(m)]
    names(cc) <- m[!is.na(m)]
    return(cc[order(names(cc))])
}

get_gti <- function(x) {
    
    x <- subset(x, year >= 1861)
    #if (is.null(country))
    #    country <- unique(x$class)
    
    x$year_factor <- factor(x$year, seq(1861, 2017))
    x$threat <- x$lss > 0
    
    tb_year <- xtabs(~ year_factor + class + threat, x)
    total <- rowSums(tb_year)
    tb_year <- tb_year[,,"TRUE"] / total
    # if (!is.table(tb_year))
    #     return(tb_year)
    temp <- as.data.frame(tb_year)
    colnames(temp) <- c("year", "country", "gti")
    temp$year <- as.numeric(as.character(temp$year))
    temp$country <- as.character(temp$country)
    return(temp)
}

smooth_gti <- function(x, m = c(1, 1), sum = FALSE, index = "gti") {
    
    x$index <- x[[index]]
    tb_year <- xtabs(index ~ year + country, x)
    if (sum) {
        tb_year <- as.table(as.matrix(rowSums(tb_year)))
        colnames(tb_year) <- "all"
    }
    tb_smo <- as.table(kernapply(tb_year, kernel("daniell", m)))
    temp <- as.data.frame(tb_smo)
    colnames(temp) <- c("year", "country", "index")
    temp$year <- as.numeric(as.character(temp$year))
    temp$country <- as.character(temp$country)
    return(temp)
}

plot_gti <- function(x, event, country = NULL, index = "gti") {
    
    if (is.null(country)) {
        country <- unique(x$country)
        sum <- TRUE
    } else {
        sum <- FALSE
    }
    x <- x[x$country %in% country,]
    temp <- smooth_gti(x, sum = sum, index = "gti")
    names(temp) <- stri_trans_totitle(names(temp))
    
    label <- data.frame()
    for (m in names(event)) {
        label <- rbind(label, data.frame(Country = stri_trans_tolower(m),
                                         Year = unlist(event[[m]]),
                                         Event = names(unlist(event[[m]])),
                                         stringsAsFactors = FALSE))
    }
    temp <- merge(temp, label, all.x = TRUE)
    if (all(temp$Country != "all")) {
        temp$Country <- factor(temp$Country, levels = country)
        v <- names(country_all)
        names(v) <- country_all
        levels(temp$Country) <- v[levels(temp$Country)]
    } else {
        temp$Country <- factor("All")
    }
    
    ggplot(temp, aes(x = Year, y = Index, group = Country)) +
        ylim(0, max(temp$Index) * 1.2) +
        geom_line(na.rm = TRUE, aes(colour = Country)) +
        geom_point(aes(y = ifelse(is.na(Event), NA, Index), colour = Country), na.rm = TRUE) +
        geom_text_repel(aes(x = Year, y = Index, label = Event, colour = Country, size = 4), 
                        na.rm = TRUE,
                        min.segment.length = 0.1,
                        nudge_y = 0.01,
                        force = 10, show.legend = FALSE,
                        segment.alpha = 0.3, direction = "y") +
        ylab("Index") +
        scale_x_continuous(limits = c(1860, 2020), breaks = seq(1860, 2020, 20)) +
        theme_bw() +
        theme(text = element_text(size = 13, colour = "black"),
              legend.position = "top",
              legend.text = element_text(size = 13, colour = "black"),
              axis.text = element_text(size = 13, colour = "black"), 
              axis.text.x =  element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
              axis.text.y =  element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.ticks = element_line(size = unit(0.5, "pt")),
              axis.ticks.length = unit(4, "pt"),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
    
}

plot_terms <- function(x, highlighted = NULL, max_words = 10000) {
    
    max_words <- check_integer(max_words, min_len = 1, max_len = 1, min = 1)
    
    if (is.null(highlighted))
        highlighted <- character()
    if (is.dictionary(highlighted)) {
        separator <- meta(highlighted, field = "separator", type = "object")
        valuetype <- meta(highlighted, field = "valuetype", type = "object")
        concatenator <- x$concatenator
        highlighted <- unlist(highlighted, use.names = FALSE)
        if (!nzchar(separator) && !is.null(concatenator)) # for backward compatibility
            highlighted <- stri_replace_all_fixed(highlighted, separator, concatenator)
    } else {
        highlighted <- unlist(highlighted, use.names = FALSE)
        valuetype <- "glob"
    }
    words_hl <- quanteda::pattern2fixed(
        highlighted,
        types = names(x$beta),
        valuetype = valuetype,
        case_insensitive = TRUE
    )
    
    # fix for a bug before v1.1.4
    x$frequency <- x$frequency[names(x$beta)]
    
    beta <- frequency <- word <- NULL
    temp <- data.frame(word = names(x$beta), beta = x$beta, frequency = log(x$frequency),
                       stringsAsFactors = FALSE)
    is_hl <- temp$word %in% unlist(words_hl, use.names = FALSE)
    is_sm <- temp$word %in% sample(temp$word, min(length(temp$word), max_words))
    temp_black <- subset(temp, is_hl)
    temp_gray <- subset(temp, !is_hl & is_sm)
    ggplot(data = temp_gray, aes(x = beta, y = frequency, label = word)) +
        geom_text(colour = "grey70", alpha = 0.7) +
        labs(x = "Polarity", y = "Frequency (log)") +
        theme_bw() +
        theme(panel.grid= element_blank(),
              axis.text = element_text(size = 13, colour = "black"), 
              axis.text.x =  element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
              axis.text.y =  element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              text = element_text(size = 13, colour = "black"),
              plot.margin = margin(t = 30)) +
        geom_text_repel(data = temp_black, aes(x = beta, y = frequency, label = word, size = 4),
                        segment.size = 0.25, colour = "black", show.legend = FALSE) +
        geom_point(data = temp_black, aes(x = beta, y = frequency), cex = 0.7, colour = "black")
    
}

