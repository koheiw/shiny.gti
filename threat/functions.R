require(quanteda)
require(stringi)
require(ggplot2)
require(ggrepel)

country_major <- c("de", "ru", "gb", "fr", "jp", "cn", "es")
country_minor <- c("ir", "iq", "af", "sy", "vn", "cu", "ca", "mx")

get_gti <- function(x, country = NULL) {
    
    x <- subset(x, year >= 1861)
    if (is.null(country))
        country <- unique(x$class)
    
    x$year_factor <- factor(x$year, seq(1861, 2017))
    x$threat <- x$lss > 0
    
    tb_year <- xtabs(~ year_factor + class + threat, x)
    total <- rowSums(tb_year)
    tb_year <- tb_year[,country,"TRUE"] / total
    if (!is.table(tb_year))
        return(tb_year)
    result <- as.data.frame(tb_year)
    colnames(result) <- c("year", "country", "gti")
    result$year <- as.numeric(as.character(result$year))
    result$country <- as.character(result$country)
    return(result)
}

smooth_gti <- function(x, m = c(1, 1), sum = FALSE, index = "gti") {
    
    x$index <- x[[index]]
    tb_year <- xtabs(index ~ year + country, x)
    if (sum) {
        tb_year <- as.table(as.matrix(rowSums(tb_year)))
        colnames(tb_year) <- "WORLD"
    }
    tb_smo <- as.table(kernapply(tb_year, kernel("daniell", m)))
    result <- as.data.frame(tb_smo)
    colnames(result) <- c("year", "country", "index")
    result$year <- as.numeric(as.character(result$year))
    result$country <- as.character(result$country)
    return(result)
}

plot_gti <- function(x, event, country = NULL, index = "gti") {
    
    if (is.null(country)) {
        country <- unique(x$country)
        sum <- TRUE
    }
    x <- x[x$country %in% country,]
    temp <- smooth_gti(x, sum = sum, index = "gti")
    temp$Index <- temp$index
    temp$Year <- temp$year
    temp$Country <- stri_trans_toupper(temp$country)
    
    label <- data.frame()
    for (m in names(event)) {
        label <- rbind(label, data.frame(Country = m,
                                         Year = unlist(event[[m]]),
                                         Name = names(unlist(event[[m]])),
                                         stringsAsFactors = FALSE))
    }
    label$Country <- stri_trans_toupper(label$Country)
    
    temp <- merge(temp, label, all.x = TRUE)
    ggplot(temp, aes(x = Year, y = Index, group = Country)) +
        geom_line(na.rm = TRUE, aes(colour = Country)) +
        geom_point(aes(y = ifelse(is.na(Name), NA, Index), colour = Country), na.rm = TRUE) +
        geom_text_repel(aes(x = Year, y = Index, label = Name, colour = Country), 
                        na.rm = TRUE,
                        min.segment.length = 0.5,
                        nudge_y = 0.01,
                        force = 10, show.legend = FALSE,
                        segment.alpha = 0.3, direction = "y") +
        ylab("Index") +
        scale_x_continuous(limits = c(1860, 2020), breaks = seq(1860, 2020, 20)) +
        theme_light() +
        theme(text = element_text(size = 13, colour = "black"),
              panel.border = element_rect(colour = "black", fill = NA, size = unit(1, "pt")),
              legend.position = "top",
              legend.text = element_text(size = 13, colour = "black"),
              axis.text = element_text(size = 13, colour = "black"), 
              axis.text.x =  element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
              axis.text.y =  element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = element_blank(),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.ticks = element_line(colour = "black", size = unit(0.5, "pt")),
              axis.ticks.length = unit(4, "pt"),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
    
}
