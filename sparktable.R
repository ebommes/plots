options(stringsAsFactors = FALSE)

library(grid)
library(gtable)
library(ggplot2)
library(ggthemes)

randu <- function(n) {
    r <- sample(1:5, 1)
    if(r == 1) return(rnorm(n));
    if(r == 2) return(runif(n));
    if(r == 3) return(rbinom(n, 10, 0.5));
    if(r == 4) return(rpois(n, 0.8));
    if(r == 5) return(rexp(n));
}

test_intg <- function(x) {
    if(class(x) != 'numeric') return(TRUE);
    perc <- length(unique(x)) / length(x)

    if(perc <= 0.05) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

sumry <- function(df) {
    df_mean <- sapply(df, mean)
    df_median <- sapply(df, median)
    df_sd <- sapply(df, sd)
    df_min <- sapply(df, min)
    df_max <- sapply(df, max)
    data.frame(Variable = names(df), 
               Mean = format(round(df_mean, 3), nsmall = 3), 
               Median = format(round(df_median, 3), nsmall = 3), 
               St.Dev = format(round(df_sd, 3), nsmall = 3),
               Min = format(round(df_min, 3), nsmall = 3),
               Max = format(round(df_max, 3), nsmall = 3))
}

theme_spark <- function() {
    theme_tufte() + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank()) 
}

tplotter <- function(x) {
    df_tmp <- data.frame(x = c(1:length(x)), y = x)

    if(test_intg(x) == TRUE) {
        p <- ggplot(df_tmp, aes(x = x, y = y)) + 
             theme_spark() +
             geom_hline(yintercept = 0, colour = 'darkgrey') +
             geom_point(fill = 'black', size = 0.1)
    } else {
        p <- ggplot(df_tmp, aes(x = x, y = y)) + 
             theme_spark() +
             geom_hline(yintercept = 0, colour = 'darkgrey') +
             geom_line(colour = 'black')
    }

    return(ggplotGrob(p))
}

dplotter <- function(x) {
    df_tmp <- data.frame(x = c(1:length(x)), y = x)

    if(test_intg(x) == TRUE) {
        p <- ggplot(df_tmp, aes(x = y)) + 
             theme_spark() + 
             geom_bar(fill = 'black', width = 0.25)
    } else {
        p <- ggplot(df_tmp, aes(x = y)) + 
             theme_spark() +
             geom_density(color = 'black')
    }

    return(ggplotGrob(p))
}

bplotter <- function(x) {
    df_tmp <- data.frame(x = c(1:length(x)), y = x)

    p <- ggplot(df_tmp, aes(x = y, y = y)) + 
         theme_spark() +
         coord_flip() +
         geom_tufteboxplot(median.type = 'line', whisker.type = 'line', 
                           hoffset = 0, width = 3, voffset = 0.02)

    return(ggplotGrob(p))
}

n <- 50
set.seed(1234)
df <- data.frame(a = randu(n), b = randu(n), c = randu(n), d = randu(n), 
                 e = randu(n), f = randu(n), g = randu(n), h = randu(n))

df_sumry <- sumry(df)

df.names <- names(df_sumry)
m <- ncol(df_sumry)
n <- nrow(df_sumry)

funs <- c('tplotter', 'bplotter', 'dplotter')

gtab <- gtable(unit(rep(1, m + length(funs)), 'null'), unit(rep(1, n + 1), 'null'))

# fill text
for(i in 1:n) {
    for(j in 1:ncol(df_sumry)) {
        if(i == 1) {
            gtab <- gtable_add_grob(gtab, textGrob(df.names[j]), 
                                    t = i, l = j, r = j)
        }

        gtab <- gtable_add_grob(gtab, textGrob(df_sumry[i, j]), 
                                t = i + 1, l = j, r = j) 
    }

    for(j in 1:length(funs)) {
        gtab <- gtable_add_grob(gtab, do.call(funs[j], list(df[, i])), 
                                t = i + 1, l = m + j, r = m + j)
    }
    
}

gtab <- gtable_add_grob(gtab, textGrob(paste('# Obs. =', nrow(df))), 
                        t = 1, l = m + 1, r = m + length(funs))

dev.new(width = 0.79 * (m + 3), height = 0.42 * n)
grid.draw(gtab)
