#' Theme 'ebt'
#'
#' Basic ggplot2 theme.

theme_ebt <- function(){
    col <- "gray50"
    theme_minimal() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          text = element_text(colour = col),
          axis.line = element_line(colour = col),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10, b = 10)),
          axis.text.y = element_text(size = rel(1.2), margin = margin(r = rel(0)), colour = col),
          axis.text.x = element_text(size = rel(1.2), margin = margin(t = rel(0)), colour = col),
          panel.border = element_blank(),
          axis.line.x = element_line(size = rel(1.5), lineend = 2),
          axis.line.y = element_line(size = rel(1.5), lineend = 2),
          strip.text = element_text(colour = col, size = rel(1)),
          legend.position = "top",
          legend.margin = margin(t = -2, b = -5),
          panel.spacing = unit(1.5, "lines")
         )
}

palette_ebt <- function(mood = "dark"){
    # define colors
    dcol <- c("#4162DC", "#e31a1c", "#33a02c", "#ff7f00", "#A80BA7")
    lcol <- c("#a6cee3", "#b2df8a", "#fb9a99", "#DDCC77", "#fdbf6f")

    # add color scales
    colrd <- scale_colour_manual(values = dcol)
    colrl <- scale_colour_manual(values = lcol)
    
    if(mood == "dark") return(colrd);
    if(mood == "light") return(colrl);
}

# example
data(mpg)

p <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + 
     geom_line(colour = "gray", lwd = 0.8) + geom_point() + 
     labs(colour = "Cylinders")

p <- p + theme_ebt()

# dark color scheme
p + palette_ebt() 

# light color scheme
p + palette_ebt("light")

# add grid lines if needed
p <- p + palette_ebt()
p + theme(panel.grid.major = element_line(colour = "gray", linetype = "dotted"))

# use facet grid if needed
p + facet_grid(. ~ cyl, scales = "free")
