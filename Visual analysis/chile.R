library("ggplot2")
Chile <-read.csv2(file.choose(), header = TRUE, sep = ",")
Chile <- na.omit(Chile)
str(Chile)
Chile$statusquo <- as.numeric(Chile$statusquo) 
#SEX
plot <- ggplot(Chile, aes(sex, fill = sex))
plot <- plot + geom_bar()
plot <- plot + scale_fill_brewer(palette = "Set1")
plot

#VOTING
plot <- ggplot(Chile, aes(vote, fill=vote))
#plot <- plot + facet_grid(. ~ sex) + theme(panel.background = element_rect(fill = 'green'))
#plot <- plot + facet_grid(. ~ sex)
plot <- plot + facet_grid(. ~ sex)
plot <- plot + geom_bar()
plot <- plot + scale_fill_brewer(palette = "Set2")
plot <- plot + geom_rect(. ~ sex, aes(ymin=0, ymax=80, xmin=xstart,
                                         xmax=xend, fill=col), alpha =0.5) + coord_flip()
plot




# Preservation of the statusquo vs. vote & gender
plot <- ggplot(Chile, aes(vote, statusquo, fill = vote))
plot <- plot + facet_grid(. ~ sex)
plot <- plot + geom_boxplot()
plot <- plot + scale_fill_brewer(palette = "Set3")
plot

# Preservation of the statusquo vs. income & gender
plot <- ggplot(Chile, aes(as.factor(income), statusquo, fill = as.factor(income)))
plot <- plot + facet_grid(sex ~ .)
plot <- plot + geom_boxplot()
plot <- plot + scale_fill_brewer(palette = "Set1")
plot

# Sample density by income
plot <- ggplot(Chile, aes(statusquo, as.factor(income), color = statusquo))
plot <- plot + geom_jitter()
plot <- plot + scale_color_gradient2(low = "purple4", mid = "seashell3", high = "springgreen4",
                                     midpoint = 0)
plot


