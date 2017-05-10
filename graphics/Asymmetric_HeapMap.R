# libraries
library(ggplot2)
library(RColorBrewer)

# dataset
USPrograms <- read.csv2(file.choose(), header = TRUE, sep=",", dec = ".")
NCProgram <- subset(USPrograms, USPrograms$program == "NC")
str(NCProgram)

# investment
plot <- ggplot(NCProgram, aes(investment, fill="red", color="red", alpha = 0.3))
plot <- plot + geom_density()
plot <- plot + geom_vline(xintercept = mean(NCProgram$investment), alpha = 0.5)
plot <- plot + ggtitle("Distribution of investment")
plot

plot <- ggplot (NCProgram, aes(year, country, fill = investment))
plot <- plot + geom_tile()
plot <- plot + theme(text = element_text(size=8))
plot <- plot + ggtitle("NC Investement per year in each country")
plot


# investment
plot <- ggplot(NCProgram, aes(invLog, fill="red", color="red", alpha = 0.3))
plot <- plot + geom_density()
plot <- plot + geom_vline(xintercept = mean(NCProgram$invLog), alpha = 0.5)
plot <- plot + ggtitle("Distribution of investment. Logarithmic transformation")
plot

plot <- ggplot (NCProgram, aes(year, country, fill = invLog))
plot <- plot + geom_tile()
plot <- plot + theme(text = element_text(size=8))
plot <- plot + ggtitle("NC Investement per year in each country")
plot