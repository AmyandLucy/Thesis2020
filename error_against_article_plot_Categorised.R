library(plyr)
library(dplyr)
library(ggplot2)

# Read Data
pre.data <- read.csv("categorised_pre_data.csv")

#Group Error by Article
pre.data %>%
  dplyr::group_by(PMCID, error) %>% #grouping article by PMCID
  dplyr::summarise(total_errors = sum(error)) -> errors_by_article #create a frequency table with number of errors per article


# Plotting
plot3 <- qplot(errors_by_article$total_errors[!errors_by_article$total_errors==0],
               geom = "histogram",
               binwidth = 1,
               xlim = c(1.5, 20),
               xlab = "Article with x Number of Errors",
               ylab = "Frequency of Errors",
               main = element_blank(),
               fill = I("#374E55FF"),
               col = I("black"))

plot3 <- plot3 + theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plot3

# Read Data
pre.data <- read.csv("categorised_pre_data.csv")

# Removing rows with NA for error
pre.data <- pre.data[complete.cases(pre.data[,23]),]


