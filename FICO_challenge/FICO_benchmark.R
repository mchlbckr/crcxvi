library(openxlsx)
fico_benchmark <- read.xlsx("FICO_challenge/FICO_benchmark.xlsx",1)

fico_benchmark <- fico_benchmark[-5,]
fico_benchmark <- fico_benchmark[order(fico_benchmark$test, decreasing = TRUE),]

library(ggrepel)
library(plotly)
library(purrr)
library(RColorBrewer)

cols <- character(9L)
cols[-7] <- brewer.pal(9, "Greys")[2:9]
cols[7] <- "#E41A1C"

fico_benchmark$FICO[5] <- "Score Card"
fico_benchmark$type[5] <- "Score Card"


b <- 
ggplot(fico_benchmark, aes(x = reorder(FICO,-test), y = test, fill = type,
                           text=map(paste('<b>AUC:</b>', test, '<br>', '<b>Model:</b>', FICO), "HTML"))) +
  geom_bar(stat="identity") +
  coord_cartesian(ylim = c(.75, .8)) +
  scale_fill_manual(values=cols) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.75)) + 
  xlab("Models") + 
  ylab("AUC on test data set")

ggplotly(b, tooltip = "test")

cols <- character(21L)
cols[-c(2,19,16)] <- brewer.pal(9, "Greys")[9]
cols[c(2,19,16)] <- "#E41A1C"

b <- ggplot(fico_benchmark, aes(x = train, y = test, color = FICO, label = FICO)) +
  geom_point(stat="identity") +
  coord_cartesian(ylim = c(.75, .9), xlim = c(.75,.9)) +
  theme_light() +
  geom_text_repel(size = 3) +
  theme(legend.position = "none") +
  scale_colour_manual(values=cols) +
  xlab("AUC on training data set") + 
  ylab("AUC on test data set") + 
  geom_abline(slope = 1, intercept = 0) + coord_fixed(ratio = 5)
b


library(readr)
score_range <- read_csv("score_range.csv")
ggplot(score_range, aes(x = reorder(Model, -range), y = range)) +
  geom_bar(stat="identity") + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.75)) + 
  xlab("Models") + 
  ylab("Score Card points") + 
  coord_flip()



åggplot(fico_benchmark, aes(train, test, label = FICO, color = type)) +
  geom_point(size = 2) +
  geom_text_repel() + theme_drwhy() + xlab("AUC for train dataset") +
  ylab("AUC for test dataset") + theme(legend.position = "bottom") +
  ggtitle("Selected ML models for FICO challenge","")
