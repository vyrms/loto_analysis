library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(pheatmap)
library(arules)



# load data
loto6 <- read.csv("data/loto6_data.csv")
loto7 <- read.csv("data/loto7_data.csv")

loto6 <- as.data.table(loto6)
loto7 <- as.data.table(loto7)


# bar plot of each honsuuji
loto7 %>% select(hon1) %>% ggplot(aes(hon1))+geom_bar()
loto7 %>% select(hon2) %>% ggplot(aes(hon2))+geom_bar()
loto7 %>% select(hon3) %>% ggplot(aes(hon3))+geom_bar()
loto7 %>% select(hon4) %>% ggplot(aes(hon4))+geom_bar()
loto7 %>% select(hon5) %>% ggplot(aes(hon5))+geom_bar()
loto7 %>% select(hon6) %>% ggplot(aes(hon6))+geom_bar()
loto7 %>% select(hon7) %>% ggplot(aes(hon7))+geom_bar()


# bar plot of all (honsuuji & bonus) data
loto7[,5:ncol(loto7)] %>% gather() %>% ggplot(aes(value))+geom_bar()
loto7[,5:ncol(loto7)] %>% gather() %>% group_by(value) %>% count() %>% arrange(desc(n)) %>% View()
# bar plot of honsuuji data
loto7[,5:11] %>% gather() %>% ggplot(aes(value))+geom_bar()
loto7[,5:11] %>% gather() %>% group_by(value) %>% count() %>% arrange(desc(n)) %>% View()
# bar plot of bonus data
loto7[,12:ncol(loto7)] %>% gather() %>% ggplot(aes(value))+geom_bar()
loto7[,12:ncol(loto7)] %>% gather() %>% group_by(value) %>% count() %>% arrange(desc(n)) %>% View()


# loto7 data by year
loto7_over_year_temp <- loto7 %>%  
  select(year, colnames(loto7[,5:ncol(loto7)])) %>% 
  pivot_longer(cols = -year, names_to = "variables", values_to = "suuji") %>%
  group_by(year, suuji) %>%
  count(name = "appearance") %>% 
  ungroup() %>%
  group_by(year) %>%
  arrange(year, desc(appearance)) %>% 
  select(year, suuji)

loto7_over_year <- matrix(ncol = 7, nrow = 37)
cnames <- distinct(loto7_over_year_temp, year)$year
for(i in 1:n_distinct(loto7_over_year_temp$year)){
  name <- cnames[i]
  loto7_over_year[,i] <- loto7_over_year_temp[loto7_over_year_temp$year == name,]$suuji
}
loto7_over_year <- as.data.table(loto7_over_year)
colnames(loto7_over_year) <- as.character(cnames)

loto7_top10_year <- head(loto7_over_year, n=10)


# correlation test / heatmap
cor_table <- cor(loto7, method = "spearman") # spearman cor table
pheatmap(cor_table)


# PCA
pca7 <- princomp(loto7[,2:13])
summary(pca7, loadings=T)


# distribution of means
expected_mean <- sum(1:37)/37

means <- c()
for(i in 1:nrow(loto7)){
  row <- loto7[i,5:13]
  avg <- sum(row) / 9
  avg <- round(avg, digits = 0)
  means[i] <- avg
}

hist(means)
mean(means)


# resampling methods
resampled_means <- c()
set.seed(123)
for(i in 1:1000){
  winning <- sample(x = c(1:37), size = 9)
  avg <- sum(winning) / 9
  avg <- round(avg, digits = 0)
  resampled_means[i] <- avg
}

hist(resampled_means)
mean(means)
quantile(resampled_means, 0.975)


## "frequent itemset mining" with arules library
# make data into factors (to be compatible with arules library)
loto7_factors <- data.frame(sapply(loto7, as.factor))

# make "transactions" object from the dataframe
trans <- as(loto7_factors[,5:11], "transactions")
summary(trans)

# get item frequency
head(itemFrequency(trans, type = 'absolute'), n = 10)
itemFrequencyPlot(trans, type = 'absolute', topN = 10, cex = 0.8)

# affinity = probability 2 numbers come up together
aff <- affinity(trans)
aff[aff > 0.3]


rules <- apriori(trans, parameter=list(support=0.05,
                                       confidence=0.5, 
                                       maxlen=10, 
                                       target="rules"))
summary(rules)

itemsets <- eclat(trans)
rules <- ruleInduction(itemsets, trans, confidence = .5)
rules


## base R to get item set frequency
# get data just for honsuuji
x <- loto7[,5:11]

m <- 2
x <- t(apply(x, 1, sort))
x0 <- apply(x, 1, combn, m=m)
y <- array(x0, c(m, length(x0)/(m*dim(x)[1]), dim(x)[1]))
ngrams <- apply(y, c(2,3), function(s) paste("(", paste(s, collapse=","), ")", sep=""))
z <- sort(table(as.factor(ngrams)), decreasing=TRUE)

head(z, n = 20)






