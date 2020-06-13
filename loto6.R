library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(pheatmap)
library(arules)

# load data
loto6 <- read.csv("data/loto6_data.csv")

loto6 <- as.data.table(loto6)

# bar plot of each honsuuji
loto6 %>% select(hon1) %>% ggplot(aes(hon1))+geom_bar()
loto6 %>% select(hon2) %>% ggplot(aes(hon2))+geom_bar()
loto6 %>% select(hon3) %>% ggplot(aes(hon3))+geom_bar()
loto6 %>% select(hon4) %>% ggplot(aes(hon4))+geom_bar()
loto6 %>% select(hon5) %>% ggplot(aes(hon5))+geom_bar()
loto6 %>% select(hon6) %>% ggplot(aes(hon6))+geom_bar()


# bar plot of all (honsuuji & bonus) data
loto6[,5:ncol(loto6)] %>% gather() %>% ggplot(aes(value))+geom_bar()
loto6[,5:ncol(loto6)] %>% gather() %>% group_by(value) %>% count() %>% arrange(desc(n)) %>% View()
# bar plot of honsuuji data
loto6[,5:10] %>% gather() %>% ggplot(aes(value))+geom_bar()
loto6[,5:10] %>% gather() %>% group_by(value) %>% count() %>% arrange(desc(n)) %>% View()
# bar plot of bonus data
loto6[,11:ncol(loto6)] %>% gather() %>% ggplot(aes(value))+geom_bar()
loto6[,11:ncol(loto6)] %>% gather() %>% group_by(value) %>% count() %>% arrange(desc(n)) %>% View()


# loto6 data by year
# arrange suuji by appearrance each year
loto6_over_year_temp <- loto6 %>%  
  select(year, colnames(loto6[,5:ncol(loto6)])) %>% 
  pivot_longer(cols = -year, names_to = "variables", values_to = "suuji") %>%
  group_by(year, suuji) %>%
  count(name = "appearance") %>% 
  ungroup() %>%
  group_by(year) %>%
  arrange(year, desc(appearance)) %>% 
  select(year, suuji)

# throws an error because some years didn't have all numbers appear
loto6_over_year <- matrix(ncol = 21, nrow = 43)
cnames <- distinct(loto6_over_year_temp, year)$year
for(i in 1:n_distinct(loto6_over_year_temp$year)){
  i = 8
  name <- cnames[i]
  loto6_over_year[,i] <- loto6_over_year_temp[loto6_over_year_temp$year == name,]$suuji
}
loto6_over_year <- as.data.table(loto6_over_year)
colnames(loto6_over_year) <- as.character(cnames)

loto7_top10_year <- head(loto6_over_year, n=10)


# correlation test / heatmap
cor_table <- cor(loto6, method = "spearman") # spearman cor table
pheatmap(cor_table)


# PCA
pca6 <- princomp(loto6[,2:11])
summary(pca6, loadings=T)


# distribution of means
expected_mean <- sum(1:43)/43

means <- c()
for(i in 1:nrow(loto6)){
  row <- loto6[i,5:11]
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
  winning <- sample(x = c(1:43), size = 7)
  avg <- sum(winning) / 7
  avg <- round(avg, digits = 0)
  resampled_means[i] <- avg
}

hist(resampled_means)
mean(means)
quantile(resampled_means, 0.975)


## "frequent itemset mining" with arules library
# make data into factors (to be compatible with arules library)
loto6_factors <- data.frame(sapply(loto6, as.factor))

# make "transactions" object from the dataframe
trans <- as(loto6_factors[,5:11], "transactions")
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
x <- loto6[,5:10]

m <- 2
x <- t(apply(x, 1, sort))
x0 <- apply(x, 1, combn, m=m)
y <- array(x0, c(m, length(x0)/(m*dim(x)[1]), dim(x)[1]))
ngrams <- apply(y, c(2,3), function(s) paste("(", paste(s, collapse=","), ")", sep=""))
z <- sort(table(as.factor(ngrams)), decreasing=TRUE)

head(z, n = 20)
tail(z, n=20)



