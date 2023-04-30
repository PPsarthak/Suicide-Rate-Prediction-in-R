df = read.csv(r"(D:\Suicide_without_NA.csv)")
str(df)
head(df, 10)
tail(df)
summary(df)
nrow(df)

df = na.omit(df)
str(df)

df$country = as.factor(df$country)
df$sex = as.factor(df$sex)
df$age = as.factor(df$age)
df$generation = as.factor(df$generation)

# IMP remove the 5-14 years old age group since they are skewing the analysis
df = subset(df, age!="5-14 years")


library(dplyr)

df1 = group_by(df, country)
mysummary = summarise(df1, meanPerCapitaSuicide = mean(suicides.100k.pop))
barplot(mysummary$meanPerCapitaSuicide, names.arg = mysummary$country, cex.names = 0.2)

df1 = group_by(df, year)
mysummary = summarise(df1, meanPerCapitaSuicide = mean(suicides.100k.pop))
barplot(mysummary$meanPerCapitaSuicide, names.arg = mysummary$year, cex.names = 1)

df1 = group_by(df, sex)
mysummary = summarise(df1, meanPerCapitaSuicide = mean(suicides.100k.pop))
barplot(mysummary$meanPerCapitaSuicide, names.arg = mysummary$sex, cex.names = 1)

df1 = group_by(df, age)
mysummary = summarise(df1, meanPerCapitaSuicide = mean(suicides.100k.pop))
barplot(mysummary$meanPerCapitaSuicide, names.arg = mysummary$age, cex.names = 0.5)



# the mutate function add a col my_ranges, 
seq(min(df$population), max(df$population), 1000000)
cut(df$population, seq(min(df$population), max(df$population), 1000000))
df1_ranges = mutate(df, my_ranges = cut(df$population, seq(min(df$population), max(df$population), 1000000)))
df1_ranges
df2 = group_by(df, df1_ranges$my_ranges)
df2
mysummary = summarise(df2, mymean = mean(suicides.100k.pop))
barplot(mysummary$mymean, names.arg = mysummary$`df1_ranges$my_ranges`, cex.names = 0.5)
# with log10
barplot(log(mysummary$mymean, 10), names.arg = mysummary$`df1_ranges$my_ranges`, cex.names = 0.5)



plot(df$gdp_per_capita...., df$suicides.100k.pop)
#dividing the gdp percapita in ranges of 5000$
# set the minimum gdp per capita range in seq() as 0 and max as max+5000
df2_ranges = mutate(df, gdp_range = cut(df$gdp_per_capita...., seq(0, max(df$gdp_per_capita....)+5000, 5000)))
df3 = group_by(df, df2_ranges$gdp_range)
df3
mysummary = summarise(df3, mymean = mean(suicides.100k.pop))
barplot(mysummary$mymean, names.arg = mysummary$`df2_ranges$gdp_range`, cex.names = 0.3)
barplot(log(mysummary$mymean,10), names.arg = mysummary$`df2_ranges$gdp_range`, cex.names = 0.5)



plot(df$gdp_for_year_in_billions, df$suicides.100k.pop)



df1 = group_by(df, generation)
mysummary = summarise(df1, meanPerCapitaSuicide = mean(suicides.100k.pop))
barplot(mysummary$meanPerCapitaSuicide, names.arg = mysummary$generation, cex.names = 0.5)

plot(df$HDI.for.year, df$suicides.100k.pop)


#HDI vs suicide rate,, countries with low HDI dont really have a good recored of suicides keep that in mind
df_without_na = na.omit(df)
df2_ranges = mutate(df_without_na, hdirange = cut(df_without_na$HDI.for.year, seq(0.3, max(df_without_na$HDI.for.year)+0.1, 0.1)))
df3 = group_by(df_without_na, df2_ranges$hdirange)
df3
mysummary = summarise(df3, mymean = mean(suicides.100k.pop))
barplot(mysummary$mymean, names.arg = mysummary$`df2_ranges$hdirange`, cex.names = 0.3)






colnames(df)

model = lm(suicides.100k.pop~country+year+sex+age+population+HDI.for.year+gdp_per_capita....+generation+gdp_for_year_in_billions, data = df)
summary(model)


lm_HDI = lm(suicides.100k.pop~HDI.for.year, data = df)
summary(lm_HDI)
plot(df$HDI.for.year, df$suicides.100k.pop)
abline(lm_HDI)


lm_HDI = lm(suicides.100k.pop~gdp_per_capita...., data = df)
summary(lm_HDI)
plot(df$gdp_per_capita...., df$suicides.100k.pop)
abline(lm_HDI)






library(neuralnet)
library(caret)

new_df = df

# use as.numeric to convert (STRING) names of countries to unique numbers
new_df = na.omit(new_df)
new_df$country = as.numeric(new_df$country)
new_df$sex = as.numeric(new_df$sex)
new_df$age = as.numeric(new_df$age)
new_df$generation = as.numeric(new_df$generation)

new_df$year = as.numeric(new_df$year)
new_df$population = as.numeric(new_df$population)
new_df$gdp_per_capita....   = as.numeric(new_df$gdp_per_capita....  )



new_df
colnames(new_df)
new_df = subset(new_df, select = -c(gdp_for_year...., country.year, suicides_no)) # remove these 3 columns

str(new_df)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
new_df =  as.data.frame( lapply(new_df, normalize) )

str(new_df)


#split the data into training and testing
library(caTools)

#use 70% of dataset as training set and 30% as test set
sample <- sample.split(new_df, SplitRatio = 0.8)
train_df  <-  as.data.frame( subset(new_df, sample == TRUE) )
test_df   <-  as.data.frame( subset(new_df, sample == FALSE) )



NN2 = neuralnet(suicides.100k.pop~country+sex+age+population+HDI.for.year+gdp_per_capita....+generation+gdp_for_year_in_billions, train_df, hidden = c(10,7,3), linear.output=FALSE)
plot(NN2)



temp_test <- subset(test_df, select = -c(suicides.100k.pop)) # remove this column for testing
head(temp_test)
nn.results <- compute(NN2, temp_test)

print(test_df$suicides.100k.pop)
results <- data.frame(actual = test_df$suicides.100k.pop, prediction = nn.results$net.result)
results


tail(results, 20)
print(sqrt(mean((results$actual - results$prediction)^2)) )





denormalize <- function(x) {   # this function is just the inverse of the `normalize` function, we have just replaced x with df$HDI.for.year since min and max of that was not saved and sice the result is of suicide_no 
  return ((x * (max(df$suicides.100k.pop) - min(df$suicides.100k.pop)))+min(df$suicides.100k.pop))
}
results2 = as.data.frame( lapply(results, denormalize) )
results2
colnames(results2)
#root mean squared error
print(sqrt(mean((results2$actual - results2$prediction)^2)))

plot(results2$actual, results2$prediction)




stddev = sqrt(var(df$suicides.100k.pop))
m = mean(df$suicides.100k.pop)
prob = dnorm(df$suicides.100k.pop, mean = m, sd = stddev)
plot(df$suicides.100k.pop, prob) # bell curve of suicide rate




