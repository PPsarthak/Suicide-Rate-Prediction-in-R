df = read.csv(r"(D:\who_suicide_statistics.csv)")
str(df)
head(df, 10)
tail(df)
summary(df)

df = na.omit(df)

df$country = as.factor(df$country)
levels(df$country)
labels(df$country)
print(df$country)

# IMP remove the 5-14 years old age group since they are skewing the analysis
df = subset(df, age!="5-14 years")
df
df$sex = as.factor(df$sex)
df$age = as.factor(df$age)

my = subset(df, year==2000)
plot(my$country, my$suicides_no)




f_df = subset(df, sex == "female")
f_df
f_mean = mean(f_df$suicides_no)

m_df = subset(df, sex == "male")
m_df
m_mean = mean(m_df$suicides_no)

barplot(c(f_mean, m_mean))
# so as we can see gender is a factor, so we need it in multiple regression


library(dplyr)

df1 = group_by(df, country)
mean(df1$suicides_no)
mysummary = summarise(df1, mymean = mean(suicides_no))
barplot(mysummary$mymean, names.arg = mysummary$country, cex.names = 0.2) 
# cex.names shrinks the names axis i.e it increases the names we can fit


df1 = group_by(df, year)
mean(df1$suicides_no)
df1
mysummary = summarise(df1, mymean = mean(suicides_no))
plot(mysummary$year, mysummary$mymean)
#therefore the suicide rate are independent of time(year) values


df1 = group_by(df, sex)
mysummary = summarise(df1, mymean = mean(suicides_no))
barplot(mysummary$mymean, names.arg = mysummary$sex)
# so as we can see gender is a factor, so we need it in multiple regression


df1 = group_by(df, age)
mysummary = summarise(df1, mymean = mean(suicides_no))
barplot(mysummary$mymean, names.arg = mysummary$age)
# so age is a factor affecting suicide rate, so we need to include in multiple regression



# the mutate function add a col my_ranges, 
seq(min(df$population), max(df$population), 1000000)
cut(df$population, seq(min(df$population), max(df$population), 1000000))

df1_ranges = mutate(df, my_ranges = cut(df$population, seq(min(df$population), max(df$population), 1000000)))
df1_ranges

df2 = group_by(df, df1_ranges$my_ranges)
df2
mysummary = summarise(df2, mymean = mean(suicides_no))
barplot(mysummary$mymean, names.arg = mysummary$`df1_ranges$my_ranges`)
# with log10
barplot(log(mysummary$mymean, 10), names.arg = mysummary$`df1_ranges$my_ranges`)
# we can observe that as population grows, the absolute number of suicide grows as expected(i.e linearly), 
#we still have to include this value in our regression 


colnames(df)
# we are done analyzing every column with respect to number of suicide



model = lm(suicides_no~country+sex+age+population, data = df)
print(model$coefficients)
summary(model)

plot(df$age ,df$suicides_no)
abline(model)
# the countries with *** mean they have shown consistently high suicide rate or more accurately consistent suicide rate close to the regression hyperplane



model1 = lm(suicides_no~population, data = df)
print(model1$coefficients)
summary(model1)

plot(df$population ,df$suicides_no)
abline(model1)



model1 = lm(suicides_no~age, data = df)
print(model1$coefficients)
summary(model1)
plot(df$age ,df$suicides_no)
abline(model1)




library(neuralnet)
library(caret)


df3 = as.numeric(df$country)
df$country

# levels is the collection of every unique thing (country in the below case) in a factor type of columb=n?collection?
levels(df$country)[1]
levels(df$country)[2]
levels(df$country)

nrow(levels(df$country))
cut(df3, 118, labels=levels(df$country))




levels(df$sex)



new_df = df

# use as.numeric to convert (STRING) names of countries to unique numbers
new_df$country = as.numeric(new_df$country)
new_df$sex = as.numeric(new_df$sex)
new_df$age = as.numeric(new_df$age)

#use the cut function to convert the numbers back into actual string names or something,, note 118 specifies the number of levels in df$country
cut(new_df$country, 118, labels=levels(df$country))




# MAX-MIN NORMALIZATION -- dont use this
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
new_df = sapply(new_df, normalize)

# OR another min-max NORMALIZATION

# process <- preProcess(new_df, method=c("range"))
# new_df <- predict(process, new_df)

new_df
tail(new_df)

#log normalization
#new_df = log(new_df)
#new_df




#split the data into training and testing
library(caTools)

#use 70% of dataset as training set and 30% as test set
sample <- sample.split(df, SplitRatio = 0.8)
train_df  <-  as.data.frame( subset(new_df, sample == TRUE) )
test_df   <-  as.data.frame( subset(new_df, sample == FALSE) )

nrow(train_df)+nrow(test_df)

head(train_df)



NN = neuralnet(suicides_no~country+sex+age+population, train_df, hidden = c(7,5), linear.output=FALSE)
plot(NN)
print(NN)


#testing the model

#The “subset” function is used to eliminate the dependent(i.e. our y which we want to predict) variable from the test data
temp_test <- subset(test_df, select = c("country","sex", "age", "population"))
head(temp_test)
nn.results <- compute(NN, temp_test)

print(test_df$suicide_no)
results <- data.frame(actual = test_df$suicides_no, prediction = nn.results$net.result)
results

tail(results, 20)

print(sqrt(mean((results$actual - results$prediction)^2)) )





denormalize <- function(x) {   # this function is just the inverse of the `normalize` function, we have just replaced x with df$suicide_no since min and max of that was not saved and sice the result is of suicide_no 
  return ((x * (max(df$suicides_no) - min(df$suicides_no)))+min(df$suicides_no))
}
results2 = as.data.frame( lapply(results, denormalize) )
results2
colnames(results2)
print(sqrt(mean((results2$actual - results2$prediction)^2)))



# the model has a high error rate for 0, 1, 2...etc i.e very small values, this is the main reason for error
# I suspect the smaller countries/island nations are skweing the data
plot(results2$actual, results2$prediction)
