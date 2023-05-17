
#help()
?install.packages
help.search("install")
library(base)
#install.packages("ggplot2")
data <- read.csv("C:/Users/Vedant Shah/Downloads/age_gender_bkts.csv~1/age_gender_bkts.csv")
#data <- read.table("filename.txt")
#data <- read.delim("filename.txt", sep="\t")
library(tableHTML)
tableHTML(head(data))
str(data)
class(data)
dim(data)
summary(data)
names(data)
tail(data)
View(data)
convert_data = data[["population_in_thousands"]]
hist(convert_data)
barplot(convert_data)
boxplot(convert_data)
year_vector = data[["year"]]
pit_vector = data[["population_in_thousands"]]
plot(year_vector,pit_vector)
trainage_vector = data[["train_users_2$age"]]
plot(pit_vector,trainage_vector, type="l")
plot(year_vector,trainage_vector, type="b")
plot(year_vector,trainage_vector,type="h")
plot(pit_vector,trainage_vector,type="s")
hist(pit_vector, col="lightblue")
abline(v = mean(year_vector), col="red")
plot(year_vector,pit_vector,type="p")
abline(v = mean(year_vector), col="red",h=mean(pit_vector))
plot(year_vector,pit_vector,type="b",pch = 19,
     col = "red", xlab = "x", ylab = "y")
length(year_vector)
min(pit_vector[pit_vector>400])
sd(year_vector)
max(pit_vector)
mean(pit_vector)
sort(pit_vector)
sum(pit_vector)
var(pit_vector)
plot(pit_vector,trainage_vector,type="p")
matrix <- matrix(pit_vector,nrow=20,ncol=20)
print(matrix)
ls()
sec_vector = data[["sessions$secs_elapsed"]]
time_vector = data[["test_users$timestamp_first_active"]]
df <- data.frame(col1=year_vector,col2=pit_vector)
print(head(df))
colnames(df) <- c("Year","Population")
print(head(df))
colnames(df)[2] <- c("Pit")
print(head(df))
df$pit <- df$Pit
print(head(df))
df[,c("pit","Year","Pit")]
print(is.factor(pit_vector))
factor_df <- factor(pit_vector)
print(factor_df)
levels(factor_df)
library(dplyr)
con_vector = data[["country_destination"]]
factor_con <- factor(con_vector)
factor_con_mod <- recode(factor_con,"US"="USA")
levels(factor_con_mod)
testdataframe <- data.frame(yr=year_vector)
transform(testdataframe,yearafter1yr=yr+1)
print(aggregate(df$Year, list(df$pit), FUN=sum))
print(aggregate(df$Year, list(df$pit), FUN=min))
print(aggregate(df$Year, list(df$pit), FUN=max))
matrix <- matrix(pit_vector,nrow=20,ncol=20)
apply( matrix, 1, sum)
apply( matrix, 2, sum)
ls(pattern = 'f')
rm(list = ls(pattern = 'f'))
ls()
#history()
table(year_vector)

cool_function <- function(x) {

  x <- x*5

  return(x)

}


cool_function <- function(x) {

  x <- x*5

  return(x)

}

typeof(cool_function)


d1 <- sapply(pit_vector, max)

print(head(d1))

tapply(pit_vector, year_vector, mean)
gen_vector=data[["gender"]]
df1 <- data.frame(col1=pit_vector,col2=year_vector,col3=gen_vector)
df2.mat=as.matrix(df1)
print(head(df2.mat))
cmat= cbind(pit_vector,year_vector)
mat2frame=as.data.frame(cmat)
print(head(mat2frame))
groupby <- by(pit_vector,list(gen_vector),mean)
print(groupby)
frame.list=as.list(gen_vector)
print(head(frame.list))
stackdf <- data.frame(pop=pit_vector,year=year_vector)
stackdf.stack=stack(stackdf)
print(head(stackdf.stack))
unstack(stackdf.stack)
#save(list = ls(pattern = 'r'), file = "C:/Sem 6/EDA/file.RData")
#load(file='file.Rdata')
dir()
sort(pit_vector)
sort(pit_vector,decreasing = TRUE)
order(pit_vector)
rank(pit_vector)
which(gen_vector == "male")
gen_vector == "male"
pit_vector > 200

df1[3,3]

df1[3,1:2]
df1[,1]

df1[c(1,3,5,7),]

df1[c(1,3,5,7),-3]

df1[c(1,3,5,7),"col3"]

df1$col1

df1$col1[1:4]

df1 %>% mutate(newcol = NA)

df1 %>% select(-col1)

demo <- df1 %>%

rename(gender = col3)

print(head(demo))

df1 %>% select(col2,col3,col1)

df1 %>% filter(col2 == 2015,col3 == "male",col1 <= 100)

df1 %>% filter(col2 == 2015,col3 == "male",col1 <= 100) %>% select(col1,col3)

df1[3,3]

df1[3,3] = "female"

df1[3,3]

df1[421,]

newdf = rbind(df1,data.frame(col1=90,col2=2015,col3="male"))

newdf[421,]

age_vector = data[["age_bucket"]]

print(head(df1))

newdf = cbind(df1,col4=c(age_vector))

print(head(newdf))
print(head(df1))

pie(pit_vector,year_vector)

df1.data = table(df1$col1)

chisq.test(df1.data)

df.exp <- exp(df1$col1)

print(head(df.exp))

df.exp <- log(df1$col1)

print(head(df.exp))

scale(matrix)

print(head(matrix))

mat2 = sweep(x = matrix, MARGIN = 1,STATS = 5, FUN = "+")

print(head(mat2))
