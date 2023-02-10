install.packages("data.table")
install.packages("lubridate")

library("data.table")
library("lubridate")

#loading data
df <- fread("/Users/nehasingh/Library/CloudStorage/OneDrive-Personal/UZH/2023/Spring_2023/R/day1/data/transactions.csv")
df #print data

#selecting rows by row numbers
df[c(1:3,5)] #selecting first 3 and 5th row

#.N gives the number of rows

#selects the last row
df[.N,] 
tail(df, 1)
df[nrow(df),]
nrow(df)

#combining conditions
df[,TransDate:=dmy(TransDate)]
df[TransDate > ymd("2010-12-24") & PurchAmount>70, ]

#select the transactions of multiple customers
df[Customer %in% c(149332, 172951), ]

#select multiple columns by column name/number
df[, list(Customer, TransDate, PurchAmount)]
df[, c(1:2, 4)]

#combine operations to select by rows and columns
df[PurchAmount > 100, list(TransDate, Cost)]
subset(df, PurchAmount > 100, select = c(TransDate, Cost))

#Apply an aggregating function to a variable by an aggregating dimension
df[, sum(PurchAmount), by=Customer]
df[, list(AggPurch=sum(PurchAmount)), by=customer] #includes renaming
df[2:5, list(AggPurch=sum(PurchAmount)), by=customer] #with selection of rows

#Create new columns in the original data.table with ":="
df[, AggPurch:=sum(PurchAmount)]
df[, list(AggPurch = sum(PurchAmount)]

#counting the number of transactions
df[, relDate:=1:.N, by=Customer]

#create a lagged variable
myData[, CostLag := shift(Cost), by=Customer]

#merging
merge(A, B, by="ID", all=TRUE) # outer join
merge(A, B, by="ID", all.x=TRUE) # left outer join
merge(A, B, by="ID", all.y=TRUE) # right outer join
merge(A, B, by.x="ID", by.y="CustID", all=FALSE) #merge with two different names
merge(A, B, by=c("ID", "TransDate"), all=FALSE) #merging by multiple IDs





