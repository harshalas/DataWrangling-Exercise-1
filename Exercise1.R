### Data Wrangling Exercise 1

## Get required libraries
library(dplyr)
library(tidyr)
library(stringr)

## Step 0 : Load Data to R Dataframe

raw.refine <- read.csv(file.choose()) # prompts you to choose the file to be loaded 
raw.refine


## Get the "shape" of the data
dim(raw.refine)
head(raw.refine)
tail(raw.refine)
str(raw.refine)

## Step 1: Cleanup brandnames

company4 <- sub("^[pPf].*","philips",raw.refine$company)

company4 <- sub("^[aA].*","akzo",company4)

company4 <- sub("^[vV].*","van houten",company4)

company4 <- sub("^[uU].*","unilever",company4)

raw.refine$company <- company4
raw.refine

## Step 2: Separate product code and number

raw.refine <- separate(raw.refine,Product.code...number,c("product_code","product_number"),sep = "-")

## Step 3: Add product categories


product_code <- raw.refine$product_code

product_category <-  lapply(product_code,function(product_code){
  if (product_code == "p") "smartphone"
  else  if (product_code == "v") "TV"
  else  if (product_code == "x") "Laptop"
  else  if (product_code == "q") "Tablet"
}
)


raw.refine <- mutate(raw.refine,product_category)

select(raw.refine,product_code,product_category)



## 4: Add full address for geocoding

raw.refine2 <- unite(raw.refine,"full_address",address,city,country,sep = ",")

## created new dataframe just so that i can preserve the old original dataframe
raw.refine2


## 5: Create dummy variables for company and product category


company_philips <-as.numeric(raw.refine2$company == "philips")
company_akzo <-as.numeric(raw.refine2$company == "akzo")
company_van_houten <-as.numeric(raw.refine2$company == "van houten")
company_unilever <-as.numeric(raw.refine2$company == "unilever")

raw.refine2 <- mutate(raw.refine2,company_philips,company_akzo,company_van_houten,company_unilever)

product_Smartphone <- as.numeric(raw.refine2$product_category == "smartphone")
product_TV <- as.numeric(raw.refine2$product_category == "TV")
product_Laptop <- as.numeric(raw.refine2$product_category == "Laptop")
product_Tablet <- as.numeric(raw.refine2$product_category == "Tablet")

raw.refine2 <- mutate(raw.refine2,product_Smartphone,product_TV,product_Laptop,product_Tablet)

raw.refine2

## Export Data to CSV

## When I tried to export the dataframe initially it gave me below error as product_category was a list
##> write.csv(raw.refine2, file = "refine_clean.csv")
##Error in .External2(C_writetable, x, file, nrow(x), p, rnames, sep, eol,  : 
##                      unimplemented type 'list' in 'EncodeElement'

## Used below function to unlist product category

df2 <- data.frame(lapply(raw.refine2, unlist))

write.csv(df2, file = "refine_clean.csv",row.names=FALSE)


