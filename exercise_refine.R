setwd("~/Desktop/Springboard_exercises")
refine_org <- read.csv("refine_xlsx_Sheet1.csv")

##### Install dplyr package
install.packages("dplyr")
install.packages("magrittr")
install.packages("tidyr")
install.packages("car")
library(car)
library(dplyr)
library(magrittr)
library(tidyr)

##### Data table overview

head(refine_org) # review the first 6 records of the table
summary(refine_org) # summary of counts of responses per heading 
dim(refine_org) # summary of number of records and variables
refine_org <- tbl_df(refine_org) # converts to tibble format; truncates additional rows past 15
names(refine_org) # lists the labels of the variables

##### Data cleaning

# Requirement 2: Separate product code and number

refine_org %>% separate(Product.code...number, c("product_code", "product_number"), sep="-") -> refine_org1

# Requirement 3: Add product categories
## Add a new column that corresponds to the product codes, where p = Smartphone, v = Tv, x = Laptop, q = Tablet

refine_org1$product_cat <- refine_org1$product_code # Create new field that contains the same product codes 
  originalvalues <- c("p", "v", "x", "q") # Define original product codes
  newvalues <- factor(c("Smartphone", "TV", "Laptop", "Tablet")) #Convert these to factor
  refine_org1$product_cat <- newvalues[match(refine_org1$product_code, originalvalues)]
  
# Requirement 4: Add full address for geocoding
## Create a new column "full_address" that concatenates the 3 address fields separated by commas

columns <- c('address', 'city', 'country')  
refine_org1$full_address <- do.call(paste, c(refine_org1[columns], sep = ", "))

# Requirement 1: Clean up brand names
## Standardize the Company column so that all spellings are standardized to "philips", "akzo", "van houten" and "unilever"

refine_org1$company <- tolower(refine_org$company)
  refine_org1$company[grepl("lips", refine_org1$company, ignore.case = FALSE)] <- "philips"
  refine_org1$company[grepl("ak", refine_org1$company, ignore.case = FALSE)] <- "akzo"
  refine_org1$company[grepl("van", refine_org1$company, ignore.case = FALSE)] <- "van houten"
  refine_org1$company[grepl("uni", refine_org1$company, ignore.case = FALSE)] <- "unilever"
  
# Requirement 5: Create dummy variables for company and product category
## Create 4 binary (1 or 0) columns for product category (e.g. product_smartphone, product_tv, etc.)
## Create 4 binary (1 or 0) columns for company category (e.g. company_philips, company_akzo, etc.)

product_ <- tolower(factor(refine_org1$product_cat))
product_matrix <- model.matrix(~ product_ + 0)

company_ <- factor(refine_org1$company)
company_matrix <- model.matrix(~ company_ + 0)

refine_org2 <- cbind(refine_org1, product_matrix, company_matrix)
  
# Generate a *.csv files containing original and cleaned data

write.csv(file = "refine_original.csv", x = refine_org)
write.csv(file = "refine_clean.csv", x = refine_org2)
