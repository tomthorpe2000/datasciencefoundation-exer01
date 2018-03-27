# Springboard Data Science Foundation Class
# Exercise 01 - Data Wrangling - Refine Data
# Clean up and extend data in an practice dataset of product information.

# load required libraries for exercise
library(devtools)
library(tidyr)
library(dplyr)

# identify input and output files
infile = "C:/Users/Tom/git/datasciencefoundation/DataWrangleExer01/refine_original.csv"
outfile = "C:/Users/Tom/git/datasciencefoundation/DataWrangleExer01/refine_clean.csv"  

# Read CSV file into a dataframe
refineData <- read.csv(file=infile, header=TRUE, sep=",")

# Lets see what the data looks like
print(refineData)

#View(refineData)
#str(refineData)

# Lets load into a local data frame and check it's contents
products <- tbl_df(refineData) 
print(products)

# clean up company names, correcting spellings
# Set company name to 'adzo', philips', 'unilever', or 'van houten' 
# The company names have various mis-spelling that most all start with the first letter of 
# the company name. The exception is one entry that starts the spelling of philips with an 'f'
# Use a regular expression to select the appropriate records for each of the company names.
products$company[grepl("^a",products$company,ignore.case=TRUE)] <- "akzo"
products$company[grepl("^p|^f",products$company,ignore.case=TRUE)] <- "philips"
products$company[grepl("^u",products$company,ignore.case=TRUE)] <- "unilever"
products$company[grepl("^v",products$company,ignore.case=TRUE)] <- "van houten"

# check the results
print(products,n=30)

# Next separate the product code number into product_code and product_number by using
# the separate function to split the data at the dash.
products <- separate(products,Product.code...number,
                     c("product_code","product_number"),
                     sep="-",
                     remove=FALSE)
# lets check out the results
select(products,contains("product")) %>% print(n=25)

products <- mutate(products,product_category=product_code)
products$product_category[grepl("p",products$product_code)] <- "Smartphone"
products$product_category[grepl("q",products$product_code)] <- "Tablet"
products$product_category[grepl("v",products$product_code)] <- "TV"
products$product_category[grepl("x",products$product_code)] <- "Laptop"

# create a full address to enable Geo-coding
# by combining the address, city and state separated by a comma
products <- mutate(products,full_address=paste(address,",",city,",",country))
# and check the results
select(products,c("address","city","country","full_address")) %>% print(n=25)

# create binary category columns for company and default to 0
products <- mutate(products,company_philips=0)
products <- mutate(products,company_akzo=0)
products <- mutate(products,company_van_houten=0)
products <- mutate(products,company_unilever=0)

# Set the company category column values to 1 if the corresponding company name is present
products$company_akzo[products$company=="akzo"] <- 1
products$company_philips[products$company=="philips"] <- 1
products$company_van_houten[products$company=="van houten"] <- 1
products$company_unilever[products$company=="unilever"] <- 1

# check the results
select(products,contains("company")) %>% print(n=25)

# I was wondering if there was a one line way to do this and tried this, doesn't work:
# products <- mutate(products,product_tablet2=(if(products$product_category=="Tablet"){1}else{0}))
# Get the following error:
#   Error in mutate_impl(.data, dots) : 
#   Evaluation error: argument is of length zero.
# But this did work:
# products <- mutate(products,product_tablet2=ifelse(products$product_category=="Tablet",1,0))

# create binary category columns for product and set to 1 if the category is present, else 0
products <- mutate(products,product_smartphone=ifelse(product_category=="Smartphone",1,0))
products <- mutate(products,product_tv=ifelse(product_category=="TV",1,0))
products <- mutate(products,product_laptop=ifelse(product_category=="Laptop",1,0))
products <- mutate(products,product_tablet=ifelse(product_category=="Tablet",1,0))

# set the product category column values to 1 if the corresponding product is present
#products$product_smartphone[products$product_category=="Smartphone"] <- 1
#products$product_laptop[products$product_category=="Laptop"] <- 1
#products$product_tv[products$product_category=="TV"] <- 1
# products$product_tablet[products$product_category=="Tablet"] <- 1 



# check the results using different selects
select(products,contains("product")) %>% print(n=25)
# The below was working
select(products,9,15:18) %>% print(n=25)
# but now getting error:
#   Error in combine_vars(vars, ind_list) : Position must be between 0 and n
# It's working again. I had an error in the mutate functions 
# and was not actually creating the new columns, so the 15:18 range was invalid.
select(products,contains("product"),
       -Product.code...number,
       -product_code,
       -product_number) %>% 
  print(n=25)
View(products)

# Lets look at the final structure for fun
str(products)

# Save the results in a CSV file
write.csv(products, file=outfile,row.names=FALSE)
