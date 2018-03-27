# Springboard Data Science Foundation Class
# Exercise 01 - Data Wrangling - Refine Data
# Clean up and extend data in an practice dataset of product information.


library(devtools)
library(tidyr)
library(dplyr)
# # Read CSV into R
#MyData <- read.csv(file="refine_original.csv", header=TRUE, sep=",")
refineData <- read.csv(file="C:/Users/Tom/git/datasciencefoundation/DataWrangleExer01/refine_original.csv", header=TRUE, sep=",")
#print(refineData)

#View(MyData)
#str(refineData)
products <- tbl_df(refineData) 

#clean up company names
# Set company name to 'adzo', philips', 'unilever', or 'van houten' 
products$company[grepl("^a",products$company,ignore.case=TRUE)] <- "akzo"
products$company[grepl("^p|^f",products$company,ignore.case=TRUE)] <- "philips"
products$company[grepl("^u",products$company,ignore.case=TRUE)] <- "unilever"
products$company[grepl("^v",products$company,ignore.case=TRUE)] <- "van houten"

products <- separate(products,Product.code...number,
                     c("product_code","product_number"),
                     sep="-",
                     remove=FALSE)

products <- mutate(products,product_category=product_code)
products$product_category[grepl("p",products$product_code)] <- "Smartphone"
products$product_category[grepl("q",products$product_code)] <- "Tablet"
products$product_category[grepl("v",products$product_code)] <- "TV"
products$product_category[grepl("x",products$product_code)] <- "Laptop"

products <- mutate(products,full_address=paste(address,",",city,",",country))

products <- mutate(products,company_philips=0)
products <- mutate(products,company_akzo=0)
products <- mutate(products,company_van_houten=0)
products <- mutate(products,company_unilever=0)
products <- mutate(products,product_smartphone=0)
products <- mutate(products,product_tv=0)
products <- mutate(products,product_laptop=0)
products <- mutate(products,product_tablet=0)

products$company_akzo[products$company=="akzo"] <- 1
products$company_philips[products$company=="philips"] <- 1
products$company_van_houten[products$company=="van houten"] <- 1
products$company_unilever[products$company=="unilever"] <- 1

select(products,contains("company")) %>% print(n=25)

products$product_smartphone[products$product_category=="Smartphone"] <- 1
products$product_laptop[products$product_category=="Laptop"] <- 1
products$product_tv[products$product_category=="TV"] <- 1
products$product_tablet[products$product_category=="Tablet"] <- 1 

# this doesn't work:
# products <- mutate(products,product_tab2=(if(products$product_category=="Tablet"){1}else{0}))

select(products,contains("product")) %>% print(n=25)
select(products,9,15:18) %>% print(n=25)
select(products,contains("product"),
       -Product.code...number,
       -product_code,
       -product_number) %>% 
  print(n=25)

#str(products)

# Write CSV in R
write.csv(products, file ="C:/Users/Tom/git/datasciencefoundation/DataWrangleExer01/refine_clean.csv",row.names=FALSE)
print(products)

View(products)

