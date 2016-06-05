## Exercise 3.1

# load 'refine_original.csv' file
ro<-read.csv("refine_original.csv")
View(ro)

# Clean up brand names
ro$company<-tolower(ro$company)

# Separate product code and number into separate columns
# Load tidyr package to use separate function
install.packages(tidyr)
# Get the column names:
colnames(ro)
# Separate the second column using the separate() function
ro_sep<-separate(ro, "Product.code...number", c("prodcode", "prodnumber"), sep="-")
View(ro_sep)

# Add a column 'prodtype' to specify which type of product each code corresponds to, 
# based on: p=Smartphone, v=TV, x=Laptop, q=Tablet
# Create an empty column named 'prodtype'
ro_sep$prodtype <- rep(NA, nrow(ro_sep))
# Replace elements of prodtype by a specific product type based on the coding scheme listed above, and the 
# value shown in prodcode.
ro_sep[ro_sep$prodcode == "p",][, "prodtype"]<-"Smartphone"
ro_sep[ro_sep$prodcode == "x",][, "prodtype"]<-"Laptop"
ro_sep[ro_sep$prodcode == "v",][, "prodtype"]<-"TV"
ro_sep[ro_sep$prodcode == "q",][, "prodtype"]<-"Tablet"

# Add full address for geoocoding
# Create an empty column, 'full_address'
ro_sep$full_address <- rep(NA, nrow(ro_sep))
# Concatenate three columns (address, city, country), separated by commas, to 
# create an easily geocoded address -- using the unite() function through tidyr
ro_unite<-unite(ro_sep, "full_address", address, city, country, sep=", ")
View(ro_unite)

## Create dummy variables for company and product category
# Company:
ro_unite$company_philips <- rep(NA, nrow(ro_unite))
ro_unite$company_akzo <- rep(NA, nrow(ro_unite))
ro_unite$company_van_houten <- rep(NA, nrow(ro_unite))
ro_unite$company_unilever <- rep(NA, nrow(ro_unite))

# Use grepl to determine whether the 'company' column does/does not
# contain the relevant company name (based on a string unique to that 
# company name), and assign a 1/0 to the associated company-specific column
# accordingly.
ro_unite$company_philips<-as.numeric(grepl("^ph", ro_unite$company))
ro_unite$company_akzo<-as.numeric(grepl("^ak", ro_unite$company))
ro_unite$company_van_houten<-as.numeric(grepl("^va", ro_unite$company))
ro_unite$company_unilever<-as.numeric(grepl("^un", ro_unite$company))

head(ro_unite)
# Do the same for product type:
# Create empty columns for the binary vectors
ro_unite$product_smartphone <- rep(NA, nrow(ro_unite))
ro_unite$product_tv <- rep(NA, nrow(ro_unite))
ro_unite$product_laptop <- rep(NA, nrow(ro_unite))
ro_unite$product_tablet <- rep(NA, nrow(ro_unite))

# Fill in the 1/0 for these columns based on the product type found in the prodtype column
ro_unite$product_smartphone<-as.numeric(grepl("Smartphone", ro_unite$prodtype))
ro_unite$product_tv<-as.numeric(grepl("TV", ro_unite$prodtype))
ro_unite$product_laptop<-as.numeric(grepl("Laptop", ro_unite$prodtype))
ro_unite$product_tablet<-as.numeric(grepl("Tablet", ro_unite$prodtype))

# Write the resulting file to 'refine_clean' using write.csv():
# Specify 'row.names' as FALSE to prevent automatic numeric naming of the rows
write.csv(ro_unite, "refine_clean.csv", row.names=FALSE)
