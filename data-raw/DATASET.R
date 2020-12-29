## Load raw data from .csv file

# A taxon list, which gives the row names in a taxon-PFT matrix
exTaxonList <- read.csv2("data-raw/taxon_list.csv", header = TRUE, # row.names = 1,
                        sep = ";", quote = "\"", dec = ".", fill = TRUE,
                        comment.char = "")

# A PFT list, which gives the column names in a taxon-PFT matrix
exPftList <- read.csv2("data-raw/pft_list.csv", header = TRUE, # row.names = 1,
                      sep = ";", quote = "\"", dec = ".", fill = TRUE,
                      comment.char = "")

## Save the data in the required R package location

usethis::use_data(exTaxonList)

usethis::use_data(exPftList)
