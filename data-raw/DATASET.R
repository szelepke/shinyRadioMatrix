## Load raw data from .csv file

# A taxon list, which gives the row names in a taxon-PFT matrix
taxon_list <- read.csv2("data-raw/taxon_list.csv", header = TRUE, # row.names = 1,
                        sep = ";", quote = "\"", dec = ".", fill = TRUE,
                        comment.char = "")

# A PFT list, which gives the column names in a taxon-PFT matrix
pft_list <- read.csv2("data-raw/pft_list.csv", header = TRUE, # row.names = 1,
                      sep = ";", quote = "\"", dec = ".", fill = TRUE,
                      comment.char = "")

## Save the data in the required R package location

usethis::use_data(taxon_list)

usethis::use_data(pft_list)
