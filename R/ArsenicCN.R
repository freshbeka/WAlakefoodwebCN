# This script takes the trophic transfer results and plots them with isotopes


library(googledrive) #to read in the data directly from googledrive
library(readxl) #to convert the excel file into a R object

##First, call the file from the google drive and save it in the data file location
drive_download("https://docs.google.com/spreadsheets/d/1NhbrwkDI7Karzcyk_s3CepfQbVTc3vRT/edit#gid=574100294", 
               path = "/Users/rebekahstiling/Desktop/RProjects/WAlakefoodwebCN/data/Trophic.xlsx",
               overwrite = TRUE)

## The file was saved locally in the project, now I want to read it in with excel

#check the sheets
excel_sheets("data/Trophic.xlsx")

trophic_messy <- read_excel("data/Trophic.xlsx", sheet = "results", skip = 1)

#Now I want to separate out the pieces of data that I know I need, and clean up the tibble



