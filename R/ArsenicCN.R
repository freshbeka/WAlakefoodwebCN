# This script takes the trophic transfer results and plots them with isotopes


library(googledrive) #to read in the data directly from googledrive
library(readxl) #to convert the excel file into a R object
library(janitor) # to clean up rownames

##First, call the file from the google drive and save it in the data file location
drive_download("https://docs.google.com/spreadsheets/d/1NhbrwkDI7Karzcyk_s3CepfQbVTc3vRT/edit#gid=574100294", 
               path = "/Users/rebekahstiling/Desktop/RProjects/WAlakefoodwebCN/data/Trophic.xlsx",
               overwrite = TRUE)

## The file was saved locally in the project, now I want to read it in with excel

#check the sheets
excel_sheets("data/Trophic.xlsx")

trophic_messy <- read_excel("data/Trophic.xlsx", 
                            sheet = "results", 
                            skip = 1,
                            .name_repair = make_clean_names)

#Now I want to separate out the pieces of data that I know I need, and clean up the tibble

#First I want to consolodate the values that have C & N

isotopes_messy <- trophic_messy %>% drop_na(uc_davis_id)

#some rows have the column names repeated, so remove them
isotopes_messy <- isotopes_messy %>% filter(species != "Species")

#change some columns from character to numeric
cols.num <- c("fillet_as", "liver_as", "gill_as","d13cvpdb","d15n_air")
isotopes_messy[cols.num] <- sapply(isotopes_messy[cols.num],as.numeric)


# start with plotting the C & N from Angle

ggplot(data = isotopes_messy %>% filter(lake == "Angle"), aes(x = d13cvpdb, y = d15n_air, shape = species)) +
  geom_point(aes(size = fillet_as, color = -fillet_as)) +
  scale_shape_manual(values=seq(2,8)) +
  theme_minimal() 

ggplot(data = isotopes_messy %>% filter(lake == "Killarney"), aes(x = d13cvpdb, y = d15n_air, shape = species)) +
  geom_point(aes(size = fillet_as)) +
  scale_shape_manual(values=seq(2,8)) +
  theme_minimal() 

ggplot(data = isotopes_messy %>% filter(lake == "Angle"), aes(x = d13cvpdb, y = d15n_air, color = species)) +
  geom_point(aes(size = fillet_as, color = species), alpha = .75) +
  scale_size(range = c(0.5, 12))  +
  theme_minimal() +
  labs(title = "Angle Lake", x = "d13C", y = "d15N")

ggplot(data = isotopes_messy %>% filter(lake == "Killarney"), aes(x = d13cvpdb, y = d15n_air, color = species)) +
  geom_point(aes(size = fillet_as, color = species), alpha = .75) +
  scale_size(range = c(0.5, 12))  +
  theme_minimal() +
  labs(title = "Killarney Lake", x = "d13C", y = "d15N")

#combine with facet wrap

ggplot(data = isotopes_messy, aes(x = d13cvpdb, y = d15n_air, color = species)) +
  geom_point(aes(size = fillet_as, color = species), alpha = .75) +
  scale_size(range = c(0.5, 12))  +
  facet_grid(. ~ lake) +
  theme_bw() +
  labs(x = "d13C", y = "d15N", size = "fillet As (ug/g)" )

#ggsave("figs/comparAs.png", width = 10, height = 6, units = "in" )


