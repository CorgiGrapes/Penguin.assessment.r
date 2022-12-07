##creating the fucntion to clean the headings
cleaning <- function(data_raw){
  data_raw %>%
    select(-starts_with("delta")) %>%
    remove_empty(c("rows","cols")) %>%
    select(-starts_with("X")) %>%
    select(-Comments) %>%
    clean_names() ## Using the clean.names() function from the package janitor to edit the column names
}


#creating another function to subset the flipper data and remove missing data
remove_empty_flipper_length <- function(data_clean){
  data_clean %>%
    filter(!is.na(flipper_length_mm)) %>%
    select(species, flipper_length_mm)
}
#So here it starts with a data table. It looks for any rows where the flipper_length_mm is NA, and removes it. 
#Then it only keeps the species and flipper_length_mm columns.

#Making another function to subset the data and remove empty rows in body mass
remove_empty_body_mass <- function(data_clean){
  data_clean %>%
    filter(!is.na(body_mass_g)) %>%
    select(species, body_mass_g, culmen_length_mm)
}

#Making another function to subset the data and remove empty rows in culmen length
remove_empty_culmen_length <- function(data_clean){
  data_clean %>%
    filter(!is.na(culmen_length_mm)) %>%
    select(species, body_mass_g, culmen_length_mm)
}

# Creating a new function and saving it to the cleaning file
cleaning_new <-function(data_raw){
  data_raw %>%
    cleaning() %>%
    remove_empty_body_mass() %>%
    remove_empty_culmen_length()
}



