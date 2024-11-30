# overwrite for certain species

replace_with_custom_maturity <- function(species){

if (species == "North Pacific Spiny Dogfish") {
  custom_maturity_code <<- c(NA, 55)
  # # custom_length_threshold <- c(70.9, 86.2)
  # # set_family <- delta_lognormal_mix()
  # # set_family2 <- delta_lognormal()
}

if (species == "Big Skate") {
  # # McFarlane and King 2006 -- shouldn't be relied on
  # Ebert et al. 2008
  custom_length_threshold <<- c(102.9, 113.1)
}

if (species == "Longnose Skate") {
  # # McFarlane and King 2006
  # custom_length_threshold <- c(65, 83)
  # Arrington 2020
  custom_length_threshold <<- c(85, 102)
}

if (species == "Sandpaper Skate") {
  # Perez 2005
  custom_length_threshold <<- c(49.2, 46.7)
}

if (species == "Spotted Ratfish") {
  # King and McPhie 2015
  custom_length_threshold <<- c(30.2, 39.3)
}

if (species == "Shortraker Rockfish") {
  # McDermott 1994:  Hutchinson 2004 F 44.9
  # Conrath 2017 for female,
  # Haldorson and Love reported 47 but based on
  # Westrheim, 1975 for both sexes = 45
  custom_length_threshold <<- c(45, 49.9)
}

if (species == "Pacific Halibut") {
  # age at 50% lowered to 10.3 years according to histology
  # Takada 2017 thesis for females, males less precise.. btw 70-79
  # no accurate values for males available so wont use for now
  custom_length_threshold <<- c(65, 96.7)
}
}
