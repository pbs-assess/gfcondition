species_list <- list(
  "Pacific Spiny Dogfish", "North Pacific Spiny Dogfish", # due to renaming
  "Pacific Ocean Perch",
  "Pacific Cod",
  "Walleye Pollock",
  "Sablefish",
  "Lingcod",
  "Bocaccio",
  "Canary Rockfish",
  "Quillback Rockfish",
  "Redbanded Rockfish",
  "Redstripe Rockfish",
  "Silvergray Rockfish",
  "Shortspine Thornyhead",
  "Widow Rockfish",
  "Yelloweye Rockfish",
  "Yellowmouth Rockfish",
  "Yellowtail Rockfish",
  "Shortraker Rockfish",
  "Rosethorn Rockfish",
  "Sharpchin Rockfish",
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  "Petrale Sole",
  "Arrowtooth Flounder",
  "English Sole",
  "Dover Sole",
  "Rex Sole",
  "Flathead Sole",
  "Southern Rock Sole",
  "Slender Sole",
  "Pacific Sanddab",
  "Pacific Halibut",
  "Pacific Hake",
  "Spotted Ratfish",
  "Longnose Skate",
  "Big Skate",
  "Sandpaper Skate"
)

# remove species due to insufficient data
species_to_remove <- c(
  "Pacific Halibut",
  "Sandpaper Skate"
)

Flatfish<- c(
  "Curlfin Sole",#
  "Butter Sole",
  "Sand Sole",#
  "Petrale Sole", #
  "Arrowtooth Flounder", #
  "English Sole",#
  "Dover Sole",#
  "Rex Sole", #
  "Flathead Sole",#
  "Southern Rock Sole",#
  "Slender Sole",#
  "Pacific Sanddab",#
  "Pacific Halibut"#
)

Rockfish<- c(
  "Bocaccio",
  "Canary Rockfish",
  "Pacific Ocean Perch",
  "Quillback Rockfish",
  "Redbanded Rockfish",
  "Redstripe Rockfish", #
  "Rougheye/Blackspotted Rockfish Complex", #
  "Silvergray Rockfish", #
  "Shortspine Thornyhead",
  "Widow Rockfish", #
  "Yelloweye Rockfish",
  "Yellowmouth Rockfish", #
  "Yellowtail Rockfish",
  "Copper Rockfish",
  "Shortraker Rockfish",
  "Rosethorn Rockfish",
  "Harlequin Rockfish",
  "Pygmy Rockfish",
  "Sharpchin Rockfish",
  "Darkblotched Rockfish",
  "Greenstriped Rockfish"
)

flatfish <- tolower(Flatfish)
rockfish <- tolower(Rockfish)

