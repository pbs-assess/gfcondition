species_list <- list(
  "Pacific Spiny Dogfish",
  "North Pacific Spiny Dogfish",
  "Pacific Ocean Perch",
  "Pacific Cod",
  "Walleye Pollock",
  "Sablefish",
  "Lingcod",
  "Bocaccio",
  "Canary Rockfish",
  "Quillback Rockfish",
  "Redbanded Rockfish",
  "Redstripe Rockfish", #
  # "Rougheye/Blackspotted Rockfish Complex", #
  "Silvergray Rockfish", #
  "Shortspine Thornyhead",
  "Widow Rockfish", #
  "Yelloweye Rockfish",
  "Yellowmouth Rockfish", #
  "Yellowtail Rockfish",
  ## "Copper Rockfish",
  "Shortraker Rockfish",
  "Rosethorn Rockfish",
  # "Harlequin Rockfish",
  # "Pygmy Rockfish",
  "Sharpchin Rockfish",
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  "Petrale Sole", #
  "Arrowtooth Flounder", #
  "English Sole",#
  "Dover Sole",#
  "Rex Sole", #
  "Flathead Sole",#
  "Southern Rock Sole",#
  "Slender Sole",#
  "Pacific Sanddab",#
  "Pacific Halibut",#
  # "Butter Sole",
  "Pacific Hake",#
  # "Pacific Tomcod",
  "Spotted Ratfish",
  "Longnose Skate",
  "Big Skate",
  "Sandpaper Skate"
  # "Curlfin Sole", #
  # "Sand Sole", #
  # # ## "Starry Flounder"# too few males!
  # # ## "C-O Sole", # way too few!
  # # ## "Deepsea Sole" # no maturity
)

# ## remove species?
species_to_remove <- c(
  "Pacific Halibut",
  # "Slender Sole",
  "Curlfin Sole",
  # "Sand Sole",
  "Sandpaper Skate",
  "Harlequin Rockfish",
  "Pacific Tomcod",
  "Pygmy Rockfish",
  "Rougheye/Blackspotted Rockfish Complex", #
  "Butter Sole" # samples only from HS and strange behaviour of RF
)

# might need additional species removed for imm?
# e.g. Hake

# Flatfish <- c(
#   "Curlfin Sole",#
#   "Butter Sole",
#   "Sand Sole",#
#   "Petrale Sole", #
#   "Arrowtooth Flounder", #
#   "English Sole",#
#   "Dover Sole",#
#   "Rex Sole", #
#   "Flathead Sole",#
#   "Southern Rock Sole",#
#   "Slender Sole",#
#   "Pacific Sanddab",#
#   "Pacific Halibut"#
# )
#
# Rockfish <- c(
#   "Pacific Ocean Perch",
#   "Bocaccio",
#   "Canary Rockfish",
#   "Redstripe Rockfish", # MSA added with mean > 4
#   "Rougheye/Blackspotted Rockfish Complex", # WILL NEED UPDATE FOR ALL MAT CLASSES
#   "Silvergray Rockfish", # MSA added with mean > 5
#   "Shortspine Thornyhead",
#   "Widow Rockfish", # hake would need mean > 1, mssm1 > 4
#   "Yelloweye Rockfish",
#   "Yellowmouth Rockfish", #
#   "Yellowtail Rockfish"
# )

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

