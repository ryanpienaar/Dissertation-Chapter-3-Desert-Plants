library(dplyr)
library(tidyr)

rush_cover <- read.csv("C:/Users/ryanp/Documents/Academia/Dissertation/Raw data/Great basin/Rush_Veg_Cover/Rush Cover All.csv")

### How many unique species do we have
unique(rush_cover$First.Foliar)
unique(rush_cover$Second.Foliar)
unique(rush_cover$third.foliar)
unique(rush_cover$Basal)
unique(rush_cover$Canopy)

# Lets go through em one by one,
rush_cover[rush_cover == "NF"] <- 0
rush_cover[rush_cover == "NC"] <- 0
rush_cover[rush_cover == ""] <- 0
rush_cover[rush_cover == "? (New Plant)"] <- 0
rush_cover[rush_cover == "DF"] <- 0
rush_cover[rush_cover == "DS"] <- 0
rush_cover[rush_cover == "WL"] <- 0
rush_cover[rush_cover == "DG"] <- 0
rush_cover[rush_cover == "BT"] <- 0
rush_cover[rush_cover == "R"] <- 0
rush_cover[rush_cover == "W"] <- 0
rush_cover[rush_cover == "LC"] <- 0
rush_cover[rush_cover == "PG"] <- 0
rush_cover[rush_cover == "BL"] <- 0
rush_cover[rush_cover == "AL"] <- 0
rush_cover[rush_cover == "CY"] <- 0
rush_cover[rush_cover == "M"] <- 0
rush_cover[rush_cover == "SW"] <- 0
rush_cover[rush_cover == "TT"] <- 0
rush_cover[rush_cover == "MCY"] <- 0
rush_cover[rush_cover == "CS"] <- 0
rush_cover[rush_cover == "-"] <- 0
rush_cover[rush_cover == "BG"] <- 0
rush_cover[rush_cover == "K"] <- 0
rush_cover[rush_cover == "C"] <- 0
rush_cover[rush_cover == "SL"] <- 0
rush_cover[rush_cover == "LI"] <- 0
rush_cover <- rush_cover %>%
  mutate(across(Canopy:Basal, ~ replace(., . == "BS", "0")))
rush_cover[rush_cover == "LL"] <- 0
rush_cover[rush_cover == "SLC"] <- 0
rush_cover[rush_cover == "S"] <- 0
rush_cover[rush_cover == "[DG]"] <- 0
rush_cover[rush_cover == "S "] <- 0
rush_cover[rush_cover == "[DF]"] <- 0
rush_cover[rush_cover == "[DS]"] <- 0
rush_cover[rush_cover == "BT(ant hill)"] <- 0
rush_cover[rush_cover == "WC"] <- 0
rush_cover[rush_cover == "Lichen"] <- 0
rush_cover[rush_cover == "Lichen White"] <- 0
rush_cover[rush_cover == "DC"] <- 0
rush_cover[rush_cover == "YL"] <- 0
rush_cover[rush_cover == "CT"] <- 0
rush_cover[rush_cover == "N"] <- 0
rush_cover[rush_cover == "WY"] <- 0
rush_cover[rush_cover == "LS"] <- 0
rush_cover[rush_cover == "F"] <- 0
rush_cover[rush_cover == "G"] <- 0
rush_cover[rush_cover == "T"] <- 0
rush_cover[rush_cover == "BIO* (ant hill)"] <- 0
rush_cover[rush_cover == "CYANOBACTE."] <- 0
rush_cover[rush_cover == "SOIL"] <- 0
rush_cover[rush_cover == "soil"] <- 0
rush_cover[rush_cover == "wood"] <- 0
rush_cover[rush_cover == "soil "] <- 0
rush_cover[rush_cover == "s"] <- 0
rush_cover[rush_cover == "AGCR (CRSTED WHEAT GRASS"] <- "AGCR"
rush_cover[rush_cover == "BTE"] <- 0
rush_cover[rush_cover == "FD"] <- 0
rush_cover[rush_cover == "AN"] <- 0
rush_cover[rush_cover == "B"] <- 0
rush_cover[rush_cover == "AG"] <- 0
rush_cover[rush_cover == "BIO* (spiderweb)"] <- 0
rush_cover[rush_cover == "l"] <- 0
rush_cover[rush_cover == "HNC"] <- 0
rush_cover[rush_cover == "Dead Wood"] <- 0
rush_cover[rush_cover == "BARE "] <- "BARE"
rush_cover[rush_cover == "LICHEN(WHITE)"] <- 0
rush_cover[rush_cover == "LICHEN(YELLOW)"] <- 0
rush_cover[rush_cover == "Orange lichen"] <- 0
rush_cover[rush_cover == "UK"] <- "Other"
rush_cover[rush_cover == "UKI"] <- "Other"
rush_cover[rush_cover == "litter"] <- "LITTER"
rush_cover[rush_cover == "Litter"] <- "LITTER"
rush_cover[rush_cover == "L"] <- "LITTER" 
rush_cover[rush_cover == "L "] <- "LITTER"
rush_cover[rush_cover == "[L]"] <- "LITTER"
rush_cover[rush_cover == "LTTER"] <- "LITTER"
rush_cover[rush_cover == "LITER"] <- "LITTER"
rush_cover[rush_cover == "Dead wood"] <- "LITTER"
rush_cover[rush_cover == "DS/BRTE"] <- "BRTE"
rush_cover[rush_cover == "BRT"] <- "BRTE"
rush_cover[rush_cover == "RTE"] <- "BRTE"
rush_cover[rush_cover == "BRET"] <- "BRTE"
rush_cover[rush_cover == "CETE 5"] <- "CETE"
rush_cover[rush_cover == "CETE "] <- "CETE"
rush_cover[rush_cover == "CETTE"] <- "CETE"
rush_cover[rush_cover == "ELEL 5"] <- "ELEL"
rush_cover[rush_cover == "[ELEL]"] <- "ELEL"
rush_cover[rush_cover == "[ARTR]"] <- "ARTR"
rush_cover[rush_cover == "sage bush"] <- "ARTR"
rush_cover[rush_cover == "sage bush "] <- "ARTR"
rush_cover[rush_cover == "SAIL"] <- "SIAL"

head(rush_cover)

# Replace 'rush_cover' with your actual dataframe name
df_presence <- rush_cover %>%
  mutate(across(Canopy:Basal, as.character))   # Ensure species columns are characters


# Get all unique species (excluding NA values)
species <- unique(unlist(df_presence[, c("Canopy", "First.Foliar", "Second.Foliar", "third.foliar", "Basal")]))
species <- species[!is.na(species)]  # Remove NA values

# Add species columns and initialize with 0
df_presence[species] <- 0

head(df_presence)

# Create a vector of species columns (from BRTE onward)
species_columns <- colnames(df_presence)[which(colnames(df_presence) == "BRTE"):ncol(df_presence)]

# Define the range of columns for Canopy to Basal
canopy_to_basal_columns <- c("Canopy", "First.Foliar", "Second.Foliar", "third.foliar", "Basal")

# Loop through each row
for(i in 1:nrow(df_presence)) {
  # Loop through each species column (BRTE onward)
  for(species in species_columns) {
    # If the species column name exists in any of the columns from Canopy to Basal, set it to 1
    if(species %in% df_presence[i, canopy_to_basal_columns]) {
      df_presence[i, species] <- 1
    }
  }
}

summary(df_presence)


# Group by DATE, Block, and Plot, then calculate the percentage of occurrences
df_summary <- df_presence %>%
  group_by(DATE, Block, Plot) 

df_summary[is.na(df_summary)] = 0
df_summary <- df_summary |>
  mutate(Plot = ifelse(Plot == 0, "BS", Plot))

head(df_summary)

df_summary <- df_summary %>%
  group_by(DATE, Block, Plot) %>%
  summarise(
    row_count = n(),  # Count the number of rows for each group
    across(BRTE:BARE, sum, na.rm = TRUE)  # Sum species columns
  )
write.csv(df_summary, "rushcovertotals.csv")

head(df_summary)


df_new <- df_summary

# Loop through columns from BRTE onward
for (col in colnames(df_summary)[which(colnames(df_summary) == "BRTE"):ncol(df_summary)]) {
  # For each row, divide the column value by row_count, multiply by 100, and round to 2 decimal places
  df_new[[col]] <- round((df_new[[col]] / df_new$row_count) * 100, 2)
}
summary(df_new)


## Do total
rush_cover_total <- df_presence
rush_cover_total$Total <- rowSums(rush_cover_total[,c(11:47)])
rush_cover_total$Total[rush_cover_total$Total >= 1] <- 1


# Group by DATE, Block, and Plot, then calculate the percentage of occurrences
rush_cover_total<- rush_cover_total %>%
  group_by(DATE, Block, Plot) 

rush_cover_total[is.na(rush_cover_total)] = 0
rush_cover_total <- rush_cover_total |>
  mutate(Plot = ifelse(Plot == 0, "BS", Plot))

rush_cover_total <- rush_cover_total %>%
  group_by(DATE, Block, Plot) %>%
  summarise(
    row_count = n(),  # Count the number of rows for each group
    across(BRTE:Total, sum, na.rm = TRUE)  # Sum species columns
  )

df_new <- rush_cover_total


# Loop through columns from BRTE onward
for (col in colnames(rush_cover_total)[which(colnames(rush_cover_total) == "BRTE"):ncol(rush_cover_total)]) {
  # For each row, divide the column value by row_count, multiply by 100, and round to 2 decimal places
  df_new[[col]] <- round((df_new[[col]] / df_new$row_count) * 100, 2)
}


names(df_new)[5:ncol(df_new)] <- paste0(names(df_new)[5:ncol(df_new)], ".cover")

write.csv(df_new, "rushcoverpercent.csv")
