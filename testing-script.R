#######################
# R scripts for engineering data summary
# for Jonathan Torres lab
# summer 2024, v0.1
#######################

# example for reading files from directory
library(tidyverse)
library(janitor)

# setting the working directory to the data folder in current directory
setwd("./data")
files <- list.files()
files

# character vector of expected column names for import
column_names <- c("Time",
                  "Displacement",
                  "Force",
                  "LVDT Displacement",
                  "LVDT(Strain 1)",
                  "Displacement (Balanced)",
                  "Displacement (compliance corrected)")


# importing files into a list structure in memory
# NB: the additional arguments are sent to read_csv via ...
# anonymous function alternative map(\(x) f(x, 1, 2, collapse = ","))
data_list <- map(files, read_csv, col_names = column_names, skip = 20)
data_list

# tibblizing the data for easier manipulations?
tb.data.list <- tibble(dl = data_list, files)
tb.data.list

# what would just unnest do
# this works as hoped and yields a combined data frame of observations across all samples
tb.combined <- tb.data.list %>%
  unnest(dl) %>%
  clean_names()
#  count(files)

# max forces, eliminating ties
max.forces <- tb.combined %>%
  select(force, displacement, lvdt_displacement, files) %>%
  group_by(files) %>%
  slice_max(force, with_ties = FALSE) 


# summary statistics
tb.combined %>%
  select(force, displacement, lvdt_displacement, files) %>%
  group_by(files) %>%
  slice_max(force, with_ties = FALSE) %>%
  mutate(force = scales::number(force, accuracy = 0.0001),
        displacement = scales::number(displacement, accuracy = 0.0001),
        lvdt_displacement = scales::number(lvdt_displacement, accuracy = 0.0001)) 

         
#  summarise(max(force))


############################################## graphing
# 
tb.combined %>%
  ggplot(aes(
    x = displacement,
    y = force,
    colour = files
  )) +
  geom_point() +
  geom_vline(data = max.forces, aes(xintercept = displacement, colour = files), linetype = "dashed")

# a graph for each file / run, but still with shared vertical axis
tb.combined %>%
  ggplot(aes(
    x = displacement,
    y = force,
    colour = files
  )) +
  geom_point() +
  facet_grid(cols = vars(files)) +
  geom_hline(data = max.forces, aes(yintercept = force, color = files), linetype = "dashed")

###----------------- working to here








###------------------ previous
################ previous

# previous
# new data frame from the first element of the data list
data_imported_1 <- select(data_list[[1]], Time, Displacement, Force) %>%
  filter(!row_number() %in% c(1))


# checking the data import process
head(data_imported)
glimpse(data_imported)

# new data frame from second element in list
data_imported_2 <- select(data_list[[2]], Time, Displacement, Force)

glimpse(data_imported_2)

# quick graph example
data_imported_1 %>%
  ggplot(aes(
    x = as.numeric(Time),
    y = as.numeric(Displacement)
  )) +
  geom_point() +
  scale_x_time()

data_imported_2 %>%
  ggplot(aes(
    x = Displacement,
    y = Force
  )) +
  geom_point()

# tibble experiments
#################### experiments
# test unnest_wider
tb.data.list %>%
  filter(files == "sample1.csv") %>%
  unnest_wider(dl)

# failed experiment, yields a tibble with the columns I want, but data is still in lists
tb.data.list %>%
  filter(files == "sample1.csv") %>%
  hoist(dl,
        time = "Time",
        displacement = "Displacement",
        force = "Force")