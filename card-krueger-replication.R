library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(sjlabelled)
library(ggrepel)
library(scales)
library(ggpubr)
library(plm)
library(lmtest)
library(stringr)


# Temporary file and path
tfile_path <- tempfile()
tdir_path <- tempdir()

# Download zip file
download.file("http://davidcard.berkeley.edu/data_sets/njmin.zip", 
              destfile = tfile_path)

# Unzip
unzip(tfile_path, exdir = tdir_path)

# Read codebook
codebook <- read_lines(file = paste0(tdir_path, "/codebook"))

# Generate a vector with variable names
variable_names <- codebook %>%
  `[`(8:59) %>% #  Element 8 (sheet)
  `[`(-c(5, 6, 13, 14, 32, 33)) %>% # 
  str_sub(1, 8) %>% # 
  str_squish() %>% # Whitespaces 
  stringr::str_to_lower() # 

# Generate a vector with variable labels
variable_labels <- codebook %>%
  `[`(8:59) %>% # variable names start at element 8 (sheet)
  `[`(-c(5, 6, 13, 14, 32, 33)) %>% # remove elements w/o variable names
  sub(".*\\.[0-9]", "", .) %>%
  `[`(-c(5:10))  %>% # these elements are combined later on
  str_squish() # remove white spaces

# Region
variable_labels[41] <- "region of restaurant"

# Read raw data
data_raw <- read_table2(paste0(tdir_path, "/public.dat"),
                        col_names = FALSE)

# Add variable names
data_mod <- data_raw %>%
  select(-X47) %>% # remove empty column
  `colnames<-`(., variable_names) %>% # Assign variable names
  mutate_all(as.numeric) %>% # treat all variables as numeric
  mutate(sheet = ifelse(sheet == 407 & chain == 4, 408, sheet)) # duplicated sheet id 407

# Process data (currently wide format)
data_mod <- data_mod %>%
  # chain value label
  mutate(chain = case_when(chain == 1 ~ "bk",
                           chain == 2 ~ "kfc",
                           chain == 3 ~ "roys",
                           chain == 4 ~ "wendys")) %>%
  # state value label
  mutate(state = case_when(state == 1 ~ "New Jersey",
                           state == 0 ~ "Pennsylvania")) %>%
  # Region dummy
  mutate(region = case_when(southj == 1 ~ "southj",
                            centralj == 1 ~ "centralj",
                            northj == 1 ~ "northj",
                            shore == 1 ~ "shorej",
                            pa1 == 1 ~ "phillypa",
                            pa2 == 1 ~ "eastonpa")) %>%
  # meals value label
  mutate(meals = case_when(meals == 0 ~ "none",
                           meals == 1 ~ "free meals",
                           meals == 2 ~ "reduced price meals",
                           meals == 3 ~ "both free and reduced price meals")) %>%
  # meals value label
  mutate(meals2 = case_when(meals2 == 0 ~ "none",
                            meals2 == 1 ~ "free meals",
                            meals2 == 2 ~ "reduced price meals",
                            meals2 == 3 ~ "both free and reduced price meals")) %>%
  # status2 value label
  mutate(status2 = case_when(status2 == 0 ~ "refused second interview",
                             status2 == 1 ~ "answered 2nd interview",
                             status2 == 2 ~ "closed for renovations",
                             status2 == 3 ~ "closed permanently",
                             status2 == 4 ~ "closed for highway construction",
                             status2 == 5 ~ "closed due to Mall fire")) %>%
  mutate(co_owned = if_else(co_owned == 1, "yes", "no")) %>%
  mutate(bonus = if_else(bonus == 1, "yes", "no")) %>%
  mutate(special2 = if_else(special2 == 1, "yes", "no")) %>%
  mutate(type2 = if_else(type2 == 1, "phone", "personal")) %>%
  select(-southj, -centralj, -northj, -shore, -pa1, -pa2) %>% # now included in region dummy
  mutate(date2 = lubridate::mdy(date2)) %>% # Convert date
  rename(open2 = open2r) %>% #Fit name to wave 1
  rename(firstinc2 = firstin2) %>% # Fit name to wave 1
  sjlabelled::set_label(variable_labels) # Add stored variable labels


# Structural variables
structure <- data_mod %>%
  select(sheet, chain, co_owned, state, region)

# Wave 1 variables
wave1 <- data_mod %>%
  select(-ends_with("2"), - names(structure)) %>%
  mutate(observation = "February 1992") %>%
  bind_cols(structure) 

# Wave 2 variables
wave2 <- data_mod %>%
  select(ends_with("2")) %>%
  rename_all(~str_remove(., "2"))  %>%
  mutate(observation = "November 1992") %>%
  bind_cols(structure) 

# Final dataset
card_krueger_1994 <- bind_rows(wave1, wave2) %>%
  select(sort(names(.))) %>% # Sort columns alphabetically
  sjlabelled::copy_labels(data_mod) # Restore variable labels

card_krueger_1994_mod <- card_krueger_1994 %>%
  mutate(emptot = empft + nmgrs + 0.5 * emppt,
         pct_fte = empft / emptot * 100)

card_krueger_1994_mod %>%
  select(chain, state) %>%
  table() %>%
  prop.table(margin = 2)  %>%
  apply(MARGIN = 2,
        FUN = scales::percent_format(accuracy = 0.1)) %>%
  noquote

card_krueger_1994_mod %>%
  filter(observation == "February 1992") %>%
  group_by(state) %>%
  summarise(emptot = mean(emptot, na.rm = TRUE),
            pct_fte  = mean(pct_fte, na.rm = TRUE),
            wage_st = mean(wage_st, na.rm = TRUE),
            hrsopen = mean(hrsopen, na.rm = TRUE)) %>%
  pivot_longer(cols=-state, names_to = "variable") %>%
  pivot_wider(names_from = state, values_from = value)

card_krueger_1994_mod %>%
  filter(observation == "November 1992") %>%
  group_by(state) %>%
  summarise(emptot = mean(emptot, na.rm = TRUE),
            pct_fte  = mean(pct_fte, na.rm = TRUE),
            wage_st = mean(wage_st, na.rm = TRUE),
            hrsopen = mean(hrsopen, na.rm = TRUE)) %>%
  pivot_longer(cols=-state, names_to = "variable") %>%
  pivot_wider(names_from = state, values_from = value)

differences <- card_krueger_1994_mod %>%
  group_by(observation, state) %>%
  summarise(emptot = mean(emptot, na.rm = TRUE))

# Treatment group (NJ) before treatment
njfeb <- differences[1,3]

# Control group (PA) before treatment
pafeb <- differences[2,3]

# Treatment group (NJ) after treatment
njnov <- differences[3,3]

# Control group (PA) after treatment
panov <- differences[4,3]

(njnov-njfeb)-(panov-pafeb) 

(njnov-panov)-(njfeb-pafeb)

nj_counterfactual <- tibble(
  observation = c("February 1992","November 1992"), 
  state = c("New Jersey (Counterfactual)","New Jersey (Counterfactual)"),
  emptot = as.numeric(c(njfeb, njfeb-(pafeb-panov)))
) 

# Data points for treatment event
intervention <- tibble(
  observation = c("Intervention", "Intervention", "Intervention"),
  state = c("New Jersey", "Pennsylvania", "New Jersey (Counterfactual)"),
  emptot = c(19.35, 22.3, 19.35)
) 

# Combine data
did_plotdata <- bind_rows(differences, 
                          nj_counterfactual, 
                          intervention)

Early <- did_plotdata %>% filter(Year <= 2000)
Late <- did_plotdata %>% filter(Year >= 2000)
ggplot(mapping = aes(Year, Value, color = Country, group = Country)) + 
  geom_line(linetype = 1, data = Early) + geom_line(linetype = 2, data = Late) 



did_plotdata %>%
  mutate(label = if_else(observation == "November 1992", as.character(state), NA_character_)) %>%
  ggplot(aes(x=observation,y=emptot, group=state)) +
  geom_line(aes(color=state), size=1.25) +
  geom_vline(xintercept = "Intervention", linetype="dotted", 
             color = "black", size=1) + 
  scale_color_brewer(palette = "Accent") +
  scale_y_continuous(limits = c(17,24)) +
  ggrepel::geom_label_repel(aes(label = label),
                            nudge_x = 0.5, nudge_y = -0.5,
                            na.rm = TRUE) +
  guides(color=FALSE) +
  labs(x="", y="Full Time Equivalent Employment (mean)") +
  annotate(
    "text",
    x = "November 1992",
    y = 19.6,
    label = "",
    angle = 90,
    size = 3
  )
