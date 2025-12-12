library(tidyverse)
library(careless)
library(formattable)
library(ggmap)
library(ggrepel)
library(ggthemes)
library(magrittr)
library(mapproj)
library(maps)
library(naniar)
library(RColorBrewer)
library(rio)
library(scales)
library(trend)
load("wci_df_reprex.Rdata")



#IMPORT#
##import wci data
index_import <- import("wci_data.csv")

#replace missing values
index_import <- index_import %>%
  mutate(across(where(is.character), ~na_if(., "--")))
index_import <- index_import %>%
  mutate(across(where(is.character), ~na_if(., "N/A")))

#name columns
colnames(index_import) <- c("respID", "nationality", "residence", "technical1", "technical2", "technical3", "technical4", "technical5", "technical1_impact", "technical1_professional", "technical1_techskill",
"technical2_impact", "technical2_professional", "technical2_techskill", "technical3_impact", "technical3_professional", "technical3_techskill", "technical4_impact", "technical4_professional", "technical4_techskill", "technical5_impact", "technical5_professional", "technical5_techskill", "attack1", "attack2", "attack3", "attack4", "attack5", "attack1_impact", "attack1_professional", "attack1_techskill", "attack2_impact", "attack2_professional", "attack2_techskill", "attack3_impact", "attack3_professional", "attack3_techskill", "attack4_impact", "attack4_professional", "attack4_techskill", "attack5_impact", "attack5_professional", "attack5_techskill", "data1", "data2", "data3", "data4", "data5", "data1_impact", "data1_professional", "data1_techskill", "data2_impact", "data2_professional", "data2_techskill", "data3_impact", "data3_professional", "data3_techskill", "data4_impact", "data4_professional", "data4_techskill", "data5_impact", "data5_professional", "data5_techskill", "scams1", "scams2", "scams3", "scams4", "scams5", "scams1_impact", "scams1_professional", "scams1_techskill", "scams2_impact", "scams2_professional", "scams2_techskill", "scams3_impact", "scams3_professional", "scams3_techskill", "scams4_impact", "scams4_professional", "scams4_techskill", "scams5_impact", "scams5_professional", "scams5_techskill", "cash1", "cash2", "cash3", "cash4", "cash5", "cash1_impact", "cash1_professional", "cash1_techskill", "cash2_impact", "cash2_professional", "cash2_techskill", "cash3_impact", "cash3_professional", "cash3_techskill", "cash4_impact", "cash4_professional", "cash4_techskill", "cash5_impact", "cash5_professional", "cash5_techskill", "expert_crimetype", "expert_crimetype_other", "expert_region", "expert_region_other", "comments")


#CLEANING#
#reshaping each variable to long

technical1_long <- index_import |> 
  select(respID:technical1, technical1_impact:technical1_techskill) |>
  pivot_longer(cols = technical1_impact:technical1_techskill, names_to = "measures", values_to = "rating")
technical1_long$position <- 1
technical1_long$crimetype <- "technical"
names(technical1_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

technical2_long <- index_import |> 
  select(respID:residence, technical2, technical2_impact:technical2_techskill) |>
  pivot_longer(cols = technical2_impact:technical2_techskill, names_to = "measures", values_to = "rating")
technical2_long$position <- 2
technical2_long$crimetype <- "technical"
names(technical2_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

technical3_long <- index_import |> 
  select(respID:residence, technical3, technical3_impact:technical3_techskill) |>
  pivot_longer(cols = technical3_impact:technical3_techskill, names_to = "measures", values_to = "rating")
technical3_long$position <- 3
technical3_long$crimetype <- "technical"
names(technical3_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

technical4_long <- index_import |> 
  select(respID:residence, technical4, technical4_impact:technical4_techskill) |>
  pivot_longer(cols = technical4_impact:technical4_techskill, names_to = "measures", values_to = "rating")
technical4_long$position <- 4
technical4_long$crimetype <- "technical"
names(technical4_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")
technical4_long$country <- str_replace(technical4_long$country, "Iran ", "Iran")


technical5_long <- index_import |> 
  select(respID:residence, technical5, technical5_impact:technical5_techskill) |>
  pivot_longer(cols = technical5_impact:technical5_techskill, names_to = "measures", values_to = "rating")
technical5_long$position <- 5
technical5_long$crimetype <- "technical"
names(technical5_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

attack1_long <- index_import |> 
  select(respID:residence, attack1, attack1_impact:attack1_techskill) |>
  pivot_longer(cols = attack1_impact:attack1_techskill, names_to = "measures", values_to = "rating")
attack1_long$position <- 1
attack1_long$crimetype <- "attack"
names(attack1_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

attack2_long <- index_import |> 
  select(respID:residence, attack2, attack2_impact:attack2_techskill) |>
  pivot_longer(cols = attack2_impact:attack2_techskill, names_to = "measures", values_to = "rating")
attack2_long$position <- 2
attack2_long$crimetype <- "attack"
names(attack2_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

attack3_long <- index_import |> 
  select(respID:residence, attack3, attack3_impact:attack3_techskill) |>
  pivot_longer(cols = attack3_impact:attack3_techskill, names_to = "measures", values_to = "rating")
attack3_long$position <- 3
attack3_long$crimetype <- "attack"
names(attack3_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

attack4_long <- index_import |> 
  select(respID:residence, attack4, attack4_impact:attack4_techskill) |>
  pivot_longer(cols = attack4_impact:attack4_techskill, names_to = "measures", values_to = "rating")
attack4_long$position <- 4
attack4_long$crimetype <- "attack"
names(attack4_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

attack5_long <- index_import |> 
  select(respID:residence, attack5, attack5_impact:attack5_techskill) |>
  pivot_longer(cols = attack5_impact:attack5_techskill, names_to = "measures", values_to = "rating")
attack5_long$position <- 5
attack5_long$crimetype <- "attack"
names(attack5_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

data1_long <- index_import |> 
  select(respID:residence, data1, data1_impact:data1_techskill) |> 
  pivot_longer(cols = data1_impact:data1_techskill, names_to = "measures", values_to = "rating")
data1_long$position <- 1
data1_long$crimetype <- "data"
names(data1_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

data2_long <- index_import |> 
  select(respID:residence, data2, data2_impact:data2_techskill) |> 
  pivot_longer(cols = data2_impact:data2_techskill, names_to = "measures", values_to = "rating")
data2_long$position <- 2
data2_long$crimetype <- "data"
names(data2_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

data3_long <- index_import |> 
  select(respID:residence, data3, data3_impact:data3_techskill) |> 
  pivot_longer(cols = data3_impact:data3_techskill, names_to = "measures", values_to = "rating")
data3_long$position <- 3
data3_long$crimetype <- "data"
names(data3_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

data4_long <- index_import |> 
  select(respID:residence, data4, data4_impact:data4_techskill) |> 
  pivot_longer(cols = data4_impact:data4_techskill, names_to = "measures", values_to = "rating")
data4_long$position <- 4
data4_long$crimetype <- "data"
names(data4_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

data5_long <- index_import |> 
  select(respID:residence, data5, data5_impact:data5_techskill) |> 
  pivot_longer(cols = data5_impact:data5_techskill, names_to = "measures", values_to = "rating")
data5_long$position <- 5
data5_long$crimetype <- "data"
names(data5_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

scams1_long <- index_import |> 
  select(respID:residence, scams1, scams1_impact:scams1_techskill) |> 
  pivot_longer(cols = scams1_impact:scams1_techskill, names_to = "measures", values_to = "rating")
scams1_long$position <- 1
scams1_long$crimetype <- "scams"
names(scams1_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

scams2_long <- index_import |> 
  select(respID:residence, scams2, scams2_impact:scams2_techskill) |> 
  pivot_longer(cols = scams2_impact:scams2_techskill, names_to = "measures", values_to = "rating")
scams2_long$position <- 2
scams2_long$crimetype <- "scams"
names(scams2_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

scams3_long <- index_import |> 
  select(respID:residence, scams3, scams3_impact:scams3_techskill) |> 
  pivot_longer(cols = scams3_impact:scams3_techskill, names_to = "measures", values_to = "rating")
scams3_long$position <- 3
scams3_long$crimetype <- "scams"
names(scams3_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

scams4_long <- index_import |> 
  select(respID:residence, scams4, scams4_impact:scams4_techskill) |> 
  pivot_longer(cols = scams4_impact:scams4_techskill, names_to = "measures", values_to = "rating")
scams4_long$position <- 4
scams4_long$crimetype <- "scams"
names(scams4_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

scams5_long <- index_import |>   
  select(respID:residence, scams5, scams5_impact:scams5_techskill) |>  
  pivot_longer(cols = scams5_impact:scams5_techskill, names_to = "measures", values_to = "rating")
scams5_long$position <- 5
scams5_long$crimetype <- "scams"
names(scams5_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

cash1_long <- index_import |> 
  select(respID:residence, cash1, cash1_impact:cash1_techskill) |> 
  pivot_longer(cols = cash1_impact:cash1_techskill, names_to = "measures", values_to = "rating")
cash1_long$position <- 1
cash1_long$crimetype <- "cash"
names(cash1_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

cash2_long <- index_import |> 
  select(respID:residence, cash2, cash2_impact:cash2_techskill) |> 
  pivot_longer(cols = cash2_impact:cash2_techskill, names_to = "measures", values_to = "rating")
cash2_long$position <- 2
cash2_long$crimetype <- "cash"
names(cash2_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

cash3_long <- index_import |> 
  select(respID:residence, cash3, cash3_impact:cash3_techskill) |> 
  pivot_longer(cols = cash3_impact:cash3_techskill, names_to = "measures", values_to = "rating")
cash3_long$position <- 3
cash3_long$crimetype <- "cash"
names(cash3_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

cash4_long <- index_import |> 
  select(respID:residence, cash4, cash4_impact:cash4_techskill) |> 
  pivot_longer(cols = cash4_impact:cash4_techskill, names_to = "measures", values_to = "rating")
cash4_long$position <- 4
cash4_long$crimetype <- "cash"
names(cash4_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")

cash5_long <- index_import |> 
  select(respID:residence, cash5, cash5_impact:cash5_techskill) |> 
  pivot_longer(cols = cash5_impact:cash5_techskill, names_to = "measures", values_to = "rating")
cash5_long$position <- 5
cash5_long$crimetype <- "cash"
names(cash5_long) <- c("respID", "nationality", "residence", "country", "measures", "rating", "position", "crimetype")



expertise_crimetype_long <- index_import |> 
  select(respID:residence, expert_crimetype:expert_crimetype_other) |> 
  pivot_longer(cols=expert_crimetype:expert_crimetype_other, names_to ="ex", values_to = "expertise_crimetype")
names(expertise_crimetype_long) <- c("respID", "nationality", "residence", "ex", "expertise_crimetype")
expertise_crimetype_long$ex <- NULL
expertise_crimetype_long <-  expertise_crimetype_long |>
  group_by(respID, nationality, residence)  |> 
  summarise(expertise_crimetype = paste0(expertise_crimetype, collapse=","))


expertise_region_long <- index_import |> 
  select(respID:residence, expert_region_other) |> 
  pivot_longer(cols=expert_region_other, names_to ="ex", values_to = "expertise_region_other" )
expertise_region_long$ex <- NULL
names(expertise_region_long) <- c("respID", "nationality", "residence", "expertise_region")
expertise_region_long <-  expertise_region_long |>
  group_by(respID, nationality, residence)  |> 
  summarise(expertise_region = paste0(expertise_region, collapse=","))

comments_long <- index_import |> 
  select(respID:residence, comments) |> 
  pivot_longer(cols=comments, names_to ="ex", values_to = "comments" )
names(comments_long) <- c("respID", "nationality", "residence", "ex", "comments")
comments_long$ex=NULL

nationality_long <- index_import |> 
  select(respID:nationality)
names(nationality_long) <- c("respID", "nationality")

residence_long <- index_import  |> 
  select(respID, residence)
names(residence_long) <- c("respID", "residence")



##join long variables together into new df, "index_long"
index_long <- bind_rows(technical1_long, technical2_long, technical3_long, technical4_long, technical5_long, attack1_long, attack2_long, attack3_long, attack4_long, attack5_long, data1_long, data2_long, data3_long, data4_long, data5_long, scams1_long, scams2_long, scams3_long, scams4_long, scams5_long, cash1_long, cash2_long, cash3_long, cash4_long, cash5_long)
index_long <- index_long |> 
  left_join(expertise_crimetype_long)
index_long <- index_long |> 
  left_join(expertise_region_long)
index_long <- index_long |> 
  left_join(comments_long)

na_strings <- c("N/A", "NA", "NA,", "Prefer not to say", "", " ")

index_long <-  index_long  |> 
  replace_with_na_all(condition = ~.x %in% na_strings)

index_long$country <- str_replace(index_long$country, "Korea, North", "North Korea")
index_long$country <- str_replace(index_long$country, "Korea, South", "South Korea")
index_long$country <- str_replace(index_long$country, "Swaziland", "Eswatini")

index_long$country <- str_replace(index_long$country, "Korea North", "North Korea")


##Frequency & region frequency variables - part of cleaning the dataset to ensure each country is counted the correct number of times
##country frequency (need to divide by 3 because each country has 3 measures/observations associated with it)
index_long <- index_long |>
  group_by(country) |>
  mutate(country_freq=n()) |>
  mutate(country_freq=country_freq / 3)

#country region frequency variable
index_long<- index_long |> 
  select(respID:country_freq) |> 
  mutate(country_region =
           ifelse(
             grepl("Argentina|Bolivia|Brazil|Chile|Colombia|Ecuador|Guyana|Paraguay|Peru|Suriname|Uruguay|Venezuela", country),"South America",
             ifelse(
               grepl("Antigua and Barbuda|Bahamas|Barbados|Belize|Canada|Costa Rica|Cuba|Dominica|Dominican Republic|El Salvador|Grenada|Guatemala|Haiti|Honduras|Jamaica|Mexico|Nicaragua|Panama|Saint Kitts and Nevis|Saint Lucia|Saint Vincent and the Grenadines|Trinidad and Tobago|United States",country),"North America",
               ifelse(
                 grepl("Albania|Andorra|Armenia|Austria|Azerbaijan|Belarus|Belgium|Bosnia and Herzegovina|Bulgaria|Croatia|Cyprus|Czech Republic|Denmark|Estonia|Finland|France|Georgia|Germany|Greece|Hungary|Iceland|Ireland|Italy|Kazakhstan|Latvia|Liechtenstein|Lithuania|Luxembourg|Malta|Moldova|Monaco|Montenegro|Netherlands|North Macedonia|Norway|Poland|Portugal|Romania|Russia|San Marino|Serbia|SLovakia|Slovenia|Spain|Sweden|Switzerland|Turkey|Ukraine|United Kingdom|Vatican City",country),"Europe",
                 ifelse(
                   grepl("Afghanistan|Australia|Bahrain|Bangladesh|Bhutan|Brunei|Cambodia|China|Cook Islands|East Timor|Egypt|Fiji|India|Indonesia|Iran|Iran |Iraq|Israel|Japan|Jordan|Kazakhstan|Kiribati|Kuwait|Kyrgyzstan|Laos|Lebanon|Libya|Malaysia|Maldives|Marshall Islands|Micronesia|Mongolia|Myanmar|Nauru|Nepal|New Zealand|Niue|North Korea|Oman|Pakistan|Palau|Palestine|Papua New Guinea|Philippines|Qatar|Samoa|Saudi Arabia|Singapore|Solomon Islands|South Korea|Sri Lanka|Syria|Tajikistan|Thailand|Tonga|Turkmenistan|Tuvalu|Uzbekistan|United Arab Emirates|Vanuatu|Vietnam|Yemen",country), "Asia Pacific",
                   ifelse(grepl("Algeria|Angola|Benin|Botswana|Burkina Faso|Burundi|Cameroon|Cape Verde|Central African Republic|Chad|Comoros|Congo, Democratic Republic|Congo, Republic|Djibouti|Egypt|Equatorial Guinea|Eritrea|Eswatini|Ethiopia|Gabon|Gambia|Ghana|Guinea|Guinea-Bissau|Ivory Coast|Kenya|Lesotho|Liberia|Libya|Madagascar|Malawi|Mali|Mauritania|Mauritius|Morocco|Mozambique|Namibia|Niger|Nigeria|Rwanda|Sao Tome and Principe|Senegal|Seychelles|Sierra Leone|Somalia|South Africa|South Sudan|Sudan|Tanzania|Togo|Tunisia|Uganda|Zambia|Zimbabwe", country),"Africa",
                          ifelse(grepl("Prefer not to say", nationality),"Prefer not to say",
                          ifelse(grepl(NA, country),"NA",
                                 "Other"))))))))


#nationality region frequency variable
index_long<- index_long |> 
  select(respID:country_region) |> 
  mutate(nationality_region =
           ifelse(
             grepl("Argentina|Bolivia|Brazil|Chile|Colombia|Ecuador|Guyana|Paraguay|Peru|Suriname|Uruguay|Venezuela", nationality),"South America",
             ifelse(
               grepl("Antigua and Barbuda|Bahamas|Barbados|Belize|Canada|Costa Rica|Cuba|Dominica|Dominican Republic|El Salvador|Grenada|Guatemala|Haiti|Honduras|Jamaica|Mexico|Nicaragua|Panama|Saint Kitts and Nevis|Saint Lucia|Saint Vincent and the Grenadines|Trinidad and Tobago|United States",nationality),"North America",
               ifelse(
                 grepl("Albania|Andorra|Armenia|Austria|Azerbaijan|Belarus|Belgium|Bosnia and Herzegovina|Bulgaria|Croatia|Cyprus|Czech Republic|Denmark|Estonia|Finland|France|Georgia|Germany|Greece|Hungary|Iceland|Ireland|Italy|Kazakhstan|Latvia|Liechtenstein|Lithuania|Luxembourg|Malta|Moldova|Monaco|Montenegro|Netherlands|North Macedonia|Norway|Poland|Portugal|Romania|Russia|San Marino|Serbia|SLovakia|Slovenia|Spain|Sweden|Switzerland|Turkey|Ukraine|United Kingdom|Vatican City",nationality),"Europe",
                 ifelse(
                   grepl("Afghanistan|Australia|Bahrain|Bangladesh|Bhutan|Brunei|Cambodia|China|Cook Islands|East Timor|Egypt|Fiji|India|Indonesia|Iran|Iran |Iraq|Israel|Japan|Jordan|Kazakhstan|Kiribati|Kuwait|Kyrgyzstan|Laos|Lebanon|Libya|Malaysia|Maldives|Marshall Islands|Micronesia|Mongolia|Myanmar|Nauru|Nepal|New Zealand|Niue|North Korea|Oman|Pakistan|Palau|Palestine|Papua New Guinea|Philippines|Qatar|Samoa|Saudi Arabia|Singapore|Solomon Islands|South Korea|Sri Lanka|Syria|Tajikistan|Thailand|Tonga|Turkmenistan|Tuvalu|Uzbekistan|United Arab Emirates|Vanuatu|Vietnam|Yemen",nationality), "Asia Pacific",
                   ifelse(grepl("Algeria|Angola|Benin|Botswana|Burkina Faso|Burundi|Cameroon|Cape Verde|Central African Republic|Chad|Comoros|Congo, Democratic Republic|Congo, Republic|Djibouti|Egypt|Equatorial Guinea|Eritrea|Eswatini|Ethiopia|Gabon|Gambia|Ghana|Guinea|Guinea-Bissau|Ivory Coast|Kenya|Lesotho|Liberia|Libya|Madagascar|Malawi|Mali|Mauritania|Mauritius|Morocco|Mozambique|Namibia|Niger|Nigeria|Rwanda|Sao Tome and Principe|Senegal|Seychelles|Sierra Leone|Somalia|South Africa|South Sudan|Sudan|Tanzania|Togo|Tunisia|Uganda|Zambia|Zimbabwe", nationality),"Africa",
                          ifelse(grepl("Prefer not to say", nationality),"Prefer not to say",
                                 ifelse(grepl(NA, nationality),"NA",
                                 "Other"))))))))


#residence region frequency variable
index_long<- index_long |> 
  select(respID:nationality_region) |> 
  mutate(residence_region =
           ifelse(
             grepl("Argentina|Bolivia|Brazil|Chile|Colombia|Ecuador|Guyana|Paraguay|Peru|Suriname|Uruguay|Venezuela", residence),"South America",
             ifelse(
               grepl("Antigua and Barbuda|Bahamas|Barbados|Belize|Canada|Costa Rica|Cuba|Dominica|Dominican Republic|El Salvador|Grenada|Guatemala|Haiti|Honduras|Jamaica|Mexico|Nicaragua|Panama|Saint Kitts and Nevis|Saint Lucia|Saint Vincent and the Grenadines|Trinidad and Tobago|United States",residence),"North America",
               ifelse(
                 grepl("Albania|Andorra|Armenia|Austria|Azerbaijan|Belarus|Belgium|Bosnia and Herzegovina|Bulgaria|Croatia|Cyprus|Czech Republic|Denmark|Estonia|Finland|France|Georgia|Germany|Greece|Hungary|Iceland|Ireland|Italy|Kazakhstan|Latvia|Liechtenstein|Lithuania|Luxembourg|Malta|Moldova|Monaco|Montenegro|Netherlands|North Macedonia|Norway|Poland|Portugal|Romania|Russia|San Marino|Serbia|SLovakia|Slovenia|Spain|Sweden|Switzerland|Turkey|Ukraine|United Kingdom|Vatican City",residence),"Europe",
                 ifelse(
                   grepl("Afghanistan|Australia|Bahrain|Bangladesh|Bhutan|Brunei|Cambodia|China|Cook Islands|East Timor|Egypt|Fiji|India|Indonesia|Iran|Iran |Iraq|Israel|Japan|Jordan|Kazakhstan|Kiribati|Kuwait|Kyrgyzstan|Laos|Lebanon|Libya|Malaysia|Maldives|Marshall Islands|Micronesia|Mongolia|Myanmar|Nauru|Nepal|New Zealand|Niue|North Korea|Oman|Pakistan|Palau|Palestine|Papua New Guinea|Philippines|Qatar|Samoa|Saudi Arabia|Singapore|Solomon Islands|South Korea|Sri Lanka|Syria|Tajikistan|Thailand|Tonga|Turkmenistan|Tuvalu|Uzbekistan|United Arab Emirates|Vanuatu|Vietnam|Yemen",residence), "Asia Pacific",
                   ifelse(grepl("Algeria|Angola|Benin|Botswana|Burkina Faso|Burundi|Cameroon|Cape Verde|Central African Republic|Chad|Comoros|Congo, Democratic Republic|Congo, Republic|Djibouti|Egypt|Equatorial Guinea|Eritrea|Eswatini|Ethiopia|Gabon|Gambia|Ghana|Guinea|Guinea-Bissau|Ivory Coast|Kenya|Lesotho|Liberia|Libya|Madagascar|Malawi|Mali|Mauritania|Mauritius|Morocco|Mozambique|Namibia|Niger|Nigeria|Rwanda|Sao Tome and Principe|Senegal|Seychelles|Sierra Leone|Somalia|South Africa|South Sudan|Sudan|Tanzania|Togo|Tunisia|Uganda|Zambia|Zimbabwe", residence),"Africa",
                          ifelse(grepl("Prefer not to say", nationality),"Prefer not to say",
                          ifelse(grepl(NA, residence),"NA",
                                 "Other"))))))))

#more frequency variables (need to divide by multiples of 3 b/c each country has 3 observations associated with it)
index_long <- index_long |>
  group_by(country_region) |> 
  mutate(country_region_freq=n()) |>
  mutate(country_region_freq=country_region_freq / 3)

index_long <- index_long |>
  group_by(nationality) |>
  mutate(nationality_freq=n()) |>
  mutate(nationality_freq=nationality_freq / 75)

index_long <- index_long |>
  group_by(nationality_region) |>
  mutate(nationality_region_freq=n()) |>
  mutate(nationality_region_freq=nationality_region_freq / 75)

index_long <- index_long |>
  group_by(residence) |>
  mutate(residence_freq=n()) |>
  mutate(residence_freq=residence_freq / 75)

index_long <- index_long |>
  group_by(residence_region) |>
  mutate(residence_region_freq=n()) |> 
  mutate(residence_region_freq=residence_region_freq / 75)



#local & not local variable for nationality and residence
#this will be helpful for later analysis/regressions
index_long <-  index_long |> 
  select(everything())  |> 
  mutate(
    nat_local = case_when(
      country == nationality ~ "local",
      country != nationality ~ "not_local",
      .default = NA
    )
  )

index_long <-  index_long |> 
  select(everything())  |> 
  mutate(
    res_local = case_when(
      country == residence ~ "local",
      country != residence ~ "not_local",
      .default = NA
    )
  )




#re-order variables
index_long <- index_long |> 
  relocate(nationality_freq, .after=nationality)
index_long <- index_long |> 
  relocate(residence_freq, .after=residence)
index_long <- index_long |> 
  relocate(country_freq, .after=country)
index_long <- index_long |> 
  relocate(country_region, .after=country_freq)
index_long <- index_long |> 
  relocate(country_region_freq, .after=country_region)
index_long <- index_long |> 
  relocate(nationality_freq, .after=nationality)
index_long <- index_long |> 
  relocate(nationality_region, .after=nationality_freq)
index_long <- index_long |> 
  relocate(nationality_region_freq, .after=nationality_region)
index_long <- index_long |> 
  relocate(residence_region, .after=residence_freq)
index_long <- index_long |> 
  relocate(residence_region_freq, .after=residence_region)
index_long <- index_long |> 
  relocate(country:country_region_freq, .after=residence_region_freq)
index_long <- index_long |> 
  relocate(res_local, .after=residence_freq)
index_long <- index_long |> 
  relocate(nat_local, .after=nationality_freq)


index_long <- index_long |> 
  arrange(respID)


#add "survey position" to do regressions that look at how rating behaviour changed as the survey progressed
#we won't be able to do this with the next survey, which randomises the order of the cybercrime categories
index_long <-  index_long |> 
  add_column(c(rep(1:5, each=15, length.out=6900))) |> 
  rename(survey_position="c(rep(1:5, each = 15, length.out = 6900))")

summary(lm(rating ~ survey_position, data = index_long))

plot(lm(rating ~ survey_position, data = index_long))

ggplot(index_long, aes(survey_position, rating))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title="Ratings as survey progressed", x="Survey progression", y="Rating")




#WCI INDICES#

#first count the number of times a country was nominated within a cybercrime category. This will be helpful when composing the full index later
tech_count <- index_long |> 
  filter(str_detect(crimetype, "technical")) |> 
  distinct() |> 
  group_by(country) |> 
  mutate(noms_tech=n()) |> 
  mutate(noms_tech=noms_tech/3) |> 
  select(country, noms_tech) |>
  distinct() |> 
  arrange(desc(noms_tech))

attack_count <- index_long |> 
  filter(str_detect(crimetype, "attack")) |> 
  distinct() |> 
  group_by(country) |> 
  mutate(noms_attack=n()) |> 
  mutate(noms_attack=noms_attack/3) |> 
  select(country, noms_attack) |>
  distinct() |> 
  arrange(desc(noms_attack))

data_count <- index_long |> 
  filter(str_detect(crimetype, "data")) |> 
  distinct() |> 
  group_by(country) |> 
  mutate(noms_data=n()) |> 
  mutate(noms_data=noms_data/3) |> 
  select(country, noms_data) |>
  distinct() |> 
  arrange(desc(noms_data))

scams_count <- index_long |> 
  filter(str_detect(crimetype, "scams")) |> 
  distinct() |> 
  group_by(country) |> 
  mutate(noms_scams=n()) |> 
  mutate(noms_scams=noms_scams/3) |> 
  select(country, noms_scams) |>
  distinct() |> 
  arrange(desc(noms_scams))

cash_count <- index_long |> 
  filter(str_detect(crimetype, "cash")) |> 
  distinct() |> 
  group_by(country) |> 
  mutate(noms_cash=n()) |> 
  mutate(noms_cash=noms_cash/3) |> 
  select(country, noms_cash) |>
  distinct() |> 
  arrange(desc(noms_cash))



#Index by cybercrime type#
##First I filter index_long for each measure (impact, professionalism, technical skill) and summarise() to get the mean. I make a separate df for each measure.
##Second I combine these dfs into a single index and clean up. 
##Finally I calculate the overall and WCI scores from the mean scores for each measure.


##Tech Index##
tech_index_impact <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "technical"))) |>
  filter(if_all(everything(), ~str_detect(measures, "impact"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(impact=mean(rating)) |> 
  arrange(desc(country_freq))


tech_index_prof <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "technical"))) |>
  filter(if_all(everything(), ~str_detect(measures, "professional"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(professionalism=mean(rating)) |> 
  arrange(desc(country_freq))



tech_index_ts <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "technical"))) |>
  filter(if_all(everything(), ~str_detect(measures, "techskill"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(technical_skill=mean(rating)) |> 
  arrange(desc(country_freq))


tech_index <-  left_join(tech_index_impact, tech_index_prof)

tech_index <- left_join(tech_index, tech_index_ts)

tech_index <- left_join(tech_index, tech_count)

tech_index <- rename(tech_index, noms=noms_tech)

tech_index <-  drop_na(tech_index)

tech_index$country_freq=NULL

##calculating "overall" (mean of impact+professionalism+technical skill)
##and WCI score (overall*(noms/92)*10)
tech_index <- tech_index |> 
  select(everything()) |> 
  group_by(country) |> 
  mutate(overall=mean(c(impact, professionalism, technical_skill)))

tech_index <- tech_index |> 
  mutate(wci = (overall*(noms/92))*10) |> 
  arrange(desc(wci)) |> 
  ungroup() |> 
  select(everything())  |>
  mutate(rank = min_rank(desc(wci))) |> 
  relocate(noms, .before = country) |> 
  relocate(rank, .before = noms)

colnames(tech_index) <- c("Rank", "Noms", "Country", "Impact", "Professionalism", "Technical_skill", "Overall", "WCI_Score")



##Attack Index##
attack_index_impact <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "attack"))) |>
  filter(if_all(everything(), ~str_detect(measures, "impact"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(impact=mean(rating)) |> 
  arrange(desc(country_freq))


attack_index_prof <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "attack"))) |>
  filter(if_all(everything(), ~str_detect(measures, "professional"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(professionalism=mean(rating)) |> 
  arrange(desc(country_freq))



attack_index_ts <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "attack"))) |>
  filter(if_all(everything(), ~str_detect(measures, "techskill"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(technical_skill=mean(rating)) |> 
  arrange(desc(country_freq))


attack_index <-  left_join(attack_index_impact, attack_index_prof)

attack_index <- left_join(attack_index, attack_index_ts)

attack_index <- left_join(attack_index, attack_count)

attack_index <- rename(attack_index, noms=noms_attack)

attack_index <-  drop_na(attack_index)

attack_index$country_freq=NULL

#calculating "overall" & WCI score
attack_index <- attack_index |> 
  select(everything()) |> 
  group_by(country) |> 
  mutate(overall=mean(c(impact, professionalism, technical_skill)))

attack_index <- attack_index |> 
  mutate(wci = (overall*(noms/92))*10) |> 
  arrange(desc(wci)) |> 
  ungroup() |> 
  select(everything())  |>
  mutate(rank = min_rank(desc(wci))) |>
  relocate(noms, .before = country) |> 
  relocate(rank, .before = noms)

colnames(attack_index) <- c("Rank", "Noms", "Country", "Impact", "Professionalism", "Technical_skill", "Overall", "WCI_Score")



##Data Index##
data_index_impact <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "data"))) |>
  filter(if_all(everything(), ~str_detect(measures, "impact"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(impact=mean(rating)) |> 
  arrange(desc(country_freq))

data_index_prof <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "data"))) |>
  filter(if_all(everything(), ~str_detect(measures, "professional"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(professionalism=mean(rating)) |> 
  arrange(desc(country_freq))


data_index_ts <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "data"))) |>
  filter(if_all(everything(), ~str_detect(measures, "techskill"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(technical_skill=mean(rating)) |> 
  arrange(desc(country_freq))


data_index <-  left_join(data_index_impact, data_index_prof)

data_index <- left_join(data_index, data_index_ts)

data_index <- left_join(data_index, data_count)

data_index <- rename(data_index, noms=noms_data)

data_index <-  drop_na(data_index)
data_index$country_freq=NULL
data_index <-  drop_na(data_index)

#calculating "overall" & WCI score
data_index <- data_index |> 
  select(everything()) |> 
  group_by(country) |> 
  mutate(overall=mean(c(impact, professionalism, technical_skill)))

data_index <- data_index |> 
  mutate(wci = (overall*(noms/92))*10) |> 
  arrange(desc(wci)) |> 
  ungroup() |> 
  select(everything())  |>
  mutate(rank = min_rank(desc(wci))) |> 
  relocate(noms, .before = country) |> 
  relocate(rank, .before = noms)

colnames(data_index) <- c("Rank", "Noms", "Country", "Impact", "Professionalism", "Technical_skill", "Overall", "WCI_Score")



##Scams Index##
scams_index_impact <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "scams"))) |>
  filter(if_all(everything(), ~str_detect(measures, "impact"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(impact=mean(rating)) |> 
  arrange(desc(country_freq))

scams_index_prof <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "scams"))) |>
  filter(if_all(everything(), ~str_detect(measures, "professional"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(professionalism=mean(rating)) |> 
  arrange(desc(country_freq))


scams_index_ts <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "scams"))) |>
  filter(if_all(everything(), ~str_detect(measures, "techskill"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(technical_skill=mean(rating)) |> 
  arrange(desc(country_freq))


scams_index <-  left_join(scams_index_impact, scams_index_prof)

scams_index <- left_join(scams_index, scams_index_ts)

scams_index <- left_join(scams_index, scams_count)

scams_index <- rename(scams_index, noms=noms_scams)

scams_index <-  drop_na(scams_index)
scams_index$country_freq=NULL
scams_index <-  drop_na(scams_index)

#calculating "overall" & WCI score
scams_index <- scams_index |> 
  select(everything()) |> 
  group_by(country) |> 
  mutate(overall=mean(c(impact, professionalism, technical_skill)))

scams_index <- scams_index |> 
  mutate(wci = (overall*(noms/92))*10) |> 
  arrange(desc(wci)) |> 
  ungroup() |> 
  select(everything())  |>
  mutate(rank = min_rank(desc(wci))) |> 
  relocate(noms, .before = country) |> 
  relocate(rank, .before = noms)

colnames(scams_index) <- c("Rank", "Noms", "Country", "Impact", "Professionalism", "Technical_skill", "Overall", "WCI_Score")




##Cash Index##
cash_index_impact <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "cash"))) |>
  filter(if_all(everything(), ~str_detect(measures, "impact"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(impact=mean(rating)) |> 
  arrange(desc(country_freq))

cash_index_prof <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "cash"))) |>
  filter(if_all(everything(), ~str_detect(measures, "professional"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(professionalism=mean(rating)) |> 
  arrange(desc(country_freq))

cash_index_ts <- index_long |> 
  filter(if_all(everything(), ~str_detect(crimetype, "cash"))) |>
  filter(if_all(everything(), ~str_detect(measures, "techskill"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(technical_skill=mean(rating)) |> 
  arrange(desc(country_freq))


cash_index <-  left_join(cash_index_impact, cash_index_prof)

cash_index <- left_join(cash_index, cash_index_ts)

cash_index <- left_join(cash_index, cash_count)

cash_index[sapply(cash_index, is.nan)]<-NA

cash_index <- rename(cash_index, noms=noms_cash)

cash_index$country_freq=NULL

cash_index <-  drop_na(cash_index)


#calculating "overall" & WCI score
cash_index <- cash_index |> 
  select(everything()) |> 
  group_by(country) |> 
  mutate(overall=mean(c(impact, professionalism, technical_skill)))

cash_index <- cash_index |> 
  mutate(wci = (overall*(noms/92))*10) |> 
  arrange(desc(wci)) |> 
  ungroup() |> 
  select(everything())  |>
  mutate(rank = min_rank(desc(wci))) |> 
  relocate(noms, .before = country) |> 
  relocate(rank, .before = noms)

colnames(cash_index) <- c("Rank", "Noms", "Country", "Impact", "Professionalism", "Technical_skill", "Overall", "WCI_Score")



##WCI Overall Index##
overall_index_impact <- index_long |> 
  filter(if_all(everything(), ~str_detect(measures, "impact"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(impact=mean(rating)) |> 
  arrange(desc(country_freq)) |> 
  print()

overall_index_prof <- index_long |> 
  filter(if_all(everything(), ~str_detect(measures, "professional"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(professionalism=mean(rating)) |> 
  arrange(desc(country_freq))

overall_index_ts <- index_long |> 
  filter(if_all(everything(), ~str_detect(measures, "techskill"))) |>
  select(everything()) |> 
  group_by(country, country_freq) |> 
  summarise(technical_skill=mean(rating)) |> 
  arrange(desc(country_freq))


overall_index <-  left_join(overall_index_impact, overall_index_prof)

overall_index <- left_join(overall_index, overall_index_ts)

overall_index <-  drop_na(overall_index)

overall_index <-  rename(overall_index, noms = country_freq)

#calculating "overall" & WCI score
overall_index <- overall_index |> 
  select(everything()) |> 
  group_by(country) |> 
  mutate(overall=mean(c(impact, professionalism, technical_skill)))
view(overall_index)
overall_index <- overall_index |> 
  mutate(wci = (overall*(noms/460))*10) |> 
  arrange(desc(wci)) |> 
  ungroup() |> 
  select(everything())  |>
  mutate(rank = min_rank(desc(wci))) |> 
  relocate(noms, .before = country) |> 
  relocate(rank, .before = noms)

colnames(overall_index) <- c("Rank", "Noms", "Country", "Impact", "Professionalism", "Technical_skill", "Overall", "WCI_Score")


#complete WCI index includes all 197 UN-recognised countries
#code below creates dfs for each index with all countries

country.list <- c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "The Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", "Congo, Democratic Republic of the", "Congo, Republic of the", "Cook Islands", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "North Korea", "North Macedonia", "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland", "Syria", "Tajikistan", "Tanzania", "Thailand", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe")

country.list.index <- as.data.frame(country.list)

country.list.index <- cbind(Rank = NA, Noms = NA, country.list, Impact = NA, Professionalism = NA, Technical_skill = NA, Overall = NA, WCI_Score = NA)
colnames(country.list.index) <- c("Rank", "Noms", "Country", "Impact", "Professionalism", "Technical_skill", "Overall", "WCI_Score")
country.list.index <- as.data.frame(country.list.index)
country.list.index$Rank <- as.numeric(country.list.index$Rank)
country.list.index$Noms <- as.numeric(country.list.index$Noms)
country.list.index$Impact <- as.numeric(country.list.index$Impact)
country.list.index$Professionalism <- as.numeric(country.list.index$Professionalism)
country.list.index$Technical_skill <- as.numeric(country.list.index$Technical_skill)
country.list.index$Overall <- as.numeric(country.list.index$Overall)
country.list.index$WCI_Score <- as.numeric(country.list.index$WCI_Score)


#Tech index with all countries
WCI_tech <- bind_rows(tech_index, country.list.index)
WCI_tech <-  WCI_tech |> 
  distinct(Country, .keep_all = T)
view(WCI_tech)
WCI_tech[45:197,1] <-rep(45)


#Attack index with all countries
WCI_attack <- bind_rows(attack_index, country.list.index)
WCI_attack <-  WCI_attack |> 
  distinct(Country, .keep_all = T)
view(WCI_attack)
WCI_attack[46:197,1] <-rep(46)


#Data index with all countries
WCI_data <- bind_rows(data_index, country.list.index)
WCI_data <-  WCI_data |> 
  distinct(Country, .keep_all = T)
view(WCI_data)
WCI_data[55:197,1] <-rep(55)



#Scams index with all countries
WCI_scams <- bind_rows(scams_index, country.list.index)
WCI_scams <-  WCI_scams |> 
  distinct(Country, .keep_all = T)
view(WCI_scams)
WCI_scams[53:197,1] <-rep(53)



#Cash index with all countries
WCI_cash <- bind_rows(cash_index, country.list.index)
WCI_cash <-  WCI_cash |> 
  distinct(Country, .keep_all = T)
view(WCI_cash)
WCI_cash[67:197,1] <-rep(67)


#Overall index with all countries
WCI_overall <- bind_rows(overall_index, country.list.index)
WCI_overall <-  WCI_overall |> 
  distinct(Country, .keep_all = T)
view(WCI_overall)
WCI_overall[98:197,1] <-rep(98)



#exporting index_wide to test the WCI score calculations
#index_wide <- index_long |> 
  #group_by(measures) |> 
  #mutate(row = row_number()) |>  
  #pivot_wider(names_from=measures, values_from = rating) |> 
  #select(-row)

#index_wide_temp <- index_wide |> 
  #select(country, country_freq, technical1_impact:cash5_techskill) |> 
  #arrange(country)
#index_wide_temp <-  index_wide_temp[1:18,] #selecting Afghanistan, Algeria, Angola, Argenti, Armenia, Australia
#write.csv(index_wide_temp,"wci_overall_test.csv")



#second test
#index_wide_temp <- index_wide |> 
  #select(country, country_freq, technical1_impact:cash5_techskill) |> 
  #arrange(country)
#index_wide_temp <-  index_wide_temp[19:116,] 

#write.csv(index_wide_temp,"wci_overall_test2.csv")


#rounding decimals for the WCI indices
WCI_attack$Impact <- 
  round(WCI_attack$Impact, digits=2)
WCI_attack$Professionalism <- 
  round(WCI_attack$Professionalism, digits=2)
WCI_attack$Technical_skill <- 
  round(WCI_attack$Technical_skill, digits=2)
WCI_attack$Overall <- 
  round(WCI_attack$Overall, digits=2)
WCI_attack$WCI_Score <- 
  round(WCI_attack$WCI_Score, digits=2)

WCI_tech$Impact <- 
  round(WCI_tech$Impact, digits=2)
WCI_tech$Professionalism <- 
  round(WCI_tech$Professionalism, digits=2)
WCI_tech$Technical_skill <- 
  round(WCI_tech$Technical_skill, digits=2)
WCI_tech$Overall <- 
  round(WCI_tech$Overall, digits=2)
WCI_tech$WCI_Score <- 
  round(WCI_tech$WCI_Score, digits=2)

WCI_data$Impact <- 
  round(WCI_data$Impact, digits=2)
WCI_data$Professionalism <- 
  round(WCI_data$Professionalism, digits=2)
WCI_data$Technical_skill <- 
  round(WCI_data$Technical_skill, digits=2)
WCI_data$Overall <- 
  round(WCI_data$Overall, digits=2)
WCI_data$WCI_Score <- 
  round(WCI_data$WCI_Score, digits=2)

WCI_scams$Impact <- 
  round(WCI_scams$Impact, digits=2)
WCI_scams$Professionalism <- 
  round(WCI_scams$Professionalism, digits=2)
WCI_scams$Technical_skill <- 
  round(WCI_scams$Technical_skill, digits=2)
WCI_scams$Overall <- 
  round(WCI_scams$Overall, digits=2)
WCI_scams$WCI_Score <- 
  round(WCI_scams$WCI_Score, digits=2)

WCI_cash$Impact <- 
  round(WCI_cash$Impact, digits=2)
WCI_cash$Professionalism <- 
  round(WCI_cash$Professionalism, digits=2)
WCI_cash$Technical_skill <- 
  round(WCI_cash$Technical_skill, digits=2)
WCI_cash$Overall <- 
  round(WCI_cash$Overall, digits=2)
WCI_cash$WCI_Score <- 
  round(WCI_cash$WCI_Score, digits=2)

view(WCI_attack)


WCI_overall$Impact <- 
  round(WCI_overall$Impact, digits=2)
WCI_overall$Professionalism <- 
  round(WCI_overall$Professionalism, digits=2)
WCI_overall$Technical_skill <- 
  round(WCI_overall$Technical_skill, digits=2)
WCI_overall$Overall <- 
  round(WCI_overall$Overall, digits=2)
WCI_overall$WCI_Score <- 
  round(WCI_overall$WCI_Score, digits=2)

view(WCI_overall)





#DATA VISUALISATION#

#WCIoverall barplot
#first add region to overall_index
overall_index<- overall_index |> 
  mutate(region =
           ifelse(
             grepl("Argentina|Bolivia|Brazil|Chile|Colombia|Ecuador|Guyana|Paraguay|Peru|Suriname|Uruguay|Venezuela", Country),"South America",
             ifelse(
               grepl("Antigua and Barbuda|Bahamas|Barbados|Belize|Canada|Costa Rica|Cuba|Dominica|Dominican Republic|El Salvador|Grenada|Guatemala|Haiti|Honduras|Jamaica|Mexico|Nicaragua|Panama|Saint Kitts and Nevis|Saint Lucia|Saint Vincent and the Grenadines|Trinidad and Tobago|United States",Country),"North America",
               ifelse(
                 grepl("Albania|Andorra|Armenia|Austria|Azerbaijan|Belarus|Belgium|Bosnia and Herzegovina|Bulgaria|Croatia|Cyprus|Czech Republic|Denmark|Estonia|Finland|France|Georgia|Germany|Greece|Hungary|Iceland|Ireland|Italy|Kazakhstan|Latvia|Liechtenstein|Lithuania|Luxembourg|Malta|Moldova|Monaco|Montenegro|Netherlands|North Macedonia|Norway|Poland|Portugal|Romania|Russia|San Marino|Serbia|SLovakia|Slovenia|Spain|Sweden|Switzerland|Turkey|Ukraine|United Kingdom|Vatican City",Country),"Europe",
                 ifelse(
                   grepl("Afghanistan|Australia|Bahrain|Bangladesh|Bhutan|Brunei|Cambodia|China|Cook Islands|East Timor|Egypt|Fiji|India|Indonesia|Iran|Iran |Iraq|Israel|Japan|Jordan|Kazakhstan|Kiribati|Kuwait|Kyrgyzstan|Laos|Lebanon|Libya|Malaysia|Maldives|Marshall Islands|Micronesia|Mongolia|Myanmar|Nauru|Nepal|New Zealand|Niue|North Korea|Oman|Pakistan|Palau|Palestine|Papua New Guinea|Philippines|Qatar|Samoa|Saudi Arabia|Singapore|Solomon Islands|South Korea|Sri Lanka|Syria|Tajikistan|Thailand|Tonga|Turkmenistan|Tuvalu|Uzbekistan|United Arab Emirates|Vanuatu|Vietnam|Yemen",Country), "Asia Pacific",
                   ifelse(grepl("Algeria|Angola|Benin|Botswana|Burkina Faso|Burundi|Cameroon|Cape Verde|Central African Republic|Chad|Comoros|Congo, Democratic Republic|Congo, Republic|Djibouti|Egypt|Equatorial Guinea|Eritrea|Eswatini|Ethiopia|Gabon|Gambia|Ghana|Guinea|Guinea-Bissau|Ivory Coast|Kenya|Lesotho|Liberia|Libya|Madagascar|Malawi|Mali|Mauritania|Mauritius|Morocco|Mozambique|Namibia|Niger|Nigeria|Rwanda|Sao Tome and Principe|Senegal|Seychelles|Sierra Leone|Somalia|South Africa|South Sudan|Sudan|Tanzania|Togo|Tunisia|Uganda|Zambia|Zimbabwe", Country),"Africa",
                          ifelse(grepl(NA, Country),"NA",
                                 "Other")))))))


overall_barplot <-
  ggplot(
    overall_index[1:50,],
    aes(
      x = reorder(Country, WCI_Score),
      y = WCI_Score,
      fill= region,
      position = "dodge"
    )
  )


overall_barplot+
  geom_col(width=0.9)+
  coord_flip()+
  scale_y_continuous(limits = c(0,60), expand = expansion(mult = c(0, 0)))+
  scale_x_discrete(guide = guide_axis(n.dodge=1.75))+
  labs(x="Country", y=expression(WCI[overall]*" Score"), fill="Region")+
  geom_text(aes(y=WCI_Score, 
                label=sprintf("%0.2f", round(WCI_Score, digits=2)), group=WCI_Score), 
            position = position_dodge(width=1),
            hjust = -0.05, size=2.5,
            inherit.aes = TRUE)+
  scale_fill_manual(values=c("#984EA3", "#4DAF4A",  "#377EB8", "#FF7F00", "#E41A1C"))+
  theme(axis.text.x=element_text(size=8), axis.text.y=element_text(size=8), axis.title.y=element_text(size=13), axis.title.x=element_text(size=13))


ggsave("overall_barplot_PLOS.png", scale=1, dpi = 300)

#title code:  (title=expression("Top 50 countries by "*WCI[overall]*" Score"), x=...) 



###ANALYSIS###

#Local/not local regressions
#regressions below are testing to see if "locals"
#rated their country of residence/nationality
#differently to non-locals
#(I created the local/not-local variable earlier in the script, line ~343)


#nationality local/not local
local_nat_lm <- index_long
local_nat_lm <- local_nat_lm |> 
  select(respID, country, rating, nat_local) |> 
  arrange(respID)

local_nat_lm$nat_local <-as.factor(local_nat_lm$nat_local)

model1 <- lm(rating~nat_local, data=local_nat_lm)

summary(model1)


#residence local/not local
local_res_lm <- index_long
local_res_lm <- local_res_lm |> 
  select(respID, country, rating, res_local) |> 
  arrange(respID)

local_res_lm$res_local <-as.factor(local_res_lm$res_local)

model2 <- lm(rating~res_local, data=local_res_lm)

summary(model2)





#Crime type experts
#regressions below are testing to see if experts in [crime type]
#rate countries in [crime type] differently to non-experts


#Technical crime experts (experts n=45)
tech_lm <- index_long
tech_lm <- tech_lm |> 
  select(respID, country, rating, expertise_crimetype, crimetype)  |> 
  group_by(expertise_crimetype) |> 
  mutate(
    expertise = case_when(
      (grepl("Technical products / services,", expertise_crimetype)) ~ "tech_expert",
      (!grepl("Technical products / services,", expertise_crimetype)) ~ "not_tech_expert",
      .default = NA
    )
  )

tech_lm <- tech_lm |> 
  select(everything()) |> 
  filter(str_detect(crimetype, "technical"))

tech_lm$expertise <-as.factor(tech_lm$expertise)

model3 <- lm(rating~expertise, data=tech_lm)

summary(model3)



#Attacks & extortions experts (experts n=55)
attack_lm <- index_long
attack_lm <- attack_lm |> 
  select(respID, country, rating, expertise_crimetype, crimetype)  |> 
  group_by(expertise_crimetype) |> 
  mutate(
    expertise = case_when(
      (grepl("Attacks and extortions,", expertise_crimetype)) ~ "attack_expert",
      (!grepl("Attacks and extortions,", expertise_crimetype)) ~ "not_attack_expert",
      .default = NA
    )
  )

attack_lm <- attack_lm |> 
  select(everything()) |> 
  filter(str_detect(crimetype, "attack"))

attack_lm$expertise <-as.factor(attack_lm$expertise)

model4 <- lm(rating~expertise, data=attack_lm)

summary(model4)




#Cashing out experts (experts n=45)
cash_lm <- index_long
cash_lm <- cash_lm |> 
  select(respID, country, rating, expertise_crimetype, crimetype)  |> 
  group_by(expertise_crimetype) |> 
  mutate(
    expertise = case_when(
      (grepl("Cashing out/money laundering,", expertise_crimetype)) ~ "cash_expert",
      (!grepl("Cashing out/money laundering,", expertise_crimetype)) ~ "not_cash_expert",
      .default = NA
    )
  )

cash_lm <- cash_lm |> 
  select(everything()) |> 
  filter(str_detect(crimetype, "cash"))

cash_lm$expertise <-as.factor(cash_lm$expertise)

model5 <- lm(rating~expertise, data=cash_lm)

summary(model5)




#Data/identity theft experts (experts n=53)
data_lm <- index_long
data_lm <- data_lm |> 
  select(respID, country, rating, expertise_crimetype, crimetype)  |> 
  group_by(expertise_crimetype) |> 
  mutate(
    expertise = case_when(
      (grepl("Data/identity theft,", expertise_crimetype)) ~ "data_expert",
      (!grepl("Data/identity theft,", expertise_crimetype)) ~ "not_data_expert",
      .default = NA
    )
  )

data_lm <- data_lm |> 
  select(everything()) |> 
  filter(str_detect(crimetype, "data"))

data_lm$expertise <-as.factor(data_lm$expertise)

model6 <- lm(rating~expertise, data=data_lm)

summary(model6)



#Scams experts (experts n=45)
scams_lm <- index_long
scams_lm <- scams_lm |> 
  select(respID, country, rating, expertise_crimetype, crimetype)  |> 
  group_by(expertise_crimetype) |> 
  mutate(
    expertise = case_when(
      (grepl("Scams,", expertise_crimetype)) ~ "scams_expert",
      (!grepl("Scams,", expertise_crimetype)) ~ "not_scams_expert",
      .default = NA
    )
  )
scams_lm <- scams_lm |> 
  select(everything()) |> 
  filter(str_detect(crimetype, "scams"))

scams_lm$expertise <-as.factor(scams_lm$expertise)

model7 <- lm(rating~expertise, data=scams_lm)

summary(model7)





#Region experts
#regressions below are testing to see if experts in [region]
#rate countries from that region differently to non-[region]-experts


#Africa experts (experts n=12)
africa_lm <- index_long
africa_lm <- africa_lm |> 
  select(respID, country, rating, expertise_region)  |> 
  group_by(expertise_region) |> 
  mutate(
    expertise = case_when(
      (grepl("Africa|EMEA", expertise_region)) ~ "africa_expert",
      (!grepl("Africa|EMEA", expertise_region)) ~ "not_africa_expert",
      .default = NA
    )
  )

africa_lm <- africa_lm |> 
  select(everything()) |> 
  filter(str_detect(country, "Algeria|Angola|Benin|Botswana|Burkina Faso|Burundi|Cameroon|Cape Verde|Central African Republic|Chad|Comoros|Congo, Democratic Republic|Congo, Republic|Djibouti|Egypt|Equatorial Guinea|Eritrea|Ethiopia|Gabon|Gambia|Ghana|Guinea|Guinea-Bissau|Ivory Coast|Kenya|Lesotho|Liberia|Libya|Madagascar|Malawi|Mali|Mauritania|Mauritius|Morocco|Mozambique|Namibia|Niger|Nigeria|Rwanda|Sao Tome and Principe|Senegal|Seychelles|Sierra Leone|Somalia|South Africa|South Sudan|Sudan|Swaziland|Tanzania|Togo|Tunisia|Uganda|Zambia|Zimbabwe"))

africa_lm$expertise <-as.factor(africa_lm$expertise)

model8 <- lm(rating~expertise, data=africa_lm)

summary(model8)



#Asia Pacific experts (experts n=13)
asia_lm <- index_long
asia_lm <- asia_lm |> 
  select(respID, country, rating, expertise_region)  |> 
  group_by(expertise_region) |> 
  mutate(
    expertise = case_when(
      (grepl("Asia Pacific|APAC|EMEA|Eurasian", expertise_region)) ~ "asia_expert",
      (!grepl("Asia Pacific|APAC|EMEA|Eurasian", expertise_region)) ~ "not_asia_expert",
      .default = NA
    )
  )

asia_lm <- asia_lm |> 
  select(everything()) |> 
  filter(str_detect(country, "Afghanistan|Australia|Bahrain|Bangladesh|Bhutan|Brunei|Cambodia|China|Cook Islands|East Timor|Egypt|Fiji|India|Indonesia|Iran|Iraq|Israel|Japan|Jordan|Kazakhstan|Kiribati|Kuwait|Kyrgyzstan|Laos|Lebanon|Libya|Malaysia|Maldives|Marshall Islands|Micronesia|Mongolia|Myanmar|Nauru|Nepal|New Zealand|Niue|North Korea|Korea, North|Korea, South|Oman|Pakistan|Palau|Palestine|Papua New Guinea|Philippines|Qatar|Samoa|Saudi Arabia|Singapore|Solomon Islands|South Korea|Sri Lanka|Syria|Tajikistan|Thailand|Tonga|Turkmenistan|Tuvalu|Uzbekistan|United Arab Emirates|Vanuatu|Vietnam|Yemen"))

asia_lm$expertise <-as.factor(asia_lm$expertise)

model9 <- lm(rating~expertise, data=asia_lm)

summary(model9)



#Europe experts (experts n=42)
europe_lm <- index_long
europe_lm <- europe_lm |> 
  select(respID, country, rating, expertise_region)  |> 
  group_by(expertise_region) |> 
  mutate(
    expertise = case_when(
      (grepl("Europe|BiH|EMEA|Eurasian|ex-USSR|RuNet|UK|EU|Nordics", expertise_region)) ~ "europe_expert",
      (!grepl("Europe|BiH|EMEA|Eurasian|ex-USSR|RuNet|UK|EU|Nordics", expertise_region)) ~ "not_europe_expert",
      .default = NA
    )
  )

europe_lm <- europe_lm |> 
  select(everything()) |> 
  filter(str_detect(country, "Albania|Andorra|Armenia|Austria|Azerbaijan|Belarus|Belgium|Bosnia and Herzegovina|Bulgaria|Croatia|Cyprus|Czech Republic|Denmark|Estonia|Finland|France|Georgia|Germany|Greece|Hungary|Iceland|Ireland|Italy|Kazakhstan|Latvia|Liechtenstein|Lithuania|Luxembourg|Malta|Moldova|Monaco|Montenegro|Netherlands|North Macedonia|Norway|Poland|Portugal|Romania|Russia|San Marino|Serbia|SLovakia|Slovenia|Spain|Sweden|Switzerland|Turkey|Ukraine|United Kingdom|Vatican City"))

asia_lm$expertise <-as.factor(asia_lm$expertise)

model10 <- lm(rating~expertise, data=europe_lm)

summary(model10)



#north america experts (experts n=3)
north_lm <- index_long
north_lm <- north_lm |> 
  select(respID, country, rating, expertise_region)  |> 
  group_by(expertise_region) |> 
  mutate(
    expertise = case_when(
      (grepl("North America", expertise_region)) ~ "north_expert",
      (!grepl("North America", expertise_region)) ~ "not_north_expert",
      .default = NA
    )
  )

north_lm <- north_lm |> 
  select(everything()) |> 
  filter(str_detect(country, "Antigua and Barbuda|Bahamas|Barbados|Belize|Canada|Costa Rica|Cuba|Dominica|Dominican Republic|El Salvador|Grenada|Guatemala|Haiti|Honduras|Jamaica|Mexico|Nicaragua|Panama|Saint Kitts and Nevis|Saint Lucia|Saint Vincent and the Grenadines|Trinidad and Tobago|United States"))

north_lm$expertise <-as.factor(north_lm$expertise)

model11 <- lm(rating~expertise, data=north_lm)

summary(model11)




#south america experts (experts n=5)
south_lm <- index_long
south_lm <- south_lm |> 
  select(respID, country, rating, expertise_region)  |> 
  group_by(expertise_region) |> 
  mutate(
    expertise = case_when(
      (grepl("South America|Latin America", expertise_region)) ~ "south_expert",
      (!grepl("South America|Latin America", expertise_region)) ~ "not_south_expert",
      .default = NA
    )
  )

south_lm <- south_lm |> 
  select(everything()) |> 
  filter(str_detect(country, "Argentina|Bolivia|Brazil|Chile|Colombia|Ecuador|Guyana|Paraguay|Peru|Suriname|Uruguay|Venezuela"))

south_lm$expertise <-as.factor(south_lm$expertise)

model12 <- lm(rating~expertise, data=south_lm)

summary(model12)







#T-score
Tscore <- tech_index[c(1:11, 13:14, 24:25), c(3,8)]

Tscore <- full_join(Tscore, attack_index[c(1:11, 13, 23, 28, 36), c(3,8)], by="Country")
colnames(Tscore) <- c("Country", "wci_tech", "wci_attack")

Tscore <- full_join(Tscore, data_index[c(1:12, 15, 21, 52), c(3,8)], by="Country")
colnames(Tscore) <- c("Country", "wci_tech", "wci_attack", "wci_data")

Tscore <- full_join(Tscore, WCI_scams[c(1:11, 19, 30, 33, 67), c(3,8)], by="Country")
colnames(Tscore) <- c("Country", "wci_tech", "wci_attack", "wci_data", "wci_scams")

Tscore <- full_join(Tscore, WCI_cash[c(1:7, 9, 12, 15:16, 18, 25, 48, 76), c(3,8)], by="Country")
colnames(Tscore) <- c("country", "wci_tech",  "wci_attack", "wci_data", "wci_scams", "wci_cash")

Tscore$wci_scams <- Tscore$wci_scams |> replace_na(0)
Tscore$wci_cash <- Tscore$wci_cash |> replace_na(0)

Tscore$wci_tech <- Tscore$wci_tech*2
Tscore$wci_attack <- Tscore$wci_attack*1
Tscore$wci_data <- Tscore$wci_data*0
Tscore$wci_scams <- Tscore$wci_scams*-1
Tscore$wci_cash <- Tscore$wci_cash*-2

Tscore <- Tscore |> 
  mutate(t_score=c(wci_tech+wci_attack+wci_data+wci_scams+wci_cash))

colnames(Tscore) <- c("Country", "wci_tech",  "wci_attack", "wci_data", "wci_scams", "wci_cash", "t_score")

Tscore <- full_join(Tscore, overall_index[c(1:15),c(3,8)], by="Country")

colnames(Tscore) <- c("country", "wci_tech",  "wci_attack", "wci_data", "wci_scams", "wci_cash", "t_score", "wci_score")

Tscore$t_score <- round(Tscore$t_score, digits=2)

flatplot <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)


#Tscore plot, no color
ggplot(data=Tscore, aes(x=t_score, y=flatplot, label=country)) +
  geom_line()+
  geom_point(size=3)+
  geom_vline(xintercept=c(0,0), linetype="dotted")+
  geom_label_repel(point.size=5, max.overlaps = Inf, min.segment.length = 0)+
  labs(title=expression("T-score of the Top 15 "*WCI[overall]*" countries"), y=NULL, x="T-score")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank() 
  )+
  expand_limits(y = c(-1, 1), x=c(-125, 125))


#Tscore plot, color
ggplot(data=Tscore, aes(x=t_score, y=flatplot, label=country)) +
  geom_line()+
  geom_point(size=3, color = ifelse(Tscore$t_score < 0, "red", "black"))+
  geom_vline(xintercept=c(0,0), linetype="dotted")+
  geom_label_repel(point.size=5, max.overlaps = Inf, min.segment.length = 0)+
  labs(y=NULL, x="T-score", color="T-score")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank() 
  )+
  expand_limits(y = c(-0.2, 0.2), x=c(-140, 100))


ggsave("Tscore_pnas.png", dpi=200)


#NOTE TO SELF deleted "heatmap" code bc didnt generate in R in the end


#Participant nationality, nationality region, residence, and residence region
nationality.region.tab <-  index_long$nationality_region |>
  replace_na("Prefer not to say")
nationality.region.tab <- table(nationality.region.tab)
nationality.region.tab <- as.data.frame(nationality.region.tab)
colnames(nationality.region.tab) <- c("Nationality (region)", "Frequency")
nationality.region.tab <-  nationality.region.tab |> 
  mutate(Frequency=Frequency/75) |> 
  arrange(desc(Frequency))
nationality.region.tab <-  nationality.region.tab |> 
  mutate(Percentage=formattable::percent(Frequency/92))
colnames(nationality.region.tab) <- c("Nationality (region)", "Frequency", "Percentage")


nationality.tab <-  index_long$nationality |> 
  replace_na("Prefer not to say")
nationality.tab <- table(nationality.tab)
nationality.tab <- as.data.frame(nationality.tab)
colnames(nationality.tab) <- c("Nationality (country)", "Frequency")
nationality.tab <-  nationality.tab |> 
  mutate(Frequency=Frequency/75) |> 
  arrange(desc(Frequency))
nationality.tab <-  nationality.tab |> 
  mutate(Percentage=formattable::percent(Frequency/92))
colnames(nationality.tab) <- c("Nationality (country)", "Frequency", "Percentage")



residence.tab <- index_long$residence |> 
  replace_na("Prefer not to say")
residence.tab <- table(residence.tab)
residence.tab <- as.data.frame(residence.tab)
colnames(residence.tab) <- c("Residence (country)", "Frequency")
residence.tab <- residence.tab |> 
  mutate(Frequency=Frequency/75) |> 
  arrange(desc(Frequency))
residence.tab <-  residence.tab |> 
  mutate(Percentage=formattable::percent(Frequency/92))
colnames(residence.tab) <- c("Residence (country)", "Frequency", "Percentage")


residence.region.tab <- index_long$residence_region |> 
  replace_na("Prefer not to say")
residence.region.tab <- table(residence.region.tab)
residence.region.tab <- as.data.frame(residence.region.tab)
colnames(residence.region.tab) <- c("Residence (region)", "Frequency")
residence.region.tab <- residence.region.tab |> 
  mutate(Frequency=Frequency/75) |> 
  arrange(desc(Frequency))
residence.region.tab <-  residence.region.tab |> 
  mutate(Percentage=formattable::percent(Frequency/92))
colnames(residence.region.tab) <- c("Residence (region)", "Frequency", "Percentage")


#SUPPLEMENTARY INFO#

##Expert crimetypes and regions##
#30.6.23

expertise_bind <- left_join(expertise_crimetype_long, expertise_region_long)
expertise_bind[expertise_bind==""] <- NA


expertise_tech <-  expertise_bind |> 
  select(respID:`expertise_crimetype`) |> 
  filter(str_detect(`expertise_crimetype`, regex("tech", ignore_case = T))) |> 
  mutate(no.tech.expert = n_distinct(expertise_tech$respID))|> 
  mutate(percentage.tech=formattable::percent(no.tech.expert/92)) |> 
  mutate(tech="tech")

expertise_attack <- expertise_bind |> 
  select(respID:`expertise_crimetype`) |> 
  filter(str_detect(`expertise_crimetype`, regex("attack", ignore_case = T)))|> 
  mutate(no.attack.expert = n_distinct(expertise_attack$respID))|> 
  mutate(percentage.attack=formattable::percent(no.attack.expert/92)) |> 
  mutate(attack="attack")

expertise_cash <- expertise_bind |> 
  select(respID:`expertise_crimetype`) |> 
  filter(str_detect(`expertise_crimetype`, regex("cash", ignore_case = T)))|> 
  mutate(no.cash.expert = n_distinct(expertise_cash$respID))|> 
  mutate(percentage.cash=formattable::percent(no.cash.expert/92)) |> 
  mutate(cash="cash")

expertise_data <- expertise_bind |> 
  select(respID:`expertise_crimetype`) |> 
  filter(str_detect(`expertise_crimetype`, regex("data", ignore_case = T)))|> 
  mutate(no.data.expert = n_distinct(expertise_data$respID))|> 
  mutate(percentage.data=formattable::percent(no.data.expert/92)) |> 
  mutate(data="data")

expertise_scams <- expertise_bind |> 
  select(respID:`expertise_crimetype`) |> 
  filter(str_detect(`expertise_crimetype`, regex("scam", ignore_case = T))) |> 
  mutate(no.scam.expert = n_distinct(expertise_scams$respID))|> 
  mutate(percentage.scam=formattable::percent(no.scam.expert/92)) |> 
  mutate(scams="scams")

expertise_other <- expertise_bind |> 
  select(respID:`expertise_crimetype`) |> 
  filter(str_detect(`expertise_crimetype`, regex("other", ignore_case = T))) |> 
  mutate(no.other.expert = n_distinct(expertise_other$respID))|> 
  mutate(percentage.other=formattable::percent(no.other.expert/92)) |> 
  mutate(other="other")

expertise_none<- expertise_bind |> 
  select(respID:`expertise_crimetype`) |> 
  filter(str_detect(`expertise_crimetype`, regex("NA,", ignore_case = T))) |> 
  mutate(no.none.expert = n_distinct(expertise_none$respID))|> 
  mutate(percentage.none=formattable::percent(no.none.expert/92)) |> 
  mutate(none="none")

expertise_crimetype_bind <- left_join(expertise_bind, expertise_attack)
expertise_crimetype_bind <- left_join(expertise_crimetype_bind, expertise_cash)
expertise_crimetype_bind <- left_join(expertise_crimetype_bind, expertise_data)
expertise_crimetype_bind <- left_join(expertise_crimetype_bind, expertise_scams)
expertise_crimetype_bind <- left_join(expertise_crimetype_bind, expertise_tech)
expertise_crimetype_bind <- left_join(expertise_crimetype_bind, expertise_other)
expertise_crimetype_bind <- left_join(expertise_crimetype_bind, expertise_none)


view(expertise_crimetype_bind)


expertise.crimetype.freqpercent <-expertise_crimetype_bind |> 
  select(no.attack.expert:percentage.none) |> 
  distinct(.keep_all=T)

expertise.crimetype.freqpercent <-  pivot_longer(expertise.crimetype.freqpercent, cols = c(no.data.expert, no.attack.expert, no.cash.expert, no.scam.expert, no.tech.expert, no.other.expert, no.none.expert), names_to="expertise", values_to="number experts")

expertise.crimetype.freqpercent <-  pivot_longer(expertise.crimetype.freqpercent, cols = c(percentage.cash, percentage.data, percentage.scam, percentage.tech, percentage.attack, percentage.other, percentage.none), names_to="exp percentage", values_to="percentage total")

view(expertise.crimetype.freqpercent)




expert.attack <- expertise_crimetype_bind |> 
  distinct(respID, .keep_all = T) |> 
  summarise("no.attack.experts"=sum(!is.na(expertise_crimetype_bind$no.attack.expert)))

expert.data <- expertise_crimetype_bind |> 
  distinct(respID, .keep_all = T) |> 
  summarise("no.data.experts"=sum(!is.na(expertise_crimetype_bind$no.data.expert)))

expert.cash <- expertise_crimetype_bind |> 
  distinct(respID, .keep_all = T) |> 
  summarise("no.cash.experts"=sum(!is.na(expertise_crimetype_bind$no.cash.expert)))

expert.scams <- expertise_crimetype_bind |> 
  distinct(respID, .keep_all = T) |> 
  summarise("no.scam.experts"=sum(!is.na(expertise_crimetype_bind$no.scam.expert)))

expert.tech <- expertise_crimetype_bind |> 
  distinct(respID, .keep_all = T) |> 
  summarise("no.tech.experts"=sum(!is.na(expertise_crimetype_bind$no.tech.expert)))

expert.other<- expertise_crimetype_bind |> 
  distinct(respID, .keep_all = T) |> 
  summarise("no.other.experts"=sum(!is.na(expertise_crimetype_bind$no.other.expert)))

expert.none<- expertise_crimetype_bind |> 
  distinct(respID, .keep_all = T) |> 
  summarise("no.none.experts"=sum(!is.na(expertise_crimetype_bind$no.none.expert)))

expert.crimebind <- bind_rows(expert.attack, expert.data, expert.cash, expert.scams, expert.tech, expert.other, expert.none)

expert.crimebind <- pivot_longer(expert.crimebind, cols = c(no.attack.experts:no.none.experts), names_to ="expertise", values_to = "number.experts" )

expertise.crimetype.freqpercent <- expert.crimebind |> 
  drop_na(number.experts) |> 
  mutate(Percentage.of.respondents=formattable::percent(number.experts/92)) |> 
  mutate(Percentage.of.experts=formattable::percent(number.experts/85)) |> 
  arrange(expertise)


expertise.crimetype.freqpercent[expertise.crimetype.freqpercent == "no.attack.experts"] <- "Attacks and extortion"
expertise.crimetype.freqpercent[expertise.crimetype.freqpercent == "no.data.experts"] <- "Data and identity theft"
expertise.crimetype.freqpercent[expertise.crimetype.freqpercent == "no.cash.experts"] <- "Cashing out and money laundering"
expertise.crimetype.freqpercent[expertise.crimetype.freqpercent == "no.scam.experts"] <- "Scams"
expertise.crimetype.freqpercent[expertise.crimetype.freqpercent == "no.tech.experts"] <- "Technical products and services"
expertise.crimetype.freqpercent[expertise.crimetype.freqpercent == "no.none.experts"] <- "None"
expertise.crimetype.freqpercent[expertise.crimetype.freqpercent == "no.other.experts"] <- "Other"


colnames(expertise.crimetype.freqpercent) <- c("Expertise", "No.", "% respondent pool", "% expert pool")

expertise.crimetype.freqpercent <- expertise.crimetype.freqpercent[c("Expertise", "No.", "% expert pool", "% respondent pool")]

expertise.crimetype.freqpercent <- expertise.crimetype.freqpercent |> 
  arrange(desc(No.)) |> 
  distinct(Expertise, .keep_all = T) 




##Expert regions dfs
#expertise region df

expert_asiapacific_df<- group_by(index_long, expertise_region) |> 
  filter(str_detect(expertise_region, "Afghanistan|Australia|Bahrain|Bangladesh|Bhutan|Brunei|Cambodia|China|Cook Islands|East Timor|Egypt|Fiji|India|Indonesia|Iran|Iran |Iraq|Israel|Japan|Jordan|Kazakhstan|Kiribati|Kuwait|Kyrgyzstan|Laos|Lebanon|Libya|Malaysia|Maldives|Marshall Islands|Micronesia|Mongolia|Myanmar|Nauru|Nepal|New Zealand|Niue|North Korea|Oman|Pakistan|Palau|Palestine|Papua New Guinea|Philippines|Qatar|Samoa|Saudi Arabia|Singapore|Solomon Islands|South Korea|Sri Lanka|Syria|Tajikistan|Thailand|Tonga|Turkmenistan|Tuvalu|Uzbekistan|United Arab Emirates|Vanuatu|Vietnam|Yemen|APAC|EMEA|Eurasian|HK|Asia Pacific|Asia"))


expert_africa_df <- group_by(index_long, expertise_region) |> 
  filter(str_detect(expertise_region, "Algeria|Angola|Benin|Botswana|Burkina Faso|Burundi|Cameroon|Cape Verde|Central African Republic|Chad|Comoros|Congo, Democratic Republic|Congo, Republic|Djibouti|Egypt|Equatorial Guinea|Eritrea|Eswatini|Ethiopia|Gabon|Gambia|Ghana|Guinea|Guinea-Bissau|Ivory Coast|Kenya|Lesotho|Liberia|Libya|Madagascar|Malawi|Mali|Mauritania|Mauritius|Morocco|Mozambique|Namibia|Niger|Nigeria|Rwanda|Sao Tome and Principe|Senegal|Seychelles|Sierra Leone|Somalia|South Africa|South Sudan|Sudan|Swaziland|Tanzania|Togo|Tunisia|Uganda|Zambia|Zimbabwe|EMEA|Africa|Congo"))



expert_europe_df<- group_by(index_long, expertise_region) |> 
  filter(str_detect(expertise_region,"Albania|Andorra|Armenia|Austria|Azerbaijan|Belarus|Belgium|Bosnia and Herzegovina|Bulgaria|Croatia|Cyprus|Czech Republic|Denmark|Estonia|Finland|France|Georgia|Germany|Greece|Hungary|Iceland|Ireland|Italy|Kazakhstan|Latvia|Liechtenstein|Lithuania|Luxembourg|Malta|Moldova|Monaco|Montenegro|Netherlands|North Macedonia|Norway|Poland|Portugal|Romania|Russia|San Marino|Serbia|SLovakia|Slovenia|Spain|Sweden|Switzerland|Turkey|Ukraine|United Kingdom|Vatican City|BiH|EMEA|Eurasian|ex-USSR|RuNet|UK|EU|Nordics|Eastern Europe|Europe"))


expert_namer_df <- group_by(index_long, expertise_region) |> 
  filter(str_detect(expertise_region, "Antigua and Barbuda|Bahamas|Barbados|Belize|Canada|Costa Rica|Cuba|Dominica|Dominican Republic|El Salvador|Grenada|Guatemala|Haiti|Honduras|Jamaica|Mexico|Nicaragua|Panama|Saint Kitts and Nevis|Saint Lucia|Saint Vincent and the Grenadines|Trinidad and Tobago|United States|North America"))


expert_samer_df <- group_by(index_long, expertise_region) |> 
  filter(str_detect(expertise_region, "South America|Argentina|Bolivia|Brazil|Chile|Colombia|Ecuador|Guyana|Paraguay|Peru|Suriname|Uruguay|Venezuela|Latin America|LATAM"))


expert_other_region_df <- group_by(index_long, expertise_region) |> 
  filter(!str_detect(expertise_region, "Afghanistan|Australia|Bahrain|Bangladesh|Bhutan|Brunei|Cambodia|China|Cook Islands|East Timor|Egypt|Fiji|India|Indonesia|Iran|Iran |Iraq|Israel|Japan|Jordan|Kazakhstan|Kiribati|Kuwait|Kyrgyzstan|Laos|Lebanon|Libya|Malaysia|Maldives|Marshall Islands|Micronesia|Mongolia|Myanmar|Nauru|Nepal|New Zealand|Niue|North Korea|Oman|Pakistan|Palau|Palestine|Papua New Guinea|Philippines|Qatar|Samoa|Saudi Arabia|Singapore|Solomon Islands|South Korea|Sri Lanka|Syria|Tajikistan|Thailand|Tonga|Turkmenistan|Tuvalu|Uzbekistan|United Arab Emirates|Vanuatu|Vietnam|Yemen|APAC|EMEA|Eurasian|HK|Asia Pacific|Asia|Algeria|Angola|Benin|Botswana|Burkina Faso|Burundi|Cameroon|Cape Verde|Central African Republic|Chad|Comoros|Congo, Democratic Republic|Congo, Republic|Djibouti|Egypt|Equatorial Guinea|Eritrea|Eswatini|Ethiopia|Gabon|Gambia|Ghana|Guinea|Guinea-Bissau|Ivory Coast|Kenya|Lesotho|Liberia|Libya|Madagascar|Malawi|Mali|Mauritania|Mauritius|Morocco|Mozambique|Namibia|Niger|Nigeria|Rwanda|Sao Tome and Principe|Senegal|Seychelles|Sierra Leone|Somalia|South Africa|South Sudan|Sudan|Swaziland|Tanzania|Togo|Tunisia|Uganda|Zambia|Zimbabwe|EMEA|Africa|Congo|Albania|Andorra|Armenia|Austria|Azerbaijan|Belarus|Belgium|Bosnia and Herzegovina|Bulgaria|Croatia|Cyprus|Czech Republic|Denmark|Estonia|Finland|France|Georgia|Germany|Greece|Hungary|Iceland|Ireland|Italy|Kazakhstan|Latvia|Liechtenstein|Lithuania|Luxembourg|Malta|Moldova|Monaco|Montenegro|Netherlands|North Macedonia|Norway|Poland|Portugal|Romania|Russia|San Marino|Serbia|SLovakia|Slovenia|Spain|Sweden|Switzerland|Turkey|Ukraine|United Kingdom|Vatican City|BiH|EMEA|Eurasian|ex-USSR|RuNet|UK|EU|Nordics|Eastern Europe|Europe|Antigua and Barbuda|Bahamas|Barbados|Belize|Canada|Costa Rica|Cuba|Dominica|Dominican Republic|El Salvador|Grenada|Guatemala|Haiti|Honduras|Jamaica|Mexico|Nicaragua|Panama|Saint Kitts and Nevis|Saint Lucia|Saint Vincent and the Grenadines|Trinidad and Tobago|United States|North America|Antigua and Barbuda|Bahamas|Barbados|Belize|Canada|Costa Rica|Cuba|Dominica|Dominican Republic|El Salvador|Grenada|Guatemala|Haiti|Honduras|Jamaica|Mexico|Nicaragua|Panama|Saint Kitts and Nevis|Saint Lucia|Saint Vincent and the Grenadines|Trinidad and Tobago|United States|North America|South America|Argentina|Bolivia|Brazil|Chile|Colombia|Ecuador|Guyana|Paraguay|Peru|Suriname|Uruguay|Venezuela|Latin America|LATAM|APAC|EMEA|Eurasian|Eurasian|Nordics"))



expert_africa_df <-  expert_africa_df |> 
  ungroup() |> 
  select(respID, expertise_region) |> 
  mutate(no.africa.expert = n_distinct(respID))|> 
  mutate(percentage.africa=formattable::percent(no.africa.expert/92)) |> 
  mutate(africa="africa")|> 
  distinct(respID, .keep_all = T)

expert_namer_df <-  expert_namer_df |> 
  ungroup() |> 
  select(respID, expertise_region) |> 
  mutate(no.namer.expert = n_distinct(respID))|> 
  mutate(percentage.namer=formattable::percent(no.namer.expert/92)) |> 
  mutate(northamerica="northamerica") |> 
  distinct(respID, .keep_all = T)

expert_europe_df <-  expert_europe_df |> 
  ungroup() |> 
  select(respID, expertise_region) |> 
  mutate(no.europe.expert = n_distinct(respID))|> 
  mutate(percentage.europe=formattable::percent(no.europe.expert/92)) |> 
  mutate(europe="europe")|> 
  distinct(respID, .keep_all = T)

expert_samer_df <-  expert_samer_df |> 
  ungroup() |> 
  select(respID, expertise_region) |> 
  mutate(no.samer.expert = n_distinct(respID))|> 
  mutate(percentage.samer=formattable::percent(no.samer.expert/92)) |> 
  mutate(southamerica="southamerica")|> 
  distinct(respID, .keep_all = T)

expert_asiapacific_df <-  expert_asiapacific_df |> 
  ungroup() |> 
  select(respID, expertise_region) |> 
  mutate(no.asiapacific.expert = n_distinct(respID))|> 
  mutate(percentage.asiapacific=formattable::percent(no.asiapacific.expert/92)) |> 
  mutate(asiapacific="asiapacific")|> 
  distinct(respID, .keep_all = T)

expert_other_region_df <-  expert_other_region_df |> 
  ungroup() |> 
  select(respID, expertise_region) |> 
  mutate(no.otherregion.expert = n_distinct(respID))|> 
  mutate(percentage.otherregion=formattable::percent(no.otherregion.expert/92)) |> 
  mutate(otherregion="otherregion")|> 
  distinct(respID, .keep_all = T)

expertise_none_region<- expertise_bind |> 
  select(respID,`expertise_region`) |> 
  distinct(respID, .keep_all = T) |> 
  filter(is.na(`expertise_region`))|> 
  mutate(no.noneregion.expert = n_distinct(expertise_none_region$respID))|> 
  mutate(percentage.noneregion=formattable::percent(no.noneregion.expert/92)) |> 
  mutate(noneregion="noneregion")|> 
  distinct(respID, .keep_all = T)


expertise_region_bind <- left_join(expertise_bind, expert_africa_df)

expertise_region_bind <- left_join(expertise_region_bind, expert_namer_df, by="respID")

expertise_region_bind <- left_join(expertise_region_bind, expert_samer_df, by="respID")

expertise_region_bind <- left_join(expertise_region_bind, expert_asiapacific_df, by="respID")

expertise_region_bind <- left_join(expertise_region_bind, expert_europe_df, by="respID")

expertise_region_bind <- left_join(expertise_region_bind, expertise_none_region, by="respID")

expertise_region_bind <- left_join(expertise_region_bind, expert_other_region_df, by="respID")

view(expertise_region_bind)

expert.africa <- expertise_region_bind |> 
  distinct(respID) |> 
  summarise("no.africa.experts"=sum(!is.na(expertise_region_bind$no.africa.expert)))

expert.asiapacific <- expertise_region_bind |> 
  distinct(respID) |> 
  summarise("no.asiapacific.experts"=sum(!is.na(expertise_region_bind$no.asiapacific.expert)))

expert.europe <- expertise_region_bind |> 
  distinct(respID) |> 
  summarise("no.europe.experts"=sum(!is.na(expertise_region_bind$no.europe.expert)))

expert.namer <- expertise_region_bind |> 
  distinct(respID) |> 
  summarise("no.namer.expert"=sum(!is.na(expertise_region_bind$no.namer.expert)))

expert.samer <- expertise_region_bind |> 
  distinct(respID) |> 
  summarise("no.samer.experts"=sum(!is.na(expertise_region_bind$no.samer.expert)))

expert.noneregion<- expertise_region_bind |> 
  distinct(respID) |> 
  summarise("no.noneregion.experts"=sum(!is.na(expertise_region_bind$no.noneregion.expert)))

expert.otherregion<- expertise_region_bind |> 
  distinct(respID) |> 
  summarise("no.otherregion.experts"=sum(!is.na(expertise_region_bind$no.otherregion.expert)))

expert.regions.all <- bind_rows(expert.asiapacific, expert.africa, expert.europe, expert.namer, expert.noneregion, expert.samer, expert.otherregion)

expert.regions.all <- expert.regions.all |> 
  select(no.asiapacific.experts:no.otherregion.experts)

expert.regions.all <- pivot_longer(expert.regions.all, cols = c(no.asiapacific.experts:no.otherregion.experts), names_to ="expertise", values_to = "number.experts" )

expertise.regions.freqpercent <- expert.regions.all |> 
  drop_na(number.experts) |> 
  mutate(Percentage.of.respondents=formattable::percent(number.experts/92)) |> 
  mutate(Percentage.of.experts=formattable::percent(number.experts/50)) |> 
  arrange(expertise)

expertise.regions.freqpercent[expertise.regions.freqpercent == "no.africa.experts"] <- "Africa"
expertise.regions.freqpercent[expertise.regions.freqpercent == "no.asiapacific.experts"] <- "Asia Pacific"
expertise.regions.freqpercent[expertise.regions.freqpercent == "no.europe.experts"] <- "Europe"
expertise.regions.freqpercent[expertise.regions.freqpercent == "no.namer.expert"] <- "North America"
expertise.regions.freqpercent[expertise.regions.freqpercent == "no.noneregion.experts"] <- "None"
expertise.regions.freqpercent[expertise.regions.freqpercent == "no.samer.experts"] <- "South America"
expertise.regions.freqpercent[expertise.regions.freqpercent == "no.otherregion.experts"] <- "Other"

colnames(expertise.regions.freqpercent) <- c("Expertise", "No.", "% respondent pool", "% expert pool")


expertise.regions.freqpercent <- expertise.regions.freqpercent |> 
  arrange(desc("No."))


expertise.regions.freqpercent <- expertise.regions.freqpercent[c("Expertise", "No.", "% expert pool", "% respondent pool")] |> 
  distinct(Expertise, .keep_all = T) 



##Expert boxplots##
#expert_tech_df<- group_by(index_long, expertise_crimetype) |> 
  #filter(str_detect(expertise_crimetype, "Technical"))
#expert_tech_df$crimetype <- "Technical"

#expert_attack_df<- group_by(index_long, expertise_crimetype) |> 
  #filter(str_detect(expertise_crimetype, "Attack"))

#expert_scam_df<- group_by(index_long, expertise_crimetype) |> 
  #filter(str_detect(expertise_crimetype, "Scam"))

#expert_data_df<- group_by(index_long, expertise_crimetype) |> 
  #filter(str_detect(expertise_crimetype, "Data")) 

#expert_cash_df<- group_by(index_long, expertise_crimetype) |> 
  #filter(str_detect(expertise_crimetype, "Cash"))

#expert_other_crimetype_df<- group_by(index_long, expertise_crimetype) |> 
  #filter(!str_detect(expertise_crimetype, "Attack|Scam|Cash|Data|Technical"))


#plots
#expert_tech_plot <-
  #ggplot(expert_tech_df, 
         #aes(x=crimetype,
            # y=rating))

#expert_tech_plot+
 # geom_boxplot(aes(fill=crimetype))+
 # coord_flip()+
 # labs(title="How technical experts rated different crime types", y="Rating", x="Crime type")+
  #theme(legend.position = "none")


#expert_attack_plot <- ggplot(expert_attack_df, 
                          #   aes(x=crimetype,
                               #  y=rating))

#expert_attack_plot+
#  geom_boxplot(aes(fill=crimetype))+
#  coord_flip()+
#  labs(title="How attack experts rated different crime types", y="Rating", x="Crime type")+
#  theme(legend.position="none")

#expert_data_plot <- ggplot(expert_data_df, 
 #                          aes(x=crimetype,
  #                             y=rating))

#expert_data_plot+
#  geom_boxplot(aes(fill=crimetype))+
#  coord_flip()+
#  labs(title="How data experts rated different crime types", y="Rating", x="Crime type")+
#  theme(legend.position="none")

#expert_scam_plot <- ggplot(expert_scam_df, 
                        #   aes(x=crimetype,
                        #       y=rating))

#expert_scam_plot+
#  geom_boxplot(aes(fill=crimetype))+
#  coord_flip()+
#  labs(title="How scam experts rated different crime types", y="Rating", x="Crime type")+
#  theme(legend.position="none")


#expert_cash_plot <- ggplot(expert_cash_df, 
 #                          aes(x=crimetype,
 #                              y=rating))

#expert_cash_plot+
#  geom_boxplot(aes(fill=crimetype))+
#  coord_flip()+
#  labs(title="How cashing out experts rated different crime types", y="Rating", x="Crime type")+
#  theme(legend.position="none")

#expert_other_plot <- ggplot(expert_other_crimetype_df, 
 #                           aes(x=crimetype,
 #                               y=rating))

#expert_other_plot+
 # geom_violin(aes(fill=crimetype))+
 # coord_flip()+
 # labs(title="How other experts rated different crime types", y="Rating", x="Crime type")+
 # theme(legend.position="none")



#plots for region expertise
#expert_europe_plot <-
#  ggplot(na.omit(expert_europe_df), 
 #        aes(x=country_region,
  #           y=rating))


#expert_europe_plot+
 # geom_violin(aes(fill=country_region))+
 # coord_flip()+
 # labs(title="How European experts rated different regions", y="Rating", x=NULL)+
 # theme(legend.position = "none")


#expert_africa_plot <- ggplot(na.omit(expert_africa_df), 
 #                            aes(x=country_region,
  #                               y=rating))

#expert_africa_plot+
  #geom_violin(aes(fill=country_region))+
 # coord_flip()+
 # labs(title="How African experts rated different regions", y="Rating", x=NULL)+
 # theme(legend.position="none")

#expert_asiapacific_plot <- ggplot(na.omit(expert_asiapacific_df), 
 #                                 aes(x=country_region,
 #                                     y=rating))

#expert_asiapacific_plot+
 # geom_violin(aes(fill=country_region))+
 # coord_flip()+
 # labs(title="How Asia Pacific experts rated different regions", y="Rating", x=NULL)+
 # theme(legend.position="none")

#expert_namer_plot <- ggplot(expert_namer_df, 
  #                          aes(x=country_region,
  #                              y=rating))

#expert_namer_plot+
 # geom_violin(aes(fill=country_region))+
 # coord_flip()+
 # labs(title="How North American experts rated different\n regions", y="Rating", x=NULL)+
 # scale_x_discrete(limits=c("North America", "Europe", "Asia Pacific", "Africa", "South America"))+
 # theme(legend.position="none")

#expert_samer_plot <- ggplot(expert_samer_df, 
  #                          aes(x=country_region,
  #                              y=rating))

#expert_samer_plot+
 # geom_violin(aes(fill=country_region))+
 # coord_flip()+
 # labs(title="How South American experts rated different\n regions", y="Rating", x=NULL)+
 # scale_x_discrete(limits=c("North America", "Europe", "Asia Pacific", "Africa", "South America"))+
 # theme(legend.position="none")+
 # geom_jitter(position=position_jitter(0.02))




#testing survey fatigue
#do overall/average scores for each country increase/decrease as survey progressed?
#row bind in survey order, then only keep countries that are repeated in each crimetype, then Mann-Kendall & Sen slope test


fatigue_df <- bind_rows(tech_index, attack_index)
fatigue_df <- bind_rows(fatigue_df, data_index)
fatigue_df <- bind_rows(fatigue_df, scams_index)
fatigue_df <- bind_rows(fatigue_df, cash_index)

fatigue_df <- fatigue_df |> 
  group_by(Country)  |> 
  filter( n() > 1 ) |> 
  view()

fatigue_df <- as.data.frame(fatigue_df)

#MK trend & sens slope test
install.packages("trend")
library(trend)
mk.test(fatigue_df$Overall)
sens.slope(fatigue_df$Overall)


#russia trend test, as an experiment
#fatigue_df_russ <- fatigue_df |> 
 # filter(str_detect(Country, "Russia"))

#mk.test(fatigue_df_russ$Overall)
#sens.slope(fatigue_df_russ$Overall)


#fatigue test all countries, not just ones nominated more than once
fatigue_df_all <- bind_rows(tech_index, attack_index)
fatigue_df_all <- bind_rows(fatigue_df_all, data_index)
fatigue_df_all <- bind_rows(fatigue_df_all, scams_index)
fatigue_df_all <- bind_rows(fatigue_df_all, cash_index)

mk.test(fatigue_df_all$Overall)
sens.slope(fatigue_df_all$Overall)



#insufficient effort response test
install.packages("careless")
library(careless)

careless_df <- index_import |> 
  select(where(is.numeric))

careless_long <- longstring(careless_df, avg = FALSE)
careless_avg <- longstring(careless_df, avg = TRUE)
boxplot(careless_avg$longstr) #produce a boxplot of the longstring index
boxplot(careless_avg$avgstr)

boxplot(careless_avg$longstr)$stats
boxplot(careless_avg$avgstr)$stats



#intra-individual response variability IRV test

# calculate the irv over all items
irv_total <- irv(careless_df)

#calculate the irv over all items + calculate the irv for each quarter of the questionnaire
irv_split <- irv(careless_df, split = TRUE, num.split = 5)
irv_split$respID <- 1:92
boxplot(irv_total)$stats

boxplot(irv_split$irv5) #produce a boxplot of the IRV for the fifth category
boxplot(irv_split$irv5)$stats
boxplot(irv_split$irv4)$stats
boxplot(irv_split$irv3)$stats
boxplot(irv_split$irv2)$stats
boxplot(irv_split$irv1)$stats
boxplot(irv_split$irv1, irv_split$irv2, irv_split$irv3, irv_split$irv4, irv_split$irv5)$stats

#ggplot(irv_split_long, aes(x = reorder(irv, progression, FUN = median), y = irv)) + geom_boxplot()

#irv_split_long <- pivot_longer(irv_split, cols=2:6, names_to = "progression", values_to = "irv")

#irv_lm <- lm(irv ~ progression, data = irv_split_long)
#summary(irv_lm)

#ggplot(irv_split_long, aes("progression", "irv"))+
 # geom_point()+
 # geom_smooth(method="lm", formula = irv~progression, se=FALSE)+
 # labs(title="IRV as survey progressed", x="Survey progression", y="IRV")


#ggplot(irv_split_long, aes(x = progression, y = irv)) + 
 # geom_point() + 
 # geom_abline(slope = coef(irv_lm), 
   #           intercept = coef(irv_lm)[["(Intercept)"]])



#standard deviation of all ratings
#rating_sd <- unlist(index_long[,17])
#sd(rating_sd, na.rm=TRUE)


#check list of dependencies#
#install.packages("renv")
#renv::dependencies() |> 
 # view()



save.image(file = "wci_df_reprex.Rdata")
