load("Card Payments.rda")

library(dplyr)

copy = data

### manipulate merchnum
merchnum_invalid = copy %>%
  filter(merchnum %in% c("", "0")) %>%
  select(merch.description) %>%
  unique()

merchnum_invalid$merchnum_2 = 1:771
merchnum_invalid$merchnum_2 = paste("M",merchnum_invalid$merchnum_2)

copy = left_join(copy, merchnum_invalid)

### manipulate state
merch.st_invalid = copy %>%
  filter(merch.state == "") %>%
  select(merch.description) %>%
  unique()

merch.st_invalid$merch.state_2 = 1:150

copy = left_join(copy, merch.st_invalid)


### manipulate zip
merch.zip_invalid = copy %>%
  filter(is.na(merch.zip)) %>%
  select(merch.description) %>%
  unique()

merch.zip_invalid$merch.zip_2 = 1:644
merch.zip_invalid$merch.zip_2 = paste("M",merch.zip_invalid$merch.zip_2)
merch.zip_invalid$merch.zip_2 = mutate(merch.zip_invalid, merch.zip_2 = as.character(merch.zip_2))

copy = left_join(copy, merch.zip_invalid$merch.zip_2)

#### replace merchnum
copy$merchnum = as.character(copy$merchnum)

a = copy %>%
  filter(merchnum %in% c("", "0")) %>%
  select(recordnum)

copy[a$recordnum, 4] = copy[a$recordnum, 11]

### Sanity check
copy %>%
  filter(merchnum %in% c("", "0")) %>%
  summarise(n())

#### replace merch.state

copy$merch.state = as.character(copy$merch.state)

a = copy %>%
  filter(merch.state == "") %>%
  select(recordnum)

copy[a$recordnum, 6] = copy[a$recordnum, 12]

### Sanity check
copy %>%
  filter(merch.state == "") %>%
  summarise(n())

#### replace merch.zip
copy$merch.zip = as.character(copy$merch.zip)

a = copy %>%
  filter(is.na(merch.zip)) %>%
  select(recordnum)

copy[a$recordnum, 7] = copy[a$recordnum, 13]

### Sanity check
copy %>%
  filter(is.na(merch.zip)) %>%
  summarise(n())

copy = copy[,1:10]

data = copy

save(data, file = "Card Payments_Cleaned.rda")
