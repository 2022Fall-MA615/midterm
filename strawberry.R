library(tidyverse)
library(magrittr)
library(readxl)
data_strawberry <- read_xlsx('/Users/yaron/study/615/data/strawberries-2022oct30-a.xlsx',col_names = T)

T <- NULL
cnames <- colnames(data_strawberry)
x <- 1:dim(data_strawberry)[2]
for(i in x){T <- c(T, dim(unique(data_strawberry[i]))[1])}
drop_cols <- cnames[which(T == 1)]
data_strawberry %<>% select(!all_of(drop_cols))

data_strawberry %<>% arrange(Year, State)

data_strawberry %<>% separate(col=`Data Item`,
                     into = c("Strawberries", "type", "items", "units"),
                     sep = ",",
                     fill = "right")


type_organic <- grep("organic", 
                     data_strawberry$type, 
                     ignore.case = T)
Domain_organic <- grep("organic", 
                       data_strawberry$Domain, 
                       ignore.case = T)
org_rows <- intersect(type_organic, Domain_organic)
strawb_organic <- data_strawberry %>% slice(org_rows, preserve = FALSE)
strawb_non_organic <- data_strawberry %>% filter(!row_number() %in% org_rows)
chem_rows <- grep("BEARING - APPLICATIONS", 
                  strawb_non_organic$type, 
                  ignore.case = T)
strawb_chem <- strawb_non_organic %>% slice(chem_rows, preserve = FALSE)
rm(type_organic, Domain_organic, org_rows, chem_rows,cnames,drop_cols,i,T,x)


before_cols = colnames(strawb_chem)
T = NULL
x = length(before_cols)
for(i in 1:x){
  b <- length(unlist(strawb_chem[,i] %>% unique()) )
  T <- c(T,b)
}
drop_cols <- before_cols[which(T == 1)]
strawb_chem %<>% select(!all_of(drop_cols))


strawb_chem %<>% separate(col=`Domain Category`, 
                          into = c("dc1", "chem_name"),
                          sep = ":", 
                          fill = "right")

strawb_chem %<>% select(Year, State, items, units, dc1, chem_name, Value)
strawb_chem %<>% rename(category = units)
strawb_chem$items <- str_remove_all(strawb_chem$items, "MEASURED IN ")
strawb_chem %<>% rename(units = items)
strawb_chem$dc1 <- str_remove_all(strawb_chem$dc1, "CHEMICAL, ")
strawb_chem$dc1 %>% unique()
strawb_chem %<>% rename(chem_types = dc1)

strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\(")
strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\)")
strawb_chem %<>% separate(col = chem_name,
                          into = c("chem_name","chem_code"),
                          sep = "=",
                          fill = "right"
) 
rm(b, before_cols,drop_cols,i,T,x)

before_cols = colnames(strawb_chem)
T = NULL
x = length(before_cols)
for(i in 1:x){
  b <- length(unlist(strawb_chem[,i] %>% unique()) )
  T <- c(T,b)
}
drop_cols <- before_cols[which(T == 1)]
strawb_chem %<>% select(!all_of(drop_cols))


strawb_chem %<>% separate(col=`Domain Category`, 
                          into = c("dc1", "chem_name"),
                          sep = ":", 
                          fill = "right")

strawb_chem %<>% select(Year, State, items, units, dc1, chem_name, Value)
strawb_chem %<>% rename(category = units)
strawb_chem$items <- str_remove_all(strawb_chem$items, "MEASURED IN ")
strawb_chem %<>% rename(units = items)
strawb_chem$dc1 <- str_remove_all(strawb_chem$dc1, "CHEMICAL, ")
strawb_chem$dc1 %>% unique()
strawb_chem %<>% rename(chem_types = dc1)

strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\(")
strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\)")
strawb_chem %<>% separate(col = chem_name,
                          into = c("chem_name","chem_code"),
                          sep = "=",
                          fill = "right"
) 
rm(b, before_cols,drop_cols,i,T,x)


before_cols = colnames(strawb_chem)
T = NULL
x = length(before_cols)
for(i in 1:x){
  b <- length(unlist(strawb_chem[,i] %>% unique()) )
  T <- c(T,b)
}
drop_cols <- before_cols[which(T == 1)]
strawb_chem %<>% select(!all_of(drop_cols))


strawb_chem %<>% separate(col=`Domain Category`, 
                          into = c("dc1", "chem_name"),
                          sep = ":", 
                          fill = "right")

strawb_chem %<>% select(Year, State, items, units, dc1, chem_name, Value)
strawb_chem %<>% rename(category = units)
strawb_chem$items <- str_remove_all(strawb_chem$items, "MEASURED IN ")
strawb_chem %<>% rename(units = items)
strawb_chem$dc1 <- str_remove_all(strawb_chem$dc1, "CHEMICAL, ")
strawb_chem$dc1 %>% unique()
strawb_chem %<>% rename(chem_types = dc1)

strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\(")
strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\)")
strawb_chem %<>% separate(col = chem_name,
                          into = c("chem_name","chem_code"),
                          sep = "=",
                          fill = "right"
) 
rm(b, before_cols,drop_cols,i,T,x)


before_cols = colnames(strawb_non_organic)
T = NULL
x = length(before_cols)
for(i in 1:x){
  b <- length(unlist(strawb_non_organic[,i] %>% unique()) )
  T <- c(T,b)
}
drop_cols <- before_cols[which(T == 1)]
strawb_non_organic %<>% select(!all_of(drop_cols))


strawb_non_organic %<>% separate(col=`Domain Category`, 
                          into = c("dc1", "chem_name"),
                          sep = ":", 
                          fill = "right")

strawb_non_organic %<>% select(Year, State, items, units, dc1, chem_name, Value)
strawb_non_organic %<>% rename(category = units)
strawb_non_organic$items <- str_remove_all(strawb_non_organic$items, "MEASURED IN ")
strawb_non_organic %<>% rename(units = items)
strawb_non_organic$dc1 <- str_remove_all(strawb_non_organic$dc1, "CHEMICAL, ")
strawb_non_organic$dc1 %>% unique()
strawb_non_organic %<>% rename(chem_types = dc1)

strawb_non_organic$chem_name <- str_remove_all(strawb_non_organic$chem_name, "\\(")
strawb_non_organic$chem_name <- str_remove_all(strawb_non_organic$chem_name, "\\)")
strawb_non_organic %<>% separate(col = chem_name,
                          into = c("chem_name","chem_code"),
                          sep = "=",
                          fill = "right"
) 
rm(b, before_cols,drop_cols,i,T,x)


before_cols = colnames(strawb_non_organic)
T = NULL
x = length(before_cols)
for(i in 1:x){
  b <- length(unlist(strawb_non_organic[,i] %>% unique()) )
  T <- c(T,b)
}
drop_cols <- before_cols[which(T == 1)]
strawb_non_organic %<>% select(!all_of(drop_cols))


strawb_non_organic %<>% separate(col=`Domain Category`, 
                          into = c("dc1", "chem_name"),
                          sep = ":", 
                          fill = "right")

strawb_non_organic %<>% select(Year, State, items, units, dc1, chem_name, Value)
strawb_non_organic %<>% rename(category = units)
strawb_non_organic$items <- str_remove_all(strawb_non_organic$items, "MEASURED IN ")
strawb_non_organic %<>% rename(units = items)
strawb_non_organic$dc1 <- str_remove_all(strawb_non_organic$dc1, "CHEMICAL, ")
strawb_non_organic$dc1 %>% unique()
strawb_non_organic %<>% rename(chem_types = dc1)

strawb_non_organic$chem_name <- str_remove_all(strawb_non_organic$chem_name, "\\(")
strawb_non_organic$chem_name <- str_remove_all(strawb_non_organic$chem_name, "\\)")
strawb_non_organic %<>% separate(col = chem_name,
                          into = c("chem_name","chem_code"),
                          sep = "=",
                          fill = "right"
) 
rm(b, before_cols,drop_cols,i,T,x)
