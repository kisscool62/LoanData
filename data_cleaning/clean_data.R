library(dplyr)


df <- read.csv("RMBSIT000183100220128.csv")


df_na <- apply(df, MARGIN =  c(1, 2), FUN=gsub, pattern="ND,[0-9]|^$", replacement=NA)
df_na <- data.frame(df_na)
names(df_na) <- names(df)

df_na_right_header <- df_na[4:nrow(df_na),]
df_names <- data.frame(apply(df_na[3,], FUN=identity, MARGIN=1))[,1]
names(df_names) <- c()
names(df_na_right_header) <- df_names

df_without_na_columns <- df_na_right_header[,which(unlist(lapply(df_na_right_header, function(x)!all(is.na(x)))))]

df_without_na <- na.omit(df_without_na_columns)


        
        
