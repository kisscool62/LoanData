library(dplyr)


df <- read.csv("RMBS.csv")


df_na <- apply(df, MARGIN =  c(1, 2), FUN=gsub, pattern="ND,[0-9]|ND\\.[0-9]|^$", replacement=NA)
df_na <- data.frame(df_na)
names(df_na) <- names(df)

df_na_right_header <- df_na[4:nrow(df_na),]
df_names <- data.frame(apply(df_na[3,], FUN=identity, MARGIN=1))[,1]
names(df_names) <- c()
names(df_na_right_header) <- df_names

df_without_na_columns <- df_na_right_header[,which(unlist(lapply(df_na_right_header, function(x)!all(is.na(x)))))]


df_without_na_columns <- subset(df_without_na_columns, select=-c(
        `Length of Payment Holiday`, 
        `Litigation`, 
        `Redemption Date`, 
        `Months in Arrears Prior`, 
        `Default or Foreclosure`, 
        `Date of Default or Foreclosure`, 
        `Cumulative Recoveries`,
        `Mortgage Mandate`, 
        `Pre-payment Amount`, 
        `Pre-payment Date`, 
        `Cumulative Pre-payments`,
        `Interest Cap Rate`,
        `Additional Collateral Value`,
        `Date Last in Arrears`))

#La variable contient moins de 2 valeurs distinctes, elle ne sera pas disponible dans l'analyse
# a refaire
# df_without_na_columns <- subset(df_without_na_columns, select=-c(
#           `Borrower Year of Birth`, 
#           `Current Valuation Date`, 
#           `Foreign National`, 
#           `Geographic Region List`, 
#           `Loan Currency Denomination`, 
#           `Loan Origination Date`, 
#           `Loan Term`, 
#           `Mortgage Inscription`, 
#           `Original Balance`, 
#           `Original Loan to Value`, 
#           `Origination Channel / Arranging Bank or Division`, 
#           `Payment Frequency`, 
#           `Pool Cut-off Date`, 
#           `Pool Identifier`, 
#           `Purpose`, 
#           `Repayment Method`, 
#           `Resident`, 
#           `Secondary Income`, 
#           `Servicer Identifier`, 
#           `Valuation Date`))


nb_different <- function(x){length(unique(x))}

df_without_na <- na.omit(subset(df_without_na_columns, select = -which(apply(df_without_na_columns, MARGIN=2, FUN=nb_different) <2) ))

write.csv(df_without_na, "RMBS_cleaned.csv", row.names=FALSE)




        
        
