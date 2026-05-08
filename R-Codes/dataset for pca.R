library(openxlsx)

data = pca_and_cluster_dataset
dim(data)
df1 = data[,colnames(data[1,1:7])] 
head(df1)
colnames(data[1,1:7])

df2 = data[,colnames(data[1,8:103])]
dim(df2)

colnames(df2)

df2$ID <- 1:189

df2 <- df2[c("ID", names(df2)[-which(names(df2) == "ID")])]

tail(df2)
summary(df2)

df2$Species <- apply(df2[, -c(1, ncol(df2))], 1, function(x) names(x[!is.na(x)]))
df = cbind(df1,df2)

dim(df2[, -c(1, ncol(df2))])

df3 = PCA
data <- df3 %>%
  separate_rows(Species, sep = ",\\s*")

result <- data %>%
  slice(rep(row_number(), each = 1))

cleaned_data <- na.omit(result)

dim(cleaned_data)
write.xlsx(cleaned_data,"PCA-New.xlsx", rowNames = FALSE)
 
