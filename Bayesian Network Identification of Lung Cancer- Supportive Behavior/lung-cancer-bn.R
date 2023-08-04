# Load and rename tolower colnames
# dataset source https://www.kaggle.com/datasets/nancyalaswad90/lung-cancer?resource=download
path <- ("/Users/farmingmachine/Documents/VENDRI/pascasarjana-task/data-science-practice/paper-uas/dataset/survey lung cancer.csv")
df <- read.csv(path)
head(df,10)
names(df) <- tolower(names(df))
colnames(df)

# fixing colnames with (.) to (_)
names(df) <- gsub(x = names(df), pattern = "\\.", replacement = "_")  

# removing 2 firscolnames
df <- subset(df, select = -c(gender, age))

# replacing lung_cancer Category (if YES = 2, if NO = 1)
df$lung_cancer[df$lung_cancer == 'YES'] <- 2
df$lung_cancer[df$lung_cancer == 'NO'] <- 1

#converting (if YES/2 == TRUE, NO/2 == FALSE)
# df <- df == 2

#convert as factor
for(j in 1:ncol(df)){
  df[,j] <- factor(as.numeric(df[,j]))
}

# ----- Break to Bayesian Network ------#
library(bnlearn)
require(bnlearn)

#learning structure
bn_df <- data.frame(df)
res <- hc(bn_df)
plot(res)

#Removing link from plot (if necessary)
res$arcs #getting result the link
res$nodes # getting details of the nodes
# res$arcs <- res$arcs[-which((res$arcs[,'from'] == "coughing" & res$arcs[,'to'] == "chronic_disease")),]


#Finding Conditional Probability Tables (CPTs) at each nodes
fittedbn <- bn.fit(res, data = bn_df)
fittedbn$smoking$prob

print(fittedbn)


# cpquery(fittedbn, event = (anxiety == 1 ) ) # Failed neec to fix and try



# Params used in bnstruct : MAP (Maximmum A Posteriori) as estimate params <- learns the conditional probabilities entailed by a network, 



# source to read :
#   - https://www.bnlearn.com/about/teaching/slides-bnshort.pdf
#   - https://cran.r-project.org/web/packages/bnstruct/vignettes/bnstruct.pdf