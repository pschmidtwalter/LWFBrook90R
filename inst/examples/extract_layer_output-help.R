# create a data.frame with monthly values
# identifiers: layer number, yr and mo
df <- expand.grid(nl = 1:5,
                  yr = 2002:2003,
                  mo = 1:12)
df
#add a value variable
df$var <- runif(nrow(df), -1,0)

extract_layer_output(df)

# add more variables
df$var1 <- runif(nrow(df), 1,2)
df$var2 <- runif(nrow(df), 2,3)

# extract specific layers
extract_layer_output(df,layers = 2:4, sep = "_")

#extract specific variables
extract_layer_output(df, layers = 2:4, value.vars = c("var1", "var2"), sep = "_")
