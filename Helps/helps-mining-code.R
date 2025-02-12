#helps

library(reshape)
mdata1.melted <- melt(mdata1.split, id="uniqueID",na.rm=T) #alternative code, works fine, not used

separate(data = df, col = FOO, into = c("left", "right"), sep = "\\|")


mdata1.melted.split <- cSplit(mdata1.melted, "SWOT.Cats", "\\s+-\\s+")%>%
  select("uniqueID", starts_with("SWOT_cats_"))

mdata1.melted.split <- mdata1.melted %>%
  bind_cols(split_into_multiple(mdata1.melted$SWOT.Cats, "\\s+-\\s+", "SWOT_cats")) %>%
  select("uniqueID", starts_with("SWOT_cats_"))

#mdata1 <- mdata1 %>% rename(mdata1, SWOT.Cats.1 = SWOT.Cats, SWOT.Cats.2 = SWOT.Cats.text)#edit vars names


#' remove quotation marks "" from text in var SWOT_categories
mdata1$SWOT_categoriess <- gsub("\"", "", mdata1$SWOT_categoriess)
head(mdata1)

mdata1$SWOT_categories

library(textclean)
z <- '\x95He said, \x93Gross, I am going to!\x94'
Encoding(z) <- "latin1"
z
replace_curly_quote(z)
replace_non_ascii(z)
z

"\\\"\\\""
raw <- gsub("\\\"\\\"", "\"", raw)

s
#"who are í ½í¸€ bringing?"
s2 <- iconv(s, "UTF-8", "ASCII", sub = "")
s2
#"who are   bringing?"