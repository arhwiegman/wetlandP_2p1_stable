# **************************************************************
# filename: generate_documentation_tables.R
# description: Takes the parameters R script and generates tables
# author: Adrian Wiegman
# revision date:  2020-05-07
# project: 
version = "wetlandPv04"
# repository: https://github.com/arhwiegman/___    
# notes:
# - need to add more types of variables to describe parameters
# **************************************************************

fn_generate_doc_table <- function(
  fconn="scripts/parameters.R",
  fname="parameters",
  patt = "^[^#]+=.+#.+\\|.+\\|.*$", # string pattern
  startline = 0,
  endline = -1L
){
  require(tidyverse)
  . <- read_lines(file=fconn,
                  skip=startline,
                  skip_empty_rows = TRUE,
                  n_max = (endline - startline))
  . <- str_squish(.) 
  strings <- .[str_which(.,patt)]
  M1 <- str_split(strings,"[\\|]",simplify = TRUE)
  M2 <- str_split(M1[,1],"=",simplify = TRUE)
  head(M2)
  M3 <- str_split(M2[,2],"#",simplify = TRUE)
  head(M3)
  
  Name <- M2[,1] %>% str_trim()
  Value <- M3[,1] %>% str_trim() 
  Unit <- M3[,2] %>% str_trim() 
  Description <- M1[,2] %>% str_trim() 
  Assumptions <- M1[,3] %>% str_trim() 
  
  df <- data.frame(Name,Value,Unit,Description,Assumptions)
  cn <- c("name","expression","unit","description","references & assumptions")
  cap <- paste(fname,format(Sys.time(), "%Y-%m-%d_%h%m%s"))
  k <- knitr::kable(df,caption=cap)
  write(k,paste0("documentation/",fname,".md"))
  write_csv(df,paste0("documentation/",fname,".csv"))
}
fn_generate_doc_table()
