library(tidyverse)

t<-jsalomon::perf_table()


score_function<-\(.,col){
  . |> 
    mutate({{col}}:=ifelse(is.na({{col}}),0,  {{col}} )) |> 
    arrange({{col}}) |> 
    mutate(
      rank=row_number(),
      rank=round(rank/max(rank)*100,0)
    ) |> 
    rename("RS_{{col}}":=rank)
}
 
RS_df<-t |> select(description, name, perf_1m, perf_3m, perf_6m, perf_ytd, perf_y) |>
  mutate(symbol_yf=str_replace(paste0(name,".ST"), "_","-"),
         stock=str_to_title(description)) |> 
  score_function(perf_1m) |>
  score_function(perf_3m) |>
  score_function(perf_6m) |>
  score_function(perf_ytd) |>
  score_function(perf_y) |>
  select(stock, symbol=name,symbol_yf , starts_with("RS_")) |> 
  arrange(-RS_perf_y) |> 
  mutate(date=Sys.time())

readr::write_csv(RS_df,file=paste0("C:/Users/johan/Documents/github/rs_se/data/rs_se.csv"))

library(git2r)

git2r::add(path = c("r/RS_script.R", "data/rs_se.csv"),#"C:/Users/johan/Documents/github/rs_se"
           )
git2r::commit(message = paste0("commit", Sys.time()))

git2r::push(getwd(),
  credentials = git2r::cred_user_pass( username = Sys.getenv("GH_Name"),
                                 password = Sys.getenv("GH_PASS") ),
  set_upstream = TRUE
  
  
)

# git2r::push(getwd(),
#             credentials = git2r::cred_env("GH_Name","GH_PASS") 
#             )      
#             
           



