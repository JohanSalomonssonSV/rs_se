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
    #deparse({{col}})
    rename("RS_{{col}}":=rank)
  #rename(paste0(deparse(substitute({{col}})),"_rank"):=rank   )
  #rename_with(~paste0({{col}}, "rank"), rank)
  #rename(paste0("{{col}}","_rank"):="rank")
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
  mutate(date=today())

readr::write_csv(RS_df,file=paste0("C:/Users/johan/Documents/github/rs_se/data/rs_se.csv"))


#library(git2rdata)

# root <- "~/rs_se" 
# write_vc(my_data, file = "rel_path/filename", root = root)
# read_vc(file = "rel_path/filename", root = root)
# root <- git2r::repository("~/my_git_repo") # git repository

# Using a git repository
# library(git2rdata)
# 
# repo <- repository("https://github.com/JohanSalomonssonSV/rs_se.git")
# pull(repo)
# write_vc(my_data, file = "rel_path/filename", root = repo, stage = TRUE)
# commit(repo, "My message")
# push(repo)
# read_vc(file = "rel_path/filename", root = repo)

# repo <- repository("~/my_git_repo")
# pull(repo)
# write_vc(my_data, file = "rel_path/filename", root = repo, stage = TRUE)
# commit(repo, "My message")
# push(repo)
# read_vc(file = "rel_path/filename", root = repo)

