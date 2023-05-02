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

git2r::add(path = c("r/RS_script.R", "data/rs_se.csv"),#"C:/Users/johan/Documents/github/rs_se"
           )
git2r::commit(message = paste0("commit", Sys.time()))
git2r::push(getwd(),
  credentials = git2r::cred_user_pass( username = "JohanSalomonssonSV",
                                 password = Sys.getenv("GH_PASS")  
  )      
  
  
)

repo <- git2r::repository("https://github.com/JohanSalomonssonSV/rs_se.git")

git2r::config( user.name="JohanSalomonssonSV", user.email="johan.salomonsson88@gmail.com")



# library(git2r)
# path_bare <- tempfile(pattern="git2r-")
# path_repo <- tempfile(pattern="git2r-")
# dir.create(path_bare)
# dir.create(path_repo)
# repo_bare <- init(path_bare, bare = TRUE)

#git2r::cred
#git2r::push(path = "C:/Users/johan/Documents/github/rs_se")

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

