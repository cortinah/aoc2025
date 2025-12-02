library(adventdrob)
library(tidyverse)
input <- advent_input(day=1, year=2025)

# Part 1
input |> separate_wider_position(cols = x, widths = c(d=1,value=3), too_few = "align_start") -> input
input$value <- as.numeric(input$value)
input$combo <- NA
solution <- tibble(d='', value=0L, combo=50)
solution <- rbind(solution, input)
solution |> mutate(value = ifelse(d=="R", value, -value)) -> solution

for (i in 2:nrow(solution))  solution[i,"combo"] <- ((solution[i-1,"combo"] + solution[i,"value"]) %% 100)  
solution |> filter(combo==0) |> nrow() -> p1

# Part 2
for (i in 2:nrow(solution))  {solution[i,"cumcombo"] <- solution[i-1,"combo"] + solution[i,"value"]
                              solution[i,"passes"] <- abs(solution[i,"cumcombo"] - solution[i,"combo"] )/100}

solution |> mutate(fix=ifelse(combo==0, pmax(0, (passes-1)), passes)) -> solution

solution$fix2 <- NA
for (i in 3:nrow(solution))  { solution[i, "fix2"] = ifelse( (solution[i,"fix"]>=1) & (solution[i-1,"combo"]==0) & (solution[i,"value"]<0) , solution[i,"fix"]-1 , solution[i,"fix"])  }



solution |> summarize(sum(fix2, na.rm=T)) |> pull() -> p2
p1 + p2

#6530 too high
#5570 too low