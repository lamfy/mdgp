# Load Libraries
source("utils.R")

# Standard vs. Index
A <- 10
N <- 10
G <- 2
result_standard <- get_sims(attributes=A, groups=G, items=N, name="standard", equal=TRUE)
result_index <- get_sims(attributes=A, groups=G, items=N, name="index", equal=TRUE)
df <- data.frame(model=c(rep("standard", 30), rep("index", 30)),
                 optval=c(result_standard$optval, result_index$optval),
                 time=c(result_standard$time, result_index$time),
                 gap=c(result_standard$gap, result_index$gap),
                 status=c(result_standard$status, result_index$status))
# write.csv(df, paste0("../data/standard_index_n", N, "_g", G, ".csv"))

# Standard vs. Big-M
A <- 10
N <- 10
G <- 2
result_standard <- get_sims(attributes=A, groups=G, items=N, name="standard", equal=FALSE)
result_bigm <- get_sims(attributes=A, groups=G, items=N, name="bigm", equal=FALSE,
                        TimeLimit=2)
df <- data.frame(model=c(rep("standard", 30), rep("bigm", 30)),
                 optval=c(result_standard$optval, result_bigm$optval),
                 time=c(result_standard$time, result_bigm$time),
                 gap=c(result_standard$gap, result_bigm$gap),
                 status=c(result_standard$status, result_bigm$status))
# write.csv(df, paste0("../data/standard_bigm_n", N, "_g", G, ".csv"))

# Analysis
df %>%
  as_tibble() %>%
  group_by(model) %>%
  summarise(
    # Average Optimal Value
    optval=mean(optval, na.rm=TRUE),
    # Average Time
    time=mean(time, na.rm=TRUE),
    # Average Gap
    gap=mean(gap, na.rm=TRUE),
    # Number of Feasible Solutions
    nfea=sum(str_count(status, "OPTIMAL"), na.rm=TRUE)+sum(str_count(status, "TIME_LIMIT"), na.rm=TRUE),
    # Number of Optimal Solutions
    nopt=sum(str_count(status, "OPTIMAL"), na.rm=TRUE))
