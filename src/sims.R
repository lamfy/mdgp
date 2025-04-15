# Load Libraries
source("utils.R")

# Standard vs. Index
A <- 10
N <- 20
G <- 5
result_standard <- get_sims(attributes=A, groups=G, items=N, name="standard", equal=TRUE,
                            n_sim=8)
result_index <- get_sims(attributes=A, groups=G, items=N, name="index", equal=TRUE)
df <- data.frame(model=c(rep("standard", 30), rep("index", 30)),
                 optval=c(result_standard$optval, result_index$optval),
                 time=c(result_standard$time, result_index$time),
                 status=c(result_standard$status, result_index$status))

# Standard vs. Big-M
A <- 10
N <- 10
G <- 2
result_standard <- get_sims(attributes=A, groups=G, items=N, name="standard", equal=FALSE)
df <- data.frame(model=c(rep("standard", 30)),
                 optval=c(result_standard$optval),
                 time=c(result_standard$time),
                 gap=c(result_standard$gap),
                 status=c(result_standard$status))
result_bigm <- get_sims(attributes=A, groups=G, items=N, name="bigm", equal=FALSE)
df <- data.frame(model=c(rep("bigm", 30)),
                 optval=c(result_bigm$optval),
                 time=c(result_bigm$time),
                 gap=c(result_bigm$gap),
                 status=c(result_bigm$status))
