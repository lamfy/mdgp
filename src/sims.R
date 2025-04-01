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
                 # gap=c(result_standard$gap, result_index$gap),
                 status=c(result_standard$status, result_index$status))
write.csv(df, paste0("../data/rel_standard_index_n", N, "_g", G, ".csv"))

# Standard vs. Big-M
A <- 10
N <- 10
G <- 2
result_standard <- get_sims(attributes=A, groups=G, items=N, name="standard", equal=FALSE)
df5 <- data.frame(model=c(rep("standard", 30)),
                 optval=c(result_standard$optval),
                 time=c(result_standard$time),
                 gap=c(result_standard$gap),
                 status=c(result_standard$status))
write.csv(df5, paste0("../data/STANDARDD_bigm_n", N, "_g", G, ".csv"))
result_bigm <- get_sims(attributes=A, groups=G, items=N, name="bigm", equal=FALSE)
df6 <- data.frame(model=c(rep("bigm", 30)),
                 optval=c(result_bigm$optval),
                 time=c(result_bigm$time),
                 gap=c(result_bigm$gap),
                 status=c(result_bigm$status))
write.csv(df6, paste0("../data/standard_BIGMM_n", N, "_g", G, ".csv"))
# write.csv(df, paste0("../data/standard_1bigm_n", N, "_g", G, ".csv"))

# Analysis

read.csv("../data/standard_index_n30_g10.csv") %>%
  select(-c(X)) %>%
  as_tibble() %>%
  group_by(model) %>%
  summarise(
    # Average Optimal Value
    optval=mean(optval[status=="OPTIMAL"|status=="TIME_LIMIT"], na.rm=TRUE),
    # Average Time
    time=mean(time, na.rm=TRUE),
    # Average Gap
    gap=mean(gap[status=="TIME_LIMIT"], na.rm=TRUE),
    # Number of Feasible Solutions
    nfea=sum(str_count(status, "OPTIMAL"), na.rm=TRUE)+sum(str_count(status, "TIME_LIMIT"), na.rm=TRUE),
    # Number of Optimal Solutions
    nopt=sum(str_count(status, "OPTIMAL"), na.rm=TRUE)) %>%
  View()


read.csv("../data/standard_index_n30_g10.csv") %>%
  select(-c(X)) %>%
  as_tibble() %>%
  select(c(model, optval)) %>%
  mutate(run=rep(1:30, 2)) %>%
  pivot_wider(names_from=model, values_from=optval) %>%
  mutate(Greater=round(index,4)>round(standard,4),
         Equal=round(index,4)==round(standard,4),
         Less=round(index,4)<round(standard,4)) %>%
  summarise(Greater=sum(Greater),
            Equal=sum(Equal),
            Less=sum(Less))

read.csv("../data/standard_bigm_n30_g10.csv") %>%
  select(-c(X)) %>%
  as_tibble() %>%
  select(c(model, optval)) %>%
  mutate(run=rep(1:30, 2)) %>%
  pivot_wider(names_from=model, values_from=optval) %>%
  mutate(Greater=round(bigm,4)>round(standard,4),
         Equal=round(bigm,4)==round(standard,4),
         Less=round(bigm,4)<round(standard,4)) %>%
  summarise(Greater=sum(Greater),
            Equal=sum(Equal),
            Less=sum(Less))
