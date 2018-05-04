## Preprocessing
data <- read.csv("input/data.csv", stringsAsFactors = FALSE, sep = ";")
data <- data %>% mutate(CUEFILE = ifelse(CUEFILE == '010.bmp','new',
                                         ifelse(CUEFILE == '050.bmp', 'neutral',
                                                ifelse(CUEFILE == '090.bmp', 'old', NA))))
data <- data %>% mutate(ALL_RT = ALL_RT / 1000)
data <- data %>% mutate(ACCURACY = ifelse(LAB_RESP=='MISS'|LAB_RESP=='FA',0,
                                          ifelse(LAB_RESP=='HIT'|LAB_RESP=='CR',1,NA))
)
data <- data %>% mutate(RESPONSE = ifelse(LAB_RESP=='MISS'|LAB_RESP=='CR','new',
                                          ifelse(LAB_RESP=='HIT'|LAB_RESP=='FA','old',NA))
)
data <- data %>% mutate(RESPONSE_NUM = ifelse(RESPONSE=='old',0,
                                              ifelse(RESPONSE=='new',1,NA))
)
data$CUEFILE <- as.factor(data$CUEFILE)
data$CUETYPE <- as.factor(data$CUETYPE)
data$CUEFILE <- relevel(data$CUEFILE, 'new')
data <- data[complete.cases(data$CUETYPE), ]
data <- data[complete.cases(data$ALL_RT), ]
data <- data[complete.cases(data$CUEFILE), ]
data <- data[complete.cases(data$RESPONSE), ]
data <- data[complete.cases(data$SUBJECT), ]

require(rtdists)
require(dplyr)
require(tidyr)
require(purrr)
require(lattice)
require(latticeExtra)
lattice.options(default.theme = standard.theme(color = FALSE))
lattice.options(default.args = list(as.table = TRUE))
options(digits = 3)
require(binom)

data(data)
data <- data[!data$outlier,]

# aggregate data for first plot:
agg_data <- data %>% group_by(SUBJECT, CUEFILE, CUETYPE) %>%
  summarise(prop = mean(RESPONSE == "new", na.rm = TRUE), mean_rt = mean(ALL_RT), median_rt = mean(ALL_RT)) %>%
  ungroup()

xyplot(prop ~ CUEFILE|SUBJECT, agg_data, group = CUETYPE, type = "b",
       auto.key = list(lines = TRUE), ylab = "Proportion of 'new' responses")

# aggregate data for second plot:
agg2_data <- data %>% group_by(SUBJECT, CUEFILE, CUETYPE) %>%
  summarise(prop = mean(RESPONSE == "old", na.rm = TRUE), mean_rt = mean(ALL_RT), median_rt = mean(ALL_RT)) %>%
  ungroup()

xyplot(prop ~ CUEFILE|SUBJECT, agg2_data, group = CUETYPE, type = "b",
       auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses")

# overview of response time distributions for congruent condition
quantiles <- c(0.1, 0.3, 0.5, 0.7, 0.9)
quantiles_data <- data[data$SUBJECT < 5,] %>%
  group_by(SUBJECT, CUEFILE, CUETYPE) %>%
  nest() %>%
  mutate(quantiles = map(data, ~ as.data.frame(t(quantile(.x$ALL_RT, probs = quantiles))))) %>%
  unnest(quantiles) %>%
  gather("quantile", "rt",`10%`:`90%`) %>%
  arrange(SUBJECT, CUEFILE, CUETYPE)

xyplot(rt ~ CUEFILE|SUBJECT + CUETYPE, quantiles_data, group = quantile, type = "b", auto.key = list(lines = TRUE), ylab = "RT (in seconds)", subset = CUETYPE == "CONGRUENT")

# overview of response time distributions for incongruent condition
xyplot(rt ~ CUEFILE|SUBJECT + CUETYPE, quantiles_data, group = quantile, type = "b", auto.key = TRUE, ylab = "RT (in seconds)", subset = CUETYPE == "INCONGRUENT")

# aggregate data for response probability plot
require(binom)
aggr_data_bin <- data[data$SUBJECT < 5,] %>%
  group_by(SUBJECT, CUEFILE, CUETYPE) %>%
  summarise(n = n(),
            old = sum(RESPONSE == "old"),
            new = sum(RESPONSE == "new")) %>%
  ungroup() %>%
  mutate(prop = map2(!is.na(old), n, ~ binom.confint(.x, .y, methods = "agresti-coull"))) %>%
  unnest(prop)

knitr::kable(
  data[data$SUBJECT < 5,] %>% group_by(SUBJECT, CUEFILE, CUETYPE, RESPONSE) %>%
    summarise(n = n()) %>%
    spread(CUEFILE, n)
)

# response probability plot
xyplot(mean ~ CUEFILE|SUBJECT, aggr_data_bin, group = CUETYPE, type = "b", auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses")

# aggregate data for quantile plot
quantiles_data_bin <- data[data$SUBJECT < 5,] %>%
  group_by(SUBJECT, CUEFILE, CUETYPE) %>%
  nest() %>%
  mutate(quantiles = map(data, ~ as.data.frame(t(quantile(.x$ALL_RT, probs = quantiles))))) %>%
  unnest(quantiles) %>%
  gather("quantile", "rt", `10%`:`90%`) %>%
  arrange(SUBJECT, CUEFILE, CUETYPE)

xyplot(rt ~ CUEFILE|SUBJECT + CUETYPE, quantiles_data_bin, group = quantile, type = "b", auto.key = list(lines = TRUE), ylab = "RT (in seconds)", subset = CUETYPE == "CONGRUENT")

# quantile plot incongruent condition
xyplot(rt ~ CUEFILE|SUBJECT + CUETYPE, quantiles_data_bin, group = quantile, type = "b", auto.key = list(lines = TRUE), ylab = "RT (in seconds)", subset = CUETYPE == "INCONGRUENT")

# aggregate data for response plot (congruent condition)
agg2_data_response <- data[data$SUBJECT < 5,] %>%
  group_by(SUBJECT, CUEFILE, CUETYPE, RESPONSE) %>%
  nest() %>%
  mutate(quantiles = map(data, ~ as.data.frame(t(quantile(.x$ALL_RT, probs = quantiles))))) %>%
  unnest(quantiles) %>%
  gather("quantile", "rt", `10%`:`90%`) %>%
  arrange(SUBJECT, CUEFILE, RESPONSE, CUETYPE)

p1 <- xyplot(rt ~ CUEFILE|SUBJECT, agg2_data_response, group = quantile, type = "b", auto.key = list(lines = TRUE), ylab = "RT (in seconds)", subset = CUETYPE == "CONGRUENT" & RESPONSE == "old", layout = c(3,1))
p2 <- xyplot(rt ~ CUEFILE|SUBJECT, agg2_data_response, group = quantile, type = "b", auto.key = list(lines = TRUE), ylab = "RT (in seconds)", subset = CUETYPE == "CONGRUENT" & RESPONSE == "new", col = "grey")
p1 + as.layer(p2)

# aggregate data for response plot (incongruent condition)
p1 <- xyplot(rt ~ CUEFILE|SUBJECT, agg2_data_response, group = quantile, type = "b", auto.key = list(lines = TRUE), ylab = "RT (in seconds)", subset = CUETYPE == "INCONGRUENT" & RESPONSE == "old", layout = c(3,1))
p2 <- xyplot(rt ~ CUEFILE|SUBJECT, agg2_data_response, group = quantile, type = "b", auto.key = list(lines = TRUE), ylab = "RT (in seconds)", subset = CUETYPE == "INCONGRUENT" & RESPONSE == "new", col = "grey")
p1 + as.layer(p2)

## diffusion model analysis
d_nested <- data[data$SUBJECT < 5,] %>%
  group_by(SUBJECT, CUETYPE) %>%
  nest()
d_nested

# objective function for diffusion with 1 a. Loops over drift to assign drift rates to strength
objective_diffusion_separate <- function(pars, rt, response, drift, ...) {
  non_v_pars <- grep("^v", names(pars), invert = TRUE, value = TRUE)
  base_par <- length(non_v_pars)
  densities <- vector("numeric", length(rt))
  for (i in seq_along(levels(drift))) {
    densities[drift == levels(drift)[i]] <-
      ddiffusion(rt[drift == levels(drift)[i]], response=response[drift == levels(drift)[i]],
                 a=pars["a"], t0=pars["t0"],
                 sv=pars["sv"],
                 sz=if ("sz" %in% non_v_pars) pars["sz"] else 0.1,
                 z=if ("z" %in% non_v_pars) pars["z"]*pars["a"] else 0.5*pars["a"],
                 st0=if ("st0" %in% non_v_pars) pars["st0"] else 0,
                 v=pars[base_par+i])
  }
  if (any(densities == 0)) return(1e6)
  return(-sum(log(densities)))
}

# function that creates random start values
get_start <- function(base_par, n_drift = 3) {
  start1 <- c(
    a = runif(1, 0.5, 3),
    a_1 = runif(1, 0.5, 3),
    a_2 = runif(1, 0.5, 3),
    t0 = runif(1, 0, 0.5),
    z = runif(1, 0.4, 0.6),
    sz = runif(1, 0, 0.5),
    sv = runif(1, 0, 0.5),
    st0 = runif(1, 0, 0.5),
    d = rnorm(1, 0, 0.05)
  )
  start2 <- sort(rnorm(n_drift), decreasing = FALSE)
  names(start2) <- paste0("v_", seq_len(n_drift))
  c(start1[base_par], start2)
}

# function that tries different random start values until it works
ensure_fit <-
  function(data, start_function, objective_function,
           base_pars, n_drift = 3, n_fits = 1,
           lower = c(rep(0, length(base_pars)), -Inf,
                     rep(-Inf,length(start_function(base_pars))-length(base_pars)))) {
    best_fit <- list(objective = 1e+06)
    for (i in seq_len(n_fits)) {
      start_ll <- 1e+06
      while(start_ll == 1e+06) {
        start <- start_function(base_pars, n_drift=n_drift)
        start_ll <- objective_function(start,
                                       rt = data$ALL_RT, response = data$RESPONSE, drift = factor(data$CUEFILE, seq_len(n_drift)),
                                       cuetype = data$CUETYPE)
      }
      cat("\nstart fitting.\n")
      
      fit <- nlminb(start, objective_function,
                    rt = data$ALL_RT, response = data$RESPONSE,
                    drift = factor(data$CUEFILE, seq_len(n_drift)),
                    cuetype = data$CUETYPE,
                    lower = lower)
      
      if (fit$objective < best_fit$objective) best_fit <- fit
    }
    out <- as.data.frame(t(unlist(best_fit[1:3])))
    colnames(out) <- sub("par.", "", colnames(out))
    out
  }

# obtain fit
fit_diffusion <- d_nested %>%
  mutate(fit =
           map(data,
               ~ensure_fit(data = ., start_function = get_start,
                           objective_function = objective_diffusion_separate,
                           base_pars = c("a", "t0", "sv", "sz", "z")))) %>%
  unnest(fit)