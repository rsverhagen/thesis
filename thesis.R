---
  title: "Thesis"
output: html_document
---
  ------------------------------------------------------------

require(rtdists)
require(dplyr)   # for data manipulations and looping
require(tidyr)   # for data manipulations
require(purrr)   # for data manipulations
require(lattice) # for plotting and corresponding themes
require(latticeExtra)
lattice.options(default.theme = standard.theme(color = FALSE))
lattice.options(default.args = list(as.table = TRUE))
options(digits = 3) # only three decimal digits
require(binom)  # for binomial confidence intervals

## Preprocessing
data <- read.csv("input/data.csv", stringsAsFactors = FALSE, sep = ";")
data <- data %>% mutate(CUEFILE = ifelse(CUEFILE == '010.bmp',25,
                                         ifelse(CUEFILE == '050.bmp', 50,
                                                ifelse(CUEFILE == '090.bmp', 75, NA))))
data <- data %>% mutate(ALL_RT = ALL_RT / 1000)
data <- data %>% mutate(ACCURACY = ifelse(LAB_RESP=='MISS'|LAB_RESP=='FA',0,
                                          ifelse(LAB_RESP=='HIT'|LAB_RESP=='CR',1,NA))
)
data <- data %>% mutate(RESPONSE = ifelse(LAB_RESP=='MISS'|LAB_RESP=='CR','new',
                                          ifelse(LAB_RESP=='HIT'|LAB_RESP=='FA','old',NA))
)
data <- data %>% mutate(RESPONSE_NUM = ifelse(RESPONSE=='old',2,
                                              ifelse(RESPONSE=='new',1,NA))
)
data$CUETYPE <- as.factor(data$CUETYPE)
data <- data[complete.cases(data$CUETYPE), ]
data <- data[complete.cases(data$ALL_RT), ]
data <- data[complete.cases(data$CUEFILE), ]
data <- data[complete.cases(data$RESPONSE), ]
data <- data[complete.cases(data$SUBJECT), ]

data(data)

# aggregate data for first plot:
agg_data <- data  %>% group_by(SUBJECT, CUETYPE, CUEFILE) %>% 
  summarise(prop = mean(RESPONSE == "old"), mean_rt = mean(ALL_RT), median_rt = mean(ALL_RT)) %>% 
  ungroup()

xyplot(prop ~ CUEFILE|SUBJECT, agg_data, group = CUETYPE, type = "b", 
       auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses")



quantiles <- c(0.1, 0.3, 0.5, 0.7, 0.9)
## aggregate data for quantile plot
quantiles_data <- data  %>% 
  group_by(SUBJECT, CUETYPE, CUEFILE) %>% 
  nest() %>% 
  mutate(quantiles = map(data, ~ as.data.frame(t(quantile(.x$ALL_RT, probs = quantiles))))) %>% 
  unnest(quantiles) %>% 
  gather("quantile", "rt",`10%`:`90%`) %>% 
  arrange(SUBJECT, CUETYPE, CUEFILE)

xyplot(rt ~ CUEFILE|SUBJECT + CUETYPE, quantiles_data, group = quantile, type = "b", 
       auto.key = list(lines = TRUE), ylab = "RT (in seconds)", subset = CUETYPE == "CONGRUENT")



xyplot(rt ~ strength|id + instruction, quantiles_rr98, group = quantile, type = "b", 
       auto.key = FALSE, ylab = "RT (in seconds)", subset = instruction == "accuracy")



#bins <- c(-0.5, 5.5, 10.5, 13.5, 16.5, 19.5, 25.5, 32.5) # seven bins like RR98
bins <- c(25,50,75)
data$cue_bin <- cut(data$CUEFILE, breaks = bins, include.lowest = TRUE)
levels(data$cue_bin) <- as.character(1:3)

# aggregate data for response probability plot:
agg_data_bin <- data %>% 
  group_by(SUBJECT, CUETYPE, cue_bin) %>%
  summarise(n = n(), 
            old = sum(RESPONSE == "old"),
            new = sum(RESPONSE == "new")) %>%
  ungroup() %>%
  mutate(prop = map2(old, n, ~ binom.confint(.x, .y, methods = "agresti-coull"))) %>% 
  unnest(prop)


knitr::kable(
  data %>% group_by(SUBJECT, CUETYPE, cue_bin, RESPONSE) %>%
    summarise(n = n()) %>%
    spread(cue_bin, n)
)



xyplot(mean ~ cue_bin|SUBJECT, agg_data_bin, group = CUETYPE, type = "b", 
       auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses")



## aggregate data for quantile plot
quantiles_data_bin <- data  %>% 
  group_by(SUBJECT, CUETYPE, cue_bin) %>% 
  nest() %>% 
  mutate(quantiles = map(data, ~ as.data.frame(t(quantile(.x$ALL_RT, probs = quantiles))))) %>% 
  unnest(quantiles) %>% 
  gather("quantile", "rt",`10%`:`90%`) %>% 
  arrange(SUBJECT, CUETYPE, cue_bin)

xyplot(rt ~ cue_bin|SUBJECT + CUETYPE, quantiles_data_bin, group = quantile, type = "b", 
       auto.key = list(lines = TRUE), ylab = "RT (in seconds)", subset = CUETYPE == "CONGRUENT")



xyplot(rt ~ cue_bin|SUBJECT + CUETYPE, quantiles_data_bin, group = quantile, type = "b", 
       auto.key = FALSE, ylab = "RT (in seconds)", subset = CUETYPE == "INCONGRUENT")



agg2_data_response <- data  %>% 
  group_by(SUBJECT, CUETYPE, cue_bin, RESPONSE) %>% 
  nest() %>% 
  mutate(quantiles = map(data, ~ as.data.frame(t(quantile(.x$ALL_RT, probs = quantiles))))) %>% 
  unnest(quantiles) %>% 
  gather("quantile", "rt",`10%`:`90%`) %>% 
  arrange(SUBJECT, CUETYPE, RESPONSE, cue_bin)

p1 <- xyplot(rt ~ cue_bin|SUBJECT, agg2_data_response, group = quantile, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT" & RESPONSE == "old", layout = c(30,2))
p2 <- xyplot(rt ~ cue_bin|SUBJECT, agg2_data_response, group = quantile, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT" & RESPONSE == "new", col = "grey")
p1 + as.layer(p2)



p1 <- xyplot(rt ~ cue_bin|SUBJECT, agg2_data_response, group = quantile, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT" & RESPONSE == "old", layout = c(30,2))
p2 <- xyplot(rt ~ cue_bin|SUBJECT, agg2_data_response, group = quantile, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT" & RESPONSE == "new", col = "grey")
p1 + as.layer(p2)



d_nested <- data %>% 
  group_by(SUBJECT, CUETYPE) %>% # we loop across both, subject and cuetype
  nest()
d_nested



# objective function for diffusion with 1 a. loops over drift to assign drift rates to strength
objective_diffusion_separate <- function(pars, rt, response, drift, ...) {
  non_v_pars <- grep("^v", names(pars), invert = TRUE, value = TRUE)
  base_par <- length(non_v_pars)  # number of non-drift parameters
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



# function that creates random start values, also 
get_start <- function(base_par, n_drift = 2) {
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

# function that tries different random start values until it works:
ensure_fit <- 
  function(data, start_function, objective_function, 
           base_pars, n_drift = 2, n_fits = 10, 
           lower = c(rep(0, length(base_pars)), -Inf,
                     rep(-Inf,length(start_function(base_pars))-length(base_pars)))) {
    best_fit <- list(objective = 1e+06)
    for (i in seq_len(n_fits)) {
      start_ll <- 1e+06
      #browser()
      while(start_ll == 1e+06) {
        start <- start_function(base_pars, n_drift=n_drift)
        start_ll <- objective_function(start, 
                                       rt = data$ALL_RT, response = data$RESPONSE_NUM, 
                                       drift = factor(data$cue_bin, seq_len(n_drift)), 
                                       instruction = data$CUETYPE)
      }
      cat("\nstart fitting.\n") # just for information to see if it is stuck
      
      fit <- nlminb(start, objective_function, 
                    rt = data$ALL_RT, response = data$RESPONSE_NUM, 
                    drift = factor(data$cue_bin, seq_len(n_drift)), 
                    instruction = data$CUETYPE,
                    lower = lower)
      
      if (fit$objective < best_fit$objective) best_fit <- fit
    }
    out <- as.data.frame(t(unlist(best_fit[1:3])))
    colnames(out) <- sub("par.", "", colnames(out))
    out
  }



fit_diffusion <- d_nested %>% 
  mutate(fit = 
           map(data, 
               ~ensure_fit(data = ., start_function = get_start, 
                           objective_function = objective_diffusion_separate, 
                           base_pars = c("a", "t0", "sv", "sz", "z")))) %>% 
  unnest(fit)



fit_diffusion$data <- NULL
if (!("st0" %in% colnames(fit_diffusion))) fit_diffusion$st0 <- 0
if (!("z" %in% colnames(fit_diffusion))) fit_diffusion$z <- 0.5
if (!("sz" %in% colnames(fit_diffusion))) fit_diffusion$sz <- 0.1
knitr::kable(fit_diffusion)
pars <- as.data.frame(fit_diffusion)



# get predicted response proportions
pars_separate_l <- fit_diffusion %>% gather("cue_bin", "v", starts_with("v"))
pars_separate_l$cue_bin <- factor(substr(pars_separate_l$cue_bin, 3,3), 
                                  levels = as.character(seq_len(length(bins)-1)))
#pars_separate_l <- inner_join(pars_separate_l, agg_rr98_bin)
pars_separate_l <- pars_separate_l  %>% group_by(SUBJECT, CUETYPE, cue_bin) %>%
  mutate(resp_prop = pdiffusion(rt=Inf, response="upper", 
                                a=a, v=v, t0=t0, sz = sz, z=a*z, sv=sv, st0=st0))

p1 <- xyplot(mean ~ cue_bin|SUBJECT + CUETYPE, agg_data_bin, type = "b", auto.key = 
               list(lines = TRUE), ylab = "Proportion of 'old' responses", col = "grey")
p2 <- segplot(cue_bin ~ upper+lower|SUBJECT + CUETYPE, agg_data_bin, 
              auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
              col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
              draw.bands = FALSE, angle = 90, length = 0.05, ends = "both")
p3 <- xyplot(resp_prop ~ cue_bin|SUBJECT + CUETYPE, pars_separate_l, type = "b", 
             auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
             col = "black")
p2 + as.layer(p1) + as.layer(p3)



# get predicted quantiles (uses predicted response proportions)
separate_pred_old <- pars_separate_l %>% do(as.data.frame(t(
  qdiffusion(quantiles*.$resp_prop, response="upper", 
             a=.$a, v=.$v, t0=.$t0, sz = .$sz, z = .$z*.$a, sv=.$sv, st0=.$st0)))) %>% 
  ungroup() %>% gather("quantiles", "old", V1:V5)
separate_pred_new <- pars_separate_l %>% do(as.data.frame(t(
  qdiffusion(quantiles*(1-.$resp_prop), response="lower", 
             a=.$a, v=.$v, t0=.$t0, sz = .$sz, z = .$z*.$a, sv=.$sv, st0=.$st0)))) %>% 
  ungroup() %>% gather("quantiles", "new", V1:V5)

#separate_pred_light %>% filter(is.na(light))
separate_pred <- inner_join(separate_pred_old, separate_pred_new)
separate_pred$quantiles <- factor(separate_pred$quantiles, 
                                  levels = c("V5", "V4", "V3", "V2", "V1"), 
                                  labels = c("90%", "70%", "50%", "30%", "10%"))
separate_pred <- separate_pred %>% gather("response", "rt", old, new)

# get SE for observed quantiles
agg2_data_response_se <- data  %>% group_by(SUBJECT, CUETYPE, cue_bin, RESPONSE) %>% 
  summarise(se_median = sqrt(pi/2)*(sd(ALL_RT)/sqrt(n()))) %>%
  ungroup()

# calculate error bars for quantiles.
agg2_data_response <- left_join(agg2_data_response, agg2_data_response_se)
agg2_data_response <- agg2_data_response %>%
  mutate(lower = rt-se_median, upper = rt+se_median)


p1 <- xyplot(rt ~ cue_bin|SUBJECT+RESPONSE, agg2_data_response, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT" & quantile == "50%", 
             layout = c(30,3), col = "grey")
p1e <- segplot(cue_bin ~ upper+lower|SUBJECT+RESPONSE, agg2_data_response, 
               auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
               col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
               draw.bands = FALSE, angle = 90, length = 0.05, ends = "both", 
               subset = CUETYPE == "CONGRUENT" & quantile == "50%", layout = c(3,2))
p2 <- xyplot(rt ~ cue_bin|SUBJECT + response, separate_pred, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT" & quantiles == "50%", 
             scales = list(y = list(limits = c(0.4, 1.5))))
p2 + as.layer(p1) + as.layer(p1e)



p1 <- xyplot(rt ~ cue_bin|SUBJECT+RESPONSE, agg2_data_response, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT" & quantile == "50%", 
             layout = c(30,3), col = "grey")
p1e <- segplot(cue_bin ~ upper+lower|SUBJECT+RESPONSE, agg2_data_response, 
               auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
               col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
               draw.bands = FALSE, angle = 90, length = 0.05, ends = "both", 
               subset = CUETYPE == "INCONGRUENT" & quantile == "50%", layout = c(3,2))
p2 <- xyplot(rt ~ cue_bin|SUBJECT + response, separate_pred, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT" & quantiles == "50%", 
             scales = list(y = list(limits = c(0.4, 1.5))))
p2 + as.layer(p1) + as.layer(p1e)



p1 <- xyplot(rt ~ cue_bin|SUBJECT+RESPONSE, agg2_data_response, group = quantile, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT", 
             layout = c(30,3), col = "grey")
p1e <- segplot(cue_bin ~ upper+lower|SUBJECT+RESPONSE, agg2_data_response, 
               auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
               col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
               draw.bands = FALSE, angle = 90, length = 0.05, ends = "both", 
               subset = CUETYPE == "CONGRUENT")
p2 <- xyplot(rt ~ cue_bin|SUBJECT + response, separate_pred, group = quantiles, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT", 
             scales = list(y = list(limits = c(0.1, 3.0))))
p2 + as.layer(p1) + as.layer(p1e)



p1 <- xyplot(rt ~ cue_bin|SUBJECT+RESPONSE, agg2_data_response, group = quantile, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT", 
             layout = c(30,3), col = "grey")
p1e <- segplot(cue_bin ~ upper+lower|SUBJECT+RESPONSE, agg2_data_response, 
               auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
               col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
               draw.bands = FALSE, angle = 90, length = 0.05, ends = "both", 
               subset = CUETYPE == "INCONGRUENT")
p2 <- xyplot(rt ~ cue_bin|SUBJECT + response, separate_pred, group = quantiles, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT", 
             scales = list(y = list(limits = c(0.1, 3.0))))
p2 + as.layer(p1) + as.layer(p1e)


#LBA analysis


# objective function for diffusion with 1 a. loops over drift to assign drift rates to strength
objective_lba_separate <- function(pars, rt, response, drift, ...) {
  non_v_pars <- grep("^v", names(pars), invert = TRUE, value = TRUE)
  base_par <- length(non_v_pars)  # number of non-drift parameters
  densities <- vector("numeric", length(rt))
  for (i in seq_along(levels(drift))) {
    if (sum(drift == levels(drift)[i]) == 0) next
    densities[drift == levels(drift)[i]] <- dLBA(
      rt[drift == levels(drift)[i]], 
      response=response[drift == levels(drift)[i]],
      A = list(pars["a_1"], pars["a_2"]), 
      b = max(pars["a_1"], pars["a_2"])+pars["b"], 
      t0 = pars["t0"], 
      mean_v = c(pars[i], 1-pars[i]), 
      sd_v = pars["sv"], silent=TRUE)
  }
  if (any(densities == 0)) return(1e6)
  return(-sum(log(densities)))
}

# function that creates random start values
get_start_lba <- function(base_par, n_drift = 10) {
  start1 <- c(
    a = runif(1, 0.5, 3),
    a_1 = runif(1, 0.5, 3), 
    a_2 = runif(1, 0.5, 3),
    t0 = runif(1, 0, 0.5), 
    b = runif(1, 0, 0.5), 
    sv = runif(1, 0.5, 1.5),
    st0 = runif(1, 0, 0.5)
  )
  start2 <- sort(rnorm(n_drift), decreasing = FALSE)
  names(start2) <- paste0("v_", seq_len(n_drift))
  c(start2, start1[base_par])
}



fit_lba <- d_nested %>% 
  mutate(fit = 
           map(data, 
               ~ensure_fit(data = ., start_function = get_start_lba, 
                           objective_function = objective_lba_separate, 
                           base_pars = c("a_1", "a_2", "t0", "b", "sv"),
                           lower = c(rep(-Inf, 5), rep(0, 5)),
                           n_drift = 2, n_fits = 10))) %>% 
  unnest(fit)



knitr::kable(fit_lba)



pars_lba <- as.data.frame(fit_lba)



# get predicted response proportions
lba_pars_separate_l <- fit_lba %>% gather("cue_bin", "v", starts_with("v"))
lba_pars_separate_l$cue_bin <- factor(substr(lba_pars_separate_l$cue_bin, 3,3), 
                                      levels = as.character(seq_len(length(bins)-1)))
#pars_separate_l <- inner_join(pars_separate_l, agg_rr98_bin)
lba_pars_separate_l <- lba_pars_separate_l  %>% group_by(SUBJECT, CUETYPE, cue_bin) %>%
  mutate(resp_prop = pLBA(rt=Inf, response=2, A=list(a_1, a_2), sd_v=sv,
                          mean_v=c(v, 1-v), t0=t0, b=max(a_1, a_2)+b, silent=TRUE))

p1 <- xyplot(mean ~ cue_bin|SUBJECT + CUETYPE, agg_data_bin, type = "b", auto.key = 
               list(lines = TRUE), ylab = "Proportion of 'old' responses", col = "grey")
p2 <- segplot(cue_bin ~ upper+lower|SUBJECT + CUETYPE, agg_data_bin, 
              auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
              col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
              draw.bands = FALSE, angle = 90, length = 0.05, ends = "both")
p3 <- xyplot(resp_prop ~ cue_bin|SUBJECT + CUETYPE, lba_pars_separate_l, type = "b", 
             auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
             col = "black")
p2 + as.layer(p1) + as.layer(p3)



# get predicted quantiles (uses predicted response proportions)
lba_separate_pred_old <- lba_pars_separate_l %>% do(as.data.frame(t(
  qLBA(quantiles*.$resp_prop, response=2, A=list(.$a_1, .$a_2), sd_v=.$sv,
       mean_v=c(.$v, 1-.$v), t0=.$t0, b=max(.$a_1, .$a_2)+.$b, silent=TRUE)))) %>% 
  ungroup() %>% gather("quantiles", "old", V1:V5)
lba_separate_pred_new <- lba_pars_separate_l %>% do(as.data.frame(t(
  qLBA(quantiles*(1-.$resp_prop), response=1, A=list(.$a_1, .$a_2), sd_v=.$sv,
       mean_v=c(.$v, 1-.$v), t0=.$t0, b=max(.$a_1, .$a_2)+.$b, silent=TRUE)))) %>% 
  ungroup() %>% gather("quantiles", "new", V1:V5)

#separate_pred_light %>% filter(is.na(light))
lba_separate_pred <- inner_join(lba_separate_pred_old, lba_separate_pred_new)
lba_separate_pred$quantiles <- factor(lba_separate_pred$quantiles, 
                                      levels = c("V5", "V4", "V3", "V2", "V1"), 
                                      labels = c("90%", "70%", "50%", "30%", "10%"))
lba_separate_pred <- lba_separate_pred %>% gather("response", "rt", old, new)

p1 <- xyplot(rt ~ cue_bin|SUBJECT+RESPONSE, agg2_data_response, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT" & quantile == "50%", 
             layout = c(30,3), col = "grey")
p1e <- segplot(cue_bin ~ upper+lower|SUBJECT+RESPONSE, agg2_data_response, 
               auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
               col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
               draw.bands = FALSE, angle = 90, length = 0.05, ends = "both", 
               subset = CUETYPE == "CONGRUENT" & quantile == "50%", layout = c(30,3))
p2 <- xyplot(rt ~ cue_bin|SUBJECT + response, lba_separate_pred, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT" & quantiles == "50%", 
             scales = list(y = list(limits = c(0.5, 1.5))))
p2 + as.layer(p1) + as.layer(p1e)



p1 <- xyplot(rt ~ cue_bin|SUBJECT+RESPONSE, agg2_data_response, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT" & quantile == "50%", 
             layout = c(30,3), col = "grey")
p1e <- segplot(cue_bin ~ upper+lower|SUBJECT+RESPONSE, agg2_data_response, 
               auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
               col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
               draw.bands = FALSE, angle = 90, length = 0.05, ends = "both", 
               subset = CUETYPE == "INCONGRUENT" & quantile == "50%", layout = c(30,3))
p2 <- xyplot(rt ~ cue_bin|SUBJECT + response, lba_separate_pred, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT" & quantiles == "50%", 
             scales = list(y = list(limits = c(0.5, 1.5))))
p2 + as.layer(p1) + as.layer(p1e)



p1 <- xyplot(rt ~ cue_bin|SUBJECT+RESPONSE, agg2_data_response, group = quantile, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT", layout = c(30,3), col = "grey")
p1e <- segplot(cue_bin ~ upper+lower|SUBJECT+RESPONSE, agg2_data_response, 
               auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
               col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
               draw.bands = FALSE, angle = 90, length = 0.05, ends = "both", 
               subset = CUETYPE == "CONGRUENT")
p2 <- xyplot(rt ~ cue_bin|SUBJECT + response, lba_separate_pred, group = quantiles, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT", scales = list(y = list(limits = c(0.5, 1.5))))
p2 + as.layer(p1) + as.layer(p1e)



p1 <- xyplot(rt ~ cue_bin|SUBJECT+RESPONSE, agg2_data_response, group = quantile, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT", layout = c(30,3), col = "grey")
p1e <- segplot(cue_bin ~ upper+lower|SUBJECT+RESPONSE, agg2_data_response, 
               auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
               col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
               draw.bands = FALSE, angle = 90, length = 0.05, ends = "both", 
               subset = CUETYPE == "INCONGRUENT")
p2 <- xyplot(rt ~ cue_bin|SUBJECT + response, lba_separate_pred, group = quantiles, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT", scales = list(y = list(limits = c(0.5, 1.5))))
p2 + as.layer(p1) + as.layer(p1e)



# Comparing Model Fit
key <- simpleKey(text = c("data", "LBA", "Diffusion"), lines = TRUE)
key$lines$col <- c("grey", "black", "black")
key$lines$lty <- c(1, 1, 2)
key$points$col <- c("grey", "black", "black")
key$points$pch <- c(1, 0, 4)

p1 <- xyplot(mean ~ cue_bin|SUBJECT + CUETYPE, agg_data_bin, type = "b", auto.key = 
               list(lines = TRUE), ylab = "Proportion of 'old' responses", col = "grey")
p2 <- segplot(cue_bin ~ upper+lower|SUBJECT + CUETYPE, agg_data_bin, 
              auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
              col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
              draw.bands = FALSE, angle = 90, length = 0.05, ends = "both")
p3 <- xyplot(resp_prop ~ cue_bin|SUBJECT + CUETYPE, lba_pars_separate_l, type = "b", 
             auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
             col = "black", pch = 0)
p4 <- xyplot(resp_prop ~ cue_bin|SUBJECT + CUETYPE, pars_separate_l, type = "b", 
             auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
             col = "black", lty = 2, key = key, pch=4)
p4 + as.layer(p2) + as.layer(p1) + as.layer(p3)



p1 <- xyplot(rt ~ cue_bin|SUBJECT+RESPONSE, agg2_data_response, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT" & quantile == "50%", 
             layout = c(30,3), col = "grey")
p1e <- segplot(cue_bin ~ upper+lower|SUBJECT+RESPONSE, agg2_data_response, 
               auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
               col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
               draw.bands = FALSE, angle = 90, length = 0.05, ends = "both", 
               subset = CUETYPE == "CONGRUENT" & quantile == "50%", layout = c(30,3))
p2 <- xyplot(rt ~ cue_bin|SUBJECT + response, lba_separate_pred, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT" & quantiles == "50%", 
             scales = list(y = list(limits = c(0.5, 1.5))), pch = 0)
p3 <- xyplot(rt ~ cue_bin|SUBJECT + response, separate_pred, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "CONGRUENT" & quantiles == "50%", 
             scales = list(y = list(limits = c(0.5, 1.5))),
             col = "black", lty = 2, key = key, pch=4)

p3 + as.layer(p2) + as.layer(p1) + as.layer(p1e)


p1 <- xyplot(rt ~ cue_bin|SUBJECT+RESPONSE, agg2_data_response, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT" & quantile == "50%", 
             layout = c(30,3), col = "grey")
p1e <- segplot(cue_bin ~ upper+lower|SUBJECT+RESPONSE, agg2_data_response, 
               auto.key = list(lines = TRUE), ylab = "Proportion of 'old' responses", 
               col = "grey", horizontal = FALSE, segments.fun = panel.arrows,  
               draw.bands = FALSE, angle = 90, length = 0.05, ends = "both", 
               subset = CUETYPE == "INCONGRUENT" & quantile == "50%", layout = c(30,3))
p2 <- xyplot(rt ~ cue_bin|SUBJECT + response, lba_separate_pred, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT" & quantiles == "50%", 
             scales = list(y = list(limits = c(0.5, 1.5))), pch = 0)
p3 <- xyplot(rt ~ cue_bin|SUBJECT + response, separate_pred, type = "b", 
             auto.key = list(lines = TRUE), ylab = "RT (in seconds)", 
             subset = CUETYPE == "INCONGRUENT" & quantiles == "50%", 
             scales = list(y = list(limits = c(0.5, 1.5))),
             col = "black", lty = 2, key = key, pch=4)

p3 + as.layer(p2) + as.layer(p1) + as.layer(p1e)

