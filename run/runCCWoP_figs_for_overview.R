library(readxl) # read excel files (UI)
library(tidyverse) # dataframe manipulations
library(purrr) # nested list indexing
devtools::document()

################################################################################
############################# Load input data from UI ##########################
################################################################################

path_to_UI <- "Templates/Full carbon calculator for windfarms on peatlands - Version 2.14.1.xlsx" # select user input spreadsheet

dat <- getData(path_to_UI)

## I need tests to ensure all data has been passed. If not, return error messages
core.dat <- dat$core.dat
forestry.dat <- dat$forestry.dat
construct.dat <- dat$construct.dat
rm(dat)

#################### NEW INPUT VARIABLES. NEED TO ADD THESE TO UI ##############
core.dat$Peatland.restoration$t_restore_microbes <- c(Exp = 15, Min = 10, Max = 20) # time to restoration of microbial function [yr]
core.dat$Peatland.restoration$n_restore_microbes <- c(Exp = 2, Min = 2, Max = 2) # shape parameter for restoration of microbial function: 1 gives concave downward function (rapid increase in function)
core.dat$Peatland.restoration$t_fallow <- c(Exp = 10, Min = 5, Max = 15) # time between harvesting and restoration [yr]
core.dat$Peatland.restoration$n_restore <- c(Exp = 4, Min = 4, Max = 4) # shape parameter for restoration of bog plant function: 1 gives concave downward function (rapid increase in function)
#################### NEW INPUT VARIABLES. NEED TO ADD THESE TO UI ##############

core.dat$Bog.plants$t_restore <- c(Exp = 25, Min = 20, Max = 30)
core.dat$Bog.plants$G_bog <- c(Exp = 20, Min = 15, Max = 25)

## Demonstrating peatland restoration model concept:
# t_rotation <- 150
t_rotation <- 100
out0 <- run_3PG(t_rotation = t_rotation,
                t_seedling_replant = 0,
                thin = NULL,
                # f_E = 0.28,
                f_E = 0.25,
                # A0.5 = 80)
                A0.5 = 140)

# age_at_felling <- 50
age_at_felling <- 0

## Counterfactual (3PG)
p1 <- ggplot(out0$res, aes(x=t-age_at_felling, y=NPP)) +
  geom_line(col="green") +
  geom_ribbon(aes(ymin = 0, ymax = NPP), alpha=0.2, fill="green") +
  geom_vline(xintercept = 0, linetype=1) +
  theme_bw() +
  # labs(y="CO2 sequestration [tCO2]", x="Time [yr]", title="Forestry NPP (3PG)")
  labs(y="NPP forestry", x="Time [yr]", title="Simplified 3PG (LCA)")


## L_harv / L_rest
L_harv <- data.frame(t=c(0, core.dat$Peatland.restoration$t_fallow[1]),
                     e=c(1,1.25))

p2 <- ggplot(L_harv, aes(x=t, y=-e)) +
  geom_vline(data=data.frame(x=c(0,
                                 core.dat$Peatland.restoration$t_fallow[1]),
                             y=c("t", "t_fallow")),
             aes(xintercept = x, linetype=factor(y))) +
  scale_linetype_manual(values=c(1,2), labels=c(expression(t[harv]), expression(t[rest]))) +
  geom_point(col="red", size=2) +
  # geom_segment(aes(x=t, y=c(0,0), yend = -e), col="red") +
  scale_x_continuous(limits = c(0, 50)) +
  scale_y_continuous(limits=c(-2,0)) +
  labs(x="Time [yr]", y="CO2 sequestration [tCO2]", linetype="", title="Management emissions (emissions factors)") +
  theme_bw()

## L_woodproducts

W_harv <- out0$res$NPP_cum[age_at_felling]
rho_wp <- c(0.25,0.25,0.25)
CO2_C <- 0.2

W_harv_wp <- W_harv * rho_wp

k1 <- c(10, 50, 100)
t <- 0:100

W_wp <- data.frame(t=t,
                   W_wpf=CO2_C*W_harv_wp[1]*exp(-t/k1[1]),
                   W_wpm=CO2_C*W_harv_wp[2]*exp(-t/k1[2]),
                   W_wps=CO2_C*W_harv_wp[3]*exp(-t/k1[3]))

L_wp <- data.frame(t=0:(nrow(W_wp)-1),
                   L_wpf=numeric(nrow(W_wp)),
                   L_wpm=numeric(nrow(W_wp)),
                   L_wps=numeric(nrow(W_wp)))
for (i in 2:nrow(W_wp)) {
  L_wp$L_wpf[i] <- W_wp$W_wpf[i-1] - W_wp$W_wpf[i]
  L_wp$L_wpm[i] <- W_wp$W_wpm[i-1] - W_wp$W_wpm[i]
  L_wp$L_wps[i] <- W_wp$W_wps[i-1] - W_wp$W_wps[i]
}

E_wp <- c(0.2, 0.3, 0.4)
L_wp$L_wpf[1] <- E_wp[1]
L_wp$L_wpm[1] <- E_wp[2]
L_wp$L_wps[1] <- E_wp[3]

S_biofuel <- -0.2

L_wp$L_wpt <- L_wp$L_wpf + L_wp$L_wpm + L_wp$L_wps + S_biofuel

L_wp_l <- L_wp %>%
  pivot_longer(cols=c(L_wpf, L_wpm, L_wps, L_wpt), names_to = "source", values_to = "val") %>%
  add_row(t=0, source="S_biofuel", val=S_biofuel) %>%
  arrange(t)

L_wp_l$source <- factor(L_wp_l$source,
                        levels=c("S_biofuel", "L_wpf", "L_wpm", "L_wps", "L_wpt"))

p3 <- ggplot(L_wp_l, aes(x=t, y=-val, col=factor(source), linewidth=factor(source))) +
  geom_vline(data=data.frame(x=c(0),
                             y=c("t")),
             aes(xintercept = x, linetype=factor(y))) +
  scale_linetype_manual(values=c(1), labels=c(expression(t[harv]))) +
  geom_line() +
  scale_linewidth_manual(values=c(0.5,0.5,0.5,0.5,1.2)) +
  geom_point(data=L_wp_l %>% filter(t==0), aes(x=t, y=-val, col=factor(source)), size=2) +
  scale_color_brewer(palette="Reds", labels=c(expression(S[biofuel]), expression(L[wpF]), expression(L[wpM]), expression(L[wpS]), expression(L[wpT]))) +
  scale_x_continuous(limits = c(0, 50)) +
  labs(x="Time [yr]", y="CO2 sequestration [tCO2]", col="", linetype="", title="Forestry product emissions (exponential decay)") +
  theme_bw() +
  guides(linewidth = "none")


## Peatland emissions
AV_indirect <- AV_peat_drained(core.dat = core.dat,
                               construct.dat = construct.dat)

R_tot <- Emissions_rates_soils(core.dat = core.dat,
                               construct.dat = construct.dat,
                               AV_indirect = AV_indirect)

L_restoration <- list()
nn <- c(1,2.25,5)
for (n in nn) {
  core.dat$Peatland.restoration$n_restore_microbes <- c(Exp = n, Min = n, Max = n) # shape parameter for restoration of hydrology: 1 gives concave downward function (rapid increase in function)
  L_restoration[[length(L_restoration)+1]] <- CO2_loss_restoration(core.dat, lapply(AV_indirect, FUN=function(x) lapply(x, FUN=function(y) y/1)), lapply(R_tot, FUN=function(x) x/1e3))
}

L_restoration_df <- bind_rows(lapply(seq(L_restoration),
                                     FUN = function(y) lapply(seq(L_restoration[[y]]),
                                                              FUN = function(x) {
                                                                source <- names(L_restoration[[y]])[x]
                                                                df <- as.data.frame(L_restoration[[y]][[x]])
                                                                df$source <- source
                                                                df$n <- nn[y]
                                                                df$t <- 0:(nrow(df)-1)
                                                                return(df)
                                                              })))

L_restoration_df$source <- factor(L_restoration_df$source, levels=c("CO2", "CH4", "Tot"))
L_restoration_df <- L_restoration_df %>%
  pivot_longer(cols=c(Exp, Min, Max), names_to = "Est", values_to = "val")

p4 <- ggplot(L_restoration_df, aes(x=t, y=-val, col=factor(n), group_by=Est)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 50)) +
  facet_grid(Est ~ source, scales="free_y") +
  theme_bw() +
  labs(x="Time [yr]", y="CO2 sequestration [tCO2]", col="n")

L_restoration_df$source <- factor(L_restoration_df$source, levels=c("CH4", "CO2", "Tot"))

p5 <- L_restoration_df %>%
  filter(Est=="Exp", n == 2.25) %>%
  ggplot(aes(x=t, y=-val, col=factor(source), linewidth=factor(source))) +
  geom_line() +
  scale_color_brewer(palette="Reds") +
  scale_linewidth_manual(values=c(0.5,0.5,1.2)) +
  geom_vline(data=data.frame(x=c(0,
                                 core.dat$Peatland.restoration$t_fallow[1],
                                 core.dat$Peatland.restoration$t_restore_microbes[1] + core.dat$Peatland.restoration$t_fallow[1]),
                             y=c("t", "t_fallow", "t_microberest")),
             aes(xintercept = x, linetype=factor(y))) +
  scale_linetype_manual(values=c(1,2,3), labels=c(expression(t[harv]), expression(t[rest]), expression(t[restM]))) +
  theme_bw() +
  labs(y="CO2 sequestration [tCO2]", x="Time [yr]", title="Microbial emissions (ECOSSE/SCOTIA/Regression)", linetype="", col="") +
  guides(linewidth="none")

## Peatland sequestration
S_bog_sequ <- list()
nn <- c(1,2.25,5)
for (n in nn) {
  core.dat$Bog.plants$n_restore <- c(Exp = n, Min = n, Max = n) # shape parameter for restoration of hydrology: 1 gives concave downward function (rapid increase in function)
  S_bog_sequ[[length(S_bog_sequ)+1]] <- Bog_plant_restoration(core.dat, lapply(AV_indirect, FUN=function(x) lapply(x, FUN=function(y) y/1e3)))


}

S_bog_sequ_df <- bind_rows(lapply(seq(S_bog_sequ),
                                  FUN = function(x) {
                                    df <- as.data.frame(S_bog_sequ[[x]])
                                    df$n <- nn[x]
                                    df$t <- 0:(nrow(df)-1)
                                    return(df)
                                  }))

S_bog_sequ_df <- S_bog_sequ_df %>%
  pivot_longer(cols=c(Exp, Min, Max), names_to = "Est", values_to = "val")

p6 <- ggplot(S_bog_sequ_df, aes(x=t, y=val, col=factor(n), group_by=Est)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 60)) +
  facet_grid(Est ~ ., scales="free_y") +
  theme_bw() +
  labs(x="Time [yr]", y="CO2 sequestration [tCO2]", col="n")

p7 <- S_bog_sequ_df %>%
  filter(Est=="Exp", n == 5) %>%
  ggplot(aes(x=t, y=val)) +
  geom_line(col="red") +
  geom_vline(data=data.frame(x=c(0,
                                 core.dat$Peatland.restoration$t_fallow[1],
                                 core.dat$Bog.plants$t_restore[1] + core.dat$Peatland.restoration$t_fallow[1]),
                             y=c("t", "t_fallow", "t_plantrest")),
             aes(xintercept = x, linetype=factor(y))) +
  scale_linetype_manual(values=c(1,2,3), labels=c(expression(t[harv]), expression(t[rest]), expression(t[restP]))) +
  theme_bw() +
  labs(y="CO2 sequestration [tCO2]", x="Time [yr]", title="Bog plant sequestration (fixed rate per unit area)", linetype="")

## Sum contributions

L_all <- rbind(L_harv %>%
                 rename(val=e) %>%
                 mutate(val=-val) %>%
                 mutate(source="L_harv"),
               L_restoration_df %>%
                 filter(Est=="Exp", n == 2.25, source=="Tot") %>%
                 select(t,val) %>%
                 mutate(val=-val) %>%
                 mutate(source="L_microbe"),
               S_bog_sequ_df %>%
                 filter(Est=="Exp", n == 5) %>%
                 select(t,val) %>%
                 mutate(source="S_plant"),
               L_wp %>%
                 rename(val=L_wpt) %>%
                 select(t,val) %>%
                 mutate(val=-val) %>%
                 mutate(source="L_wp"))

L_all <- rbind(L_all,
               L_all %>%
                 group_by(t) %>%
                 summarise(val=sum(val)) %>%
                 mutate(source="L_all"))

L_all$source <- factor(L_all$source, levels=c("L_harv","L_microbe","S_plant","L_wp","L_all"))
p8 <- ggplot(L_all, aes(x=t, y=val, col=factor(source), linewidth = factor(source))) +
  geom_vline(data=data.frame(x=c(0,
                                 core.dat$Peatland.restoration$t_fallow[1],
                                 core.dat$Bog.plants$t_restore[1] + core.dat$Peatland.restoration$t_fallow[1],
                                 core.dat$Peatland.restoration$t_restore_microbes[1] + core.dat$Peatland.restoration$t_fallow[1]),
                             y=c("t", "t_fallow", "t_plantrest", "t_microberest")),
             aes(xintercept = x, linetype=factor(y))) +
  scale_linetype_manual(values=c(1,2,3,4), labels=c(expression(t[harv]), expression(t[rest]), expression(t[restM]), expression(t[restP]))) +
  geom_line() +
  scale_color_brewer(palette="Reds", labels=c(expression(L[harv]), expression(L[M]), expression(NPP[P]), expression(L[wp]), expression(L[all]))) +
  scale_linewidth_manual(values=c(0.5,0.5,0.5,0.5,1.2)) +
  labs(x="Time [yr]", y="CO2 sequestration [tCO2]", linetype="", col="", title="Net emissions restored peatland") +
  theme_bw() +
  guides(linewidth = "none")

## Carbon payback time

res <- rbind(out0$res %>%
               select(NPP, t) %>%
               rename(val=NPP) %>%
               mutate(t=t-age_at_felling,
                      source="Forestry"),
             L_all %>%
               filter(source=="L_all") %>%
               select(val, t) %>%
               mutate(source="Peatland"))

res_payback <- left_join(res %>%
                           filter(source=="Forestry" & t >= 0) %>%
                           mutate(val = cumsum(val)),
                         res %>%
                           filter(source=="Peatland" & t >= 0) %>%
                           mutate(val = cumsum(val)),
                         by="t")
res_payback$val_diff <- res_payback$val.x - res_payback$val.y

p9 <- ggplot(res, aes(x=t, y=val, col=factor(source))) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = val, fill=factor(source)), alpha=0.2) +
  scale_colour_manual(values=c("green", "red")) +
  scale_fill_manual(values=c("green", "red")) +
  geom_vline(data=data.frame(x=c(0,
                                 min((res_payback %>%
                                        filter(val_diff<=0))$t)),
                             y=c("t_harv", "t_payback")),
             aes(xintercept = x, linetype=factor(y))) +
  scale_linetype_manual(values=c(1,2), labels=c(expression(t[harv]), expression(t[payback]))) +
  labs(x="Time [yr]", y="CO2 sequestration [tCO2]", linetype="", col="", fill="", title="Peatland restoration carbon payback time") +
  theme_bw()

ww <- 14
hh <- 10
png("../Figures/Forestry_NPP.png", width=ww, height=hh, units="cm", res=300)
p1
dev.off()

png("../Figures/Management_emissions.png", width=ww, height=hh, units="cm", res=300)
p2
dev.off()

png("../Figures/Forestry_product_emissions.png", width=ww, height=hh, units="cm", res=300)
p3
dev.off()

png("../Figures/Emissions_restoration_model.png", width=ww, height=hh, units="cm", res=300)
p4
dev.off()

png("../Figures/Microbial_emissions.png", width=ww, height=hh, units="cm", res=300)
p5
dev.off()

png("../Figures/Sequestration_restoration.png", width=ww, height=hh, units="cm", res=300)
p6
dev.off()

png("../Figures/Bog_plant_sequestration.png", width=ww, height=hh, units="cm", res=300)
p7
dev.off()

png("../Figures/Net_emissions_peatland.png", width=ww, height=hh, units="cm", res=300)
p8
dev.off()

png("../Figures/Carbon_payback_time.png", width=ww, height=hh, units="cm", res=300)
p9
dev.off()

