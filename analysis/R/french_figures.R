# Source lines 1-153 in inference_figs.R
# Figure 4: Trib time series ----

# trib time series ----
tribs.all <- read.csv(here("analysis/data/raw/trib-spwn.csv")) |>
  mutate(
    estimate = case_when(
      system == "whitehorse" ~ estimate*(1-hatch_contrib),
      .default = estimate)) |>
  unite(tributary, c("system_alt", "type"), sep = " ") |>
  select(!hatch_contrib)


dat_text <- tribs.all |>
  group_by(tributary) |>
  slice_head() |>
  select(tributary, CU)


trib_order <- c("Porcupine sonar","Miner aerial","Klondike sonar","Chandindu weir","Tincup aerial" ,"Ross aerial","Pelly aerial",
                "Pelly sonar","Blind Creek weir","Tachun foot", "Tachun weir","Little Salmon aerial","Big Salmon aerial",
                "Big Salmon sonar","Tahkini aerial","Tahkini sonar","Whitehorse fishway","Michie foot",
                "Teslin sonar","Nisutlin aerial","Nisutlin sonar","Wolf aerial", "Morley aerial")
tribs.all$tribs_name_ord<- factor(tribs.all$tributary, levels = trib_order)



tribs.all <- tribs.all |>
  mutate(tribs_name = gsub("salmon", "Salmon", gsub("_", "-", str_to_sentence(tributary)))) |>
  mutate(CU_f = factor(gsub("BigSalmonR", "Big.Salmon", CU), levels=c(CU_order, "Porcupine"))) |>
  arrange(CU_f) |>
  mutate(tribs_name = factor(tribs_name, levels=unique(tribs_name)))

tribs.all <- tribs.all |>
  mutate(tribs_name_ord_FR = case_when(
    tribs_name_ord == "Chandindu weir" ~ "Fascine Chandindu",
    tribs_name_ord == "Blind Creek weir" ~ "Fascine Blind ",
    tribs_name_ord == "Klondike sonar" ~ "Sonar Klondike ",
    tribs_name_ord == "Michie foot"~ "À pied Michie",
    tribs_name_ord == "Teslin sonar" ~ "Sonar Teslin",
    tribs_name_ord == "Nisutlin sonar" ~ "Sonar Nisutlin",
    tribs_name_ord == "Miner aerial" ~ "Aérien Miner",
    tribs_name_ord == "Nisutlin aerial" ~ "Aérien Nisutlin",
    tribs_name_ord == "Ross aerial" ~ "Aérien Ross",
    tribs_name_ord == "Big Salmon aerial" ~ "Aérien Big Salmon",
    tribs_name_ord == "Big Salmon sonar" ~ "Sonar Big Salmon",
    tribs_name_ord == "Pelly aerial" ~ "Aérien Pelly",
    tribs_name_ord == "Pelly sonar" ~ "Sonar Pelly",
    tribs_name_ord == "Porcupine sonar" ~ "Sonar Porcupine",
    tribs_name_ord == "Tachun foot" ~ "À pied Tachun",
    tribs_name_ord == "Tachun weir" ~ "Fascine Tachun",
    tribs_name_ord == "Tahkini aerial" ~ "Aérien Tahkini",
    tribs_name_ord == "Tahkini sonar" ~ "Sonar Tahkini",
    tribs_name_ord == "Tincup aerial" ~ "Aérien Tincup",
    tribs_name_ord == "Whitehorse fishway" ~ "Passe Whitehorse",
    tribs_name_ord == "Morley aerial" ~ "Aérien Morley",
    tribs_name_ord == "Wolf aerial" ~ "Aérien Wolf",
    tribs_name_ord == "Little Salmon aerial" ~ "Aérien Little Salmon"
  ))

tribs.all |>
  ggplot(aes(x = year, y = estimate/1000, col=CU_f)) +
  scale_color_viridis_d() +
  geom_line(lwd = 0.8) +
  labs(x="Year", y="Spawners (000s)", col="CU") +
  facet_wrap(~tribs_name_ord_FR, ncol=4, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_sleek() +
  theme(axis.title = element_text(size=12),
        strip.text = element_text(size=8))

ggsave(here("csasdown/figure/french/trib-escape.PNG"), height = 700*2,
       width=1000*2, units="px", dpi= 240)

# Figure 5: CU run-timing plot ----

FR_labels <- c("Yukon Nord et ses affluents" , "White et ses affluents ", "Pelly", "Stewart", "Nordenskiold", "Eaux d’amont du Yukon-de la Treslin", "Moyen Yukon et ses affluents",
                "Cours supérieur du Yukon ")

CU_orders <- c(1,2,5,3,6,9,4,8)
cols <- viridis(rpt$nS+1)
cols_ordered <- cols[CU_orders]
x <- rpt$day_d
x_trunc <- x[x>=170 & x <=270]

y_dpt <- rpt$rho_dst
y_dp  <- apply( y_dpt, 1:2, mean )

png( file=here("csasdown/figure/french/CU-run-timing.PNG"), width= 9, height = 5.562,units="in", res =700 )

par(mar=c(5,15,1,1),oma=c(0,0,0,0),
    col.lab = "grey40")

plot( x=range(x_trunc), c(1,rpt$nS+1.5), type="n", axes=FALSE,
      xlab="Jour de l’année ", ylab="" )
axis( side=1 , col="grey40", col.axis="grey40")
axis( side=2, at=rpt$nS:1, labels=FR_labels,
      las=1, cex.axis=1, col="grey40", col.axis="grey40" )
for( p in 1:rpt$nS )
{
  y <- 1.5*(y_dp[ ,p]/max(y_dp[ ,p]))
  polygon( x=c(x,rev(x)), y=rpt$nS-p+1+c(y,rep(0,length(x))),
           border=NA, col=cols_ordered[p] )
}

abline(v=205, col="grey",lty=2)
dev.off()

# Figure 6: Escapement plot ----


esc$CU_f <- factor(esc$stock, levels = CU_order)

esc |>
  left_join(CU_name_lookup, by="CU_f") |>
  ggplot(aes(x = year, y = mean/1000)) +
  geom_ribbon(aes(ymin = lower/1000, ymax = upper/1000),  fill = "darkgrey", alpha = 0.5) +
  geom_line(lwd = 1.1, col="grey30") +
  xlab("Année ") +
  ylab("Géniteurs (en milliers) ") +
  facet_wrap(~CU_pretty_FR, ncol=3, scales = "free_y") +
  theme_sleek() +
  theme(strip.text = element_text(size=10))

ggsave(here("csasdown/figure/french/cu-escape.PNG"), width=900*2, height=800*2, units="px",
       dpi=240)

# Figure 7: CU comps ----

esc |>
  left_join(CU_name_lookup, by="CU_f")

total_spawn <- esc |>
  left_join(CU_name_lookup, by="CU_f") |>
  group_by(year) |>
  summarise(total_spwn = sum(mean)) |>
  ungroup()

prop_CU <- esc |>
  left_join(total_spawn, by="year") |>
  left_join(CU_name_lookup, by="CU_f")  |>
  mutate(contr = (mean/total_spwn)*100) |>
  select(CU_f,CU_pretty,year,contr)

prop_CU_avg <- prop_CU |>
  group_by(CU_f) |>
  summarize(avg_cont = mean(contr))

ggplot(prop_CU, aes(x=year, y=contr, fill=CU_pretty)) +
  geom_area() +
  scale_fill_viridis_d() +
  labs(fill="Unité de conservation", x = "Année", y = "Contribution au total des géniteurs (%)") +
  theme_sleek()

my.ggsave(here("csasdown/figure/french/percent-cc-contribution.PNG"), height=4.25, width=8)

# Figure 8: Tribs vs CU RRs ---

esc_join <- esc |>
  mutate(CU = stock,
         mean = mean/1000) |>
  select(CU, year, mean, CU_f)
esc$CU_f <- factor(esc$stock, levels = CU_order)

tribs <- read.csv(here("analysis/data/raw/trib-spwn.csv")) |>
  filter(CU != "Porcupine")|>
  filter(system != "teslin") |>
  unite(tributary, c("system", "type")) |>
  select(!hatch_contrib)

trib_rr <- left_join(tribs,esc_join,by = join_by("CU", "year")) |>
  drop_na(c("mean", "estimate")) |>
  filter(! tributary %in% c("morley_aerial", "chandindu_weir","nisutlin_sonar", "pelly_aerial", "ross_aerial"))


trib_rr <- trib_rr |>
  mutate(tribs_name = gsub("creek", " Creek",
                           gsub("salmon", " Salmon",
                                gsub("_", " ", str_to_sentence(tributary)))))

trib_order_RR <- c("Klondike sonar", "Tincup aerial", "Pelly sonar", "Blind Creek weir", "Tachun foot","Tachun weir","Little Salmon aerial",
                   "Tahkini aerial","Tahkini sonar","Whitehorse fishway","Michie foot","Wolf aerial","Nisutlin aerial")
trib_rr$tribs_name_ord<- factor(trib_rr$tribs_name, levels = trib_order_RR)

trib_rr <- trib_rr |>
  mutate(tribs_name_ord_FR = case_when(
    tribs_name_ord == "Blind Creek weir" ~ "Fascine Blind ",
    tribs_name_ord == "Klondike sonar" ~ "Sonar Klondike ",
    tribs_name_ord == "Michie foot"~ "À pied Michie",
    tribs_name_ord == "Nisutlin aerial" ~ "Aérien Nisutlin",
    tribs_name_ord == "Pelly sonar" ~ "Sonar Pelly",
    tribs_name_ord == "Tachun foot" ~ "À pied Tachun",
    tribs_name_ord == "Tachun weir" ~ "Fascine Tachun",
    tribs_name_ord == "Tahkini aerial" ~ "Aérien Tahkini",
    tribs_name_ord == "Tahkini sonar" ~ "Sonar Tahkini",
    tribs_name_ord == "Tincup aerial" ~ "Aérien Tincup",
    tribs_name_ord == "Whitehorse fishway" ~ "Passe Whitehorse",
    tribs_name_ord == "Wolf aerial" ~ "Aérien Wolf",
    tribs_name_ord == "Little Salmon aerial" ~ "Aérien Little Salmon"
         ))

ggplot(trib_rr,aes(x = mean, y = estimate)) +
  geom_smooth(method="lm", color="grey") +
  geom_point(size=2, color="dark grey")+
  xlab("Géniteurs des UC (en milliers) ") +
  ylab("Reproducteurs des affluents ") +
  theme_sleek() +
  facet_wrap(~tribs_name_ord_FR, scales = "free", ncol = 4) +
  theme(axis.title = element_text(size=12))

ggsave(here("csasdown/figure/french/RR-vs-trib-spawners.PNG"), width=777*2, height=800*2, units="px",
       dpi=240)

# Figure 9: posterior ref points ----
# Figure 8: posterior distribution of reference points ----
bench.long <- pivot_longer(bench.posts, cols = c(Smsr.20, Smsr.40, S.recent), names_to = "par") |>
  arrange(CU, par, value) |>
  filter(value <= 10000) #hack to cut off fat tails to help with density visualization, also an IUCN cutoff...

bs <- bench.long |>
  filter(CU == "Big.Salmon",
         value <= 6000)

n <- bench.long |>
  filter(CU == "Nordenskiold",
         value <= 2500)

s <- bench.long |>
  filter(CU == "Stewart",
         value <= 4000)

u <- bench.long |>
  filter(CU == "UpperYukonR.",
         value <= 4000)


w <- bench.long |>
  filter(CU == "Whiteandtribs.",
         value <= 5000)

t <- bench.long |>
  filter(CU == "YukonR.Teslinheadwaters",
         value <= 5000)

p <- bench.long |>
  filter(CU == "Pelly",
         value <= 9000)

m <- bench.long |>
  filter(CU == "MiddleYukonR.andtribs.",
         value <= 10000)

no <- bench.long |>
  filter(CU == "NorthernYukonR.andtribs.",
         value <= 10000)

custom.bench <- rbind(bs,n,s,u,w,t,p,m,no) |>
  mutate(CU_f = CU) |>
  left_join(CU_name_lookup, by="CU_f")

b <- ggplot(custom.bench |> filter(), aes(Smsr/1000, fill = CU_pretty_FR, color = CU_pretty_FR)) +
  geom_density(alpha = 0.3,bw=0.6) +
  theme(legend.position = "bottom") +
  labs(x = expression(italic(N[NMR])), y = "Densité a posteriori") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_sleek()   +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        legend.position=c(0.68, 0.65)) +
  guides(fill=guide_legend(ncol=2, theme = theme(legend.byrow = TRUE)),
         colour=guide_legend(ncol=2, theme = theme(legend.byrow = TRUE))) +
  scale_x_continuous(limits = c(0, 25))

c <- ggplot(custom.bench |> filter(), aes(Umsy, fill = CU_pretty_FR, color = CU_pretty_FR)) +
  geom_density(alpha = 0.3,bw=0.03) +
  theme(legend.position = "bottom") +
  labs(x = expression(italic(U[RMD])), y = "Densité a posteriori") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_sleek()   +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        legend.position="none") +
  scale_x_continuous(limits = c(0, 1))


par.long <- par.posts |>
  mutate(CU_f = CU,
         alpha = exp(ln_a)) |>
  left_join(CU_name_lookup, by="CU_f")

a <- ggplot(par.long, aes(alpha, fill = CU_pretty_FR, color = CU_pretty_FR)) +
  geom_density(alpha = 0.3,bw=0.4) +
  labs(x = "Productivité intrinsèque", y = "Densité a posteriori") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_sleek()   +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title=element_blank()) +
  scale_x_continuous(limits = c(0, 15))

cowplot::plot_grid(a, b, c, labels="auto", ncol=1)

ggsave(here("csasdown/figure/french/par-ref-hist.PNG"), width = 675*2, height = 900*2,
       units="px", dpi=240)

# Figure 10: SR fits ----
ggplot() +
  geom_abline(intercept = 0, slope = 1,col="dark grey") +
  geom_ribbon(data = SR.preds, aes(x = Spawn/1000, ymin = Rec_lwr/1000, ymax = Rec_upr/1000),
              fill = "grey80", alpha=0.5, linetype=2, colour="gray46") +
  geom_errorbar(data = brood.all, aes(x= S_med/1000, y = R_med/1000,
                                      ymin = R_lwr/1000, ymax = R_upr/1000),
                colour="grey", width=0, linewidth=0.3) +
  geom_errorbarh(data = brood.all, aes(y = R_med/1000, xmin = S_lwr/1000, xmax = S_upr/1000),
                 height=0, colour = "grey", linewidth = 0.3) +
  geom_point(data = brood.all,
             aes(x = S_med/1000,
                 y = R_med/1000,
                 color=BroodYear),
             size = 1.5) +
  geom_line(data = SR.preds, aes(x = Spawn/1000, y = Rec_med/1000)) +
  facet_wrap(~CU_f, scales = "free", labeller=CU_labeller_FR) +
  scale_colour_viridis_c(name = "Brood Année")+
  labs(x = "Géniteurs (en milliers)",
       y = "Recrues (en milliers)") +
  theme_sleek()+
  theme(legend.position = c(0.94,0.925),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.title = element_text(size=7, vjust=3),
        legend.text = element_text(size=6, angle=0, hjust=0),
        strip.text = element_text(size=10))

ggsave(here("csasdown/figure/french/SR_fits_AR1.PNG"), height=800*2, width=900*2, units="px", dpi=240)


# Figure 11: TV alpha


# Figure 11: TV alpha ----
a.yrs.all |>
  filter(brood_year < 2018) |>
  left_join(CU_name_lookup, by=c("CU"= "CU_f")) |>
  ggplot(aes(color = CU_pretty_FR)) +
  geom_line(aes(x = brood_year , y = mid), lwd = 1.5) +
  scale_color_viridis_d() +
  theme_sleek() +
  geom_hline(yintercept = 1, lty=2, col = "grey") +
  labs(y ="Productivité (\U03B1)", x = "Année d’éclosion")+
  guides(color=guide_legend(title="Unité de conservation")) +
  theme(legend.position = c(0.62,0.75),
        plot.margin = margin(60,60,10,60),
        legend.text = element_text(size=7),
        axis.title = element_text(size=12))

ggsave(here("csasdown/figure/french/changing_productivity.PNG"), height = 550*2,
       width = 700*2, units="px", dpi=200)

# Figure 12: demographics ----
# Source line 1-134 in asl_wrangle.R
fem_age_comp <- fem_age_comps[,c(1,3:6)]%>%
  group_by(Sample.Year) %>%
  mutate(total_prop=sum(age_4,age_5,age_6,age_7),
         age4=age_4/total_prop,
         age5=age_5/total_prop,
         age6=age_6/total_prop,
         age7=age_7/total_prop)%>%
  select(Sample.Year,age4, age5, age6, age7)%>%
  rename(Quatre = age4, Cinq = age5, Six = age6, Sept = age7)%>%
  pivot_longer(!Sample.Year , names_to = "Âge",
               values_to = "prop")

fem_age_comp$age_f <- factor(fem_age_comp$Âge, levels = c("Quatre", "Cinq", "Six", "Sept"))

a <- ggplot(fem_age_comp, aes(fill=age_f, y=prop, x=Sample.Year)) +
  geom_bar(position="stack", stat="identity")+
  xlab("Année") +
  ylab("Proportion") +
  scale_fill_viridis_d(name = "Âge") +
  theme_sleek() +
  theme(legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size=7),
        legend.text = element_text(size=7),
        legend.position="top",
        plot.margin = margin(10,20,0.5,20),
        axis.title = element_text(size=7),
        axis.text = element_text(size=7))


prop_females <- rowSums(fem_age_comps[,3:6])

sex_ratio<-cbind(seq(1985,2024),prop_females)
colnames(sex_ratio)<-c("Year","prop_fem")
sex_ratio<-as.data.frame(sex_ratio)

b <- ggplot(sex_ratio, aes(x = Year, y = prop_fem)) +
  geom_smooth(method="lm", color="grey") +
  geom_point(size=2, color="dark grey")+
  xlab("Année") +
  ylab("Proportion de femelles") +
  coord_cartesian(ylim=c(0,1)) +
  theme_sleek() +
  theme(strip.text = element_text(size = 7),
        plot.margin = margin(15,20,0.5,20),
        axis.title = element_text(size=7),
        axis.text = element_text(size=7))

laa <- as.data.frame(fem_len_comp)%>%
  select(Sample.Year,age_4, age_5, age_6, age_7)%>%
  rename(Quatre = age_4, Cinq = age_5, Six = age_6, Sept = age_7)%>%
  pivot_longer(!Sample.Year , names_to = "Âge",
               values_to = "length")


laa$age_f <- factor(laa$Âge, levels = c("Quatre", "Cinq", "Six", "Sept"))

c <- ggplot(laa, aes(x = Sample.Year, y = length)) +
  geom_smooth(method="lm", color="grey") +
  geom_point(size=2, color="dark grey")+
  xlab("Année") +
  ylab("Longueur des femelles \n (mm; MEF) ") +
  theme_sleek() +
  facet_wrap(~age_f, scales = "free_y") +
  scale_x_continuous(breaks=c(1990,2000,2010,2020)) +
  theme(strip.text.x = element_text(size=7),
        axis.title = element_text(size=7),
        axis.text = element_text(size=6),
        plot.margin = margin(0.5,10,1,0.5))


d <- ggplot(reproOutput, aes(x = V1, y = reproOutputPerSpawnerEggs)) +
  geom_smooth(method="lm", color="grey") +
  geom_point(size=2, color="dark grey")+
  xlab("Année") +
  ylab("Efficacité de reproduction moyenne \n (nombre d'œufs total par géniteur)") +
  theme_sleek() +
  theme(plot.margin = margin(45,10,0.5,0.5),
        axis.title = element_text(size=7),
        axis.text = element_text(size=7))

g <- ggarrange(b,c,a,d,
               labels = c("a", "b","c", "d"),
               heights = c(0.8,1))


g
ggsave(here("csasdown/figure/french/asl.PNG"), height = 550*3,
       width = 700*3, units="px", dpi=200*2)



# Figure 13: egg-mass recruitment relationships ----
# run line s 1-125 in demographic)ref_points.R
ggplot() +
  geom_ribbon(data = ER.preds, aes(x = EM/1000, ymin = Rec_lwr/1000, ymax = Rec_upr/1000),
              fill = "grey80", alpha=0.5, linetype=2, colour="gray46") +
  geom_errorbar(data = brood.all, aes(x= EM_med/1000, y = R_med/1000,
                                      ymin = R_lwr/1000, ymax = R_upr/1000),
                colour="grey", width=0, linewidth=0.3) +
  geom_errorbarh(data = brood.all, aes(y = R_med/1000, xmin = EM_lwr/1000, xmax = EM_upr/1000),
                 height=0, colour = "grey", linewidth = 0.3) +
  geom_point(data = brood.all,
             aes(x = EM_med/1000,
                 y = R_med/1000,
                 color=BroodYear),
             size = 1.5) +
  geom_line(data = ER.preds, aes(x = EM/1000, y = Rec_med/1000)) +
  facet_wrap(~CU_f, scales = "free", labeller=CU_labeller_FR) +
  scale_colour_viridis_c(name = "Brood Année")+
  labs(x = "Masse d’œufs (kg)",
       y = "Recrues (en milliers)") +
  theme_sleek()+
  theme(legend.position = c(0.94,0.925),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.title = element_text(size=7, vjust=3),
        legend.text = element_text(size=6, angle=0, hjust=0),
        strip.text = element_text(size=10))

ggsave(here("csasdown/figure/french/EM-R_fits.PNG"), height=800*2, width=900*2, units="px", dpi=240)

# Figure 14: SMU run ----
SMU_RR <- read.csv(here("analysis/data/raw/rr_95_table.csv")) |>
  filter(Stock == "Canada",
         Counts != "Harvest")

a<- ggplot(SMU_RR |>
             filter(Counts != "Exploitation")) +
  geom_hline(yintercept = 31, col = "darkred", lty=2) +
  annotate("text", label="PRIP", x=2023, y=38, size=2.5) +
  geom_hline(yintercept = 86, col = "dark green", lty=2) +
  annotate("text", label="PRS", x=2023, y=93, size=2.5) +
  geom_ribbon(aes(x = Year, ymin = Lower95./1000, ymax = Upper95./1000, col = Counts, fill = Counts), alpha=0.5) +
  geom_line(aes(x = Year, y = Median50./1000, col = Counts), size = 1) +
  ylab("Poissons (en milliers)") +
  xlab("Année") +
  scale_color_discrete() +
  scale_color_manual(values=c('#999999','#E69F00'),labels = c("Échappée", "Remonte")) +
  scale_fill_manual(values=c('#999999', '#E69F00'),labels = c("Échappée", "Remonte")) +
  theme_sleek() +
  theme(legend.position = c(0.75,0.85),
        legend.title = element_blank(),
        legend.text = element_text(size=11),
        axis.title = element_text(size=12),
        plot.margin = margin(0.5,20,0.5,0.5))

b<- ggplot(SMU_RR |>
             filter(Counts == "Exploitation")) +
  geom_hline(yintercept = 37, col = "darkred", lty=2) +
  annotate("text", label="TRR", x=2024, y=40, size=2.5) +
  geom_ribbon(aes(x = Year, ymin = Lower95., ymax = Upper95.), fill="darkblue", col="darkblue", alpha=0.4) +
  geom_line(aes(x = Year, y = Median50.), size = 1, col = "darkblue") +
  ylab("Taux de prélèvement (%)") +
  xlab("Année ") +
  theme_sleek() + theme(plot.margin = margin(0.5,20,0.5,0.5),
                        axis.title = element_text(size=12))

cowplot::plot_grid(a, b, labels="auto", ncol=2)


my.ggsave(here("csasdown/figure/french/SMU-run-esc.PNG"), width = 8, height = 3)

# Figure 16: hatchery releases ----
## hatchery releases ----
rel_rep <- read.csv("./analysis/data/raw/yukon_releases.csv")

yukn_rel <- rel_rep %>%
  filter(Status != "Exclude") %>%
  filter(STOCK_PROD_AREA_CODE == "YUKN") %>% #Exclude - not part of the Yukon watershed
  filter(!is.na(REL_CU_INDEX)) %>%
  mutate(REL_CU = paste(REL_CU_INDEX, REL_CU_NAME)) %>%
  mutate(FACILITY_NAME = case_when(FACILITY_NAME == "Yukon River H" ~ "Whitehorse Rapids Fish H",
                                   FACILITY_NAME == "McIntyre Creek H" ~ "McIntyre Creek Fish Incubation Facility",
                                   FACILITY_NAME == "Klondike River, North H" ~ "North Klondike River H",
                                   grepl("Schools", FACILITY_NAME) ~ "School Programs",
                                   .default = FACILITY_NAME)) %>%
  mutate(FACILITY_NAME = gsub(" H", " Hatchery", FACILITY_NAME)) %>%
  group_by(RELEASE_YEAR,FACILITY_NAME, REL_CU) %>%
  dplyr::summarise(TotalRelease = sum(TotalRelease, na.rm = TRUE),
                   .groups = 'drop') %>%
  arrange(REL_CU) |>
  mutate(FACILITY_NAME = case_when(FACILITY_NAME == "Whitehorse Rapids Fish Hatchery" ~ "Écloserie des rapides de Whitehorse",
                                   FACILITY_NAME == "McIntyre Creek Fish Incubation Facility" ~ "Installation d’incubation des poissons du ruisseau McIntyre",
                                   FACILITY_NAME == "North Klondike River Hatchery" ~ "Écloserie de la rivière North Klondike",
                                   FACILITY_NAME == "School Programs" ~ "Programmes scolaires",
                                   FACILITY_NAME == "Mayo River Hatchery" ~ "Écloserie de la rivière Mayo"))|>
  mutate(REL_CU_FR = case_when(REL_CU == "CK-68 YUKON RIVER-TESLIN HEADWATERS" ~ "CK-68 EAUX D’AMONT DU FLEUVE YUKON-DE LA RIVIÈRE TESLIN",
                               REL_CU == "CK-69 UPPER YUKON RIVER" ~ "CK-69 COURS SUPÉRIEUR DU FLEUVE YUKON",
                               REL_CU == "CK-73 MIDDLE YUKON RIVER AND TRIBUTARIES" ~ "CK-73 MOYEN YUKON ET SES AFFLUENTS",
                               REL_CU == "CK-72 PELLY" ~ "CK-72 PELLY",
                               REL_CU == "CK-74 STEWART" ~ "CK-74 STEWART",
                               REL_CU == "CK-73 MIDDLE YUKON RIVER AND TRIBUTARIES" ~ "CK-73 MOYEN YUKON ET SES AFFLUENTS",
                               REL_CU == "CK-76 NORTHERN YUKON RIVER AND TRIBUTARIES" ~ "CK-76 YUKON NORD ET SES AFFLUENTS "))


breakV <- seq(min(yukn_rel$RELEASE_YEAR), max(yukn_rel$RELEASE_YEAR), by = 5) # breaks for fig

ggplot(yukn_rel, aes(x=RELEASE_YEAR, y=TotalRelease/1000, fill = REL_CU_FR))+
  geom_bar(stat = "identity", width = 1, colour="white", linewidth = 0.1) +
  facet_wrap(~FACILITY_NAME, ncol=2,
             scales = "free_y") +
  scale_x_continuous(name = "Année de relâche", breaks = breakV) +
  scale_y_continuous(name = "Relâches totales (en milliers)") +
  scale_fill_viridis_d() + labs(fill="Unité de conservation de la relâche") +
  theme_sleek() +
  theme(legend.position = c(0.75, 0.15),
        strip.text = element_text(size=11),
        axis.title = element_text(size=13))

my.ggsave(here("csasdown/figure/french/hatch_bar.PNG"),width= 8, height = 5, dpi= 180)

# Figure 17:  proportion hatchery fish at whitehorse fishway ----

trib.spwn <- read.csv(here('analysis/data/raw/trib-spwn.csv'))
wh.hatch <- trib.spwn[!is.na(trib.spwn$hatch_contrib),]

wh.hatch <- wh.hatch %>% mutate(Hatchery = estimate*hatch_contrib,
                                Wild = estimate*(1-hatch_contrib)) %>%
  pivot_longer(cols=c("Hatchery", "Wild"), names_to="Origin", values_to="Returns")

ggplot(wh.hatch, aes(x=year)) +
  geom_bar(aes(y=Returns, fill=Origin), stat="identity") +
  scale_y_continuous(sec.axis = sec_axis(~./4000, name="Proportion")) +
  geom_line(aes(y=hatch_contrib*4000, col="Proportion de géniteurs \n d’écloserie"), linewidth=0.6, alpha=0.6) +
  geom_line(aes(y=pni*4000, col="Influence naturelle \n proportionnelle (IPN)"), linewidth=0.6, alpha=0.6) +
  scale_color_manual(name = "", values=c("Proportion de géniteurs \n d’écloserie" = "darkblue", "Influence naturelle \n proportionnelle (IPN)" = "darkred"), guide ="legend") +
  scale_fill_manual(name = "Remontes", values=c("Hatchery" = "pink2", "Wild" = "green4")) +
  theme_sleek() + labs(x="Année" ) +
  theme(axis.text.y.right = element_text(margin = margin(r=9)),
        legend.position = c(0.27,0.7),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9),
        axis.title = element_text(size=10),
        legend.spacing.y = unit(0, "pt"))

my.ggsave(here("analysis/plots/trib-rr/hatch_prop.PNG"))
ggsave(here("csasdown/figure/hatch_prop.PNG"), width=700*2, height=400*2, dpi=240,
       units="px")

# Figure 19: productivity scenarios ----
# Source lines 1-53 in fwd_sims_figs.R first

alpha.posts |> filter(scenario != "stationary") |>
  mutate(CU_f = factor(CU, levels=CU_order)) |>
  mutate(scenario = str_to_sentence(scenario)) |>
  mutate(scenario_fr = case_when(scenario == "Reference (most recent generation)" ~ "Référence (génération la plus récente)",
                                 scenario == "Robustness (long-term average)" ~ "Robustesse (moyenne à long terme)",
                                 scenario == "Robustness (most recent year)" ~ "Robustesse (années les plus récentes)")) |>
  mutate(scenario_fr = str_to_sentence(scenario_fr)) |>
  ggplot(aes(value, fill = scenario_fr, color = scenario_fr)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~CU_f, scales = "free_y", labeller=CU_labeller_FR) +
  theme_sleek() +
  scale_x_continuous(limits = c(NA, 4)) +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, lty=2, col="grey") +
  scale_colour_grey(aesthetics = c("colour", "fill"),start = 0.3, end = 0.6) +
  guides(fill=guide_legend(nrow=2),
         colour=guide_legend(nrow=2)) +
  labs(y = "", x = expression(Log(alpha)), fill="Scénario de productivité", color="Scénario de productivité")

ggsave(here("csasdown/figure/french/OM-productivity-scenarios.PNG"), height=600*2, width=777*2,
       units = "px", dpi=240)

# Figure 20: visualize HCRs ----
HCR_cols <- c("#B07300", "purple3", "grey25", "#CCA000", "#FEE106",  "#0F8A2E", "#3638A5")
names(HCR_cols) <- c("Solution de rechange de l’AP", "Fixed ER 60", "No fishing", "Moratoire", "Plafond du moratoire", "OEGP", "Plafond de l’OEGP")
HCR_lookup <- data.frame(HCR=c(HCRs[1:6], "fixed.ER.60"), HCR_name = names(HCR_cols)[c(3:4,6,5,7,1,2)])
HCR_lookup
HCR_order <- c("Moratoire", "OEGP", "Plafond du moratoire", "Plafond de l’OEGP", "Solution de rechange de l’AP")
ER.cap <- round(min(filter(read.csv(here("analysis/data/generated/bench_par_table.csv")),
                           bench.par == "Umsy")$mean), 2) # Umsy for least productive CU
out <- visualize_HCR(HCRs=HCRs[2:6], ER.cap=ER.cap) # get simulated HRs
out <- left_join(out, HCR_lookup, by="HCR")
out$HCR_name <- factor(out$HCR_name, levels=HCR_order)

ggplot(out) + geom_line(aes(x=run_size/1000, y=HR*100, col=HCR_name), linewidth=0.75) +
  scale_colour_manual(values=HCR_cols, guide="none") +
  scale_fill_manual(values="grey70", guide="legend") +
  facet_wrap(~HCR_name) +
  labs(x="Taille de la remonte (en milliers)", y="Taux de prélèvement (%)") +
  theme_minimal() + theme(strip.text = element_text(size=10),
                          axis.title = element_text(size=10)) +
  lims(x=c(0,200)) +
  scale_y_continuous(breaks=seq(0,100,20), limits=c(0,100))

my.ggsave(here("analysis/plots/fwd-sim/HCR_visualize.PNG"))
my.ggsave(here("csasdown/figure/french/HCR_visualize.PNG"), height=4,
          width=6)

# Figure 21: spawner trajectories
 i = "simple"

S.fwd |> filter(HCR %in% HCR_grps[[i]]) |>
  left_join(HCR_lookup, by="HCR") |>
  ggplot() +
  # Observations:
  geom_ribbon(data = filter(spwn.obs, year >= max(spwn.obs$year)-7),
              aes(ymin = S.25/1000, ymax = S.75/1000,
                  x= year), #offset to return year
              fill = "grey", color = "grey") +
  geom_line(data = filter(spwn.obs, year >= max(spwn.obs$year)-7),
            aes(y=S.50/1000, x= year), color = "black") +
  # Projections:
  geom_ribbon(aes(ymin = S.25/1000, ymax = S.75/1000, x = year, color=HCR_name, fill = HCR_name),
              alpha = 0.2) +
  geom_line(aes(year, S.50/1000, color = HCR_name), lwd=1) +
  # Benchmarks:
  geom_hline(data=all.bench,
             aes(yintercept = rt/1000), lty=2,
             color = "pink3") +
  geom_hline(data=all.bench,
             aes(yintercept = upr_bench/1000), lty=2,
             color = "forestgreen") +
  geom_hline(data=all.bench,
             aes(yintercept = lwr_bench/1000), lty=2,
             color = "darkred") +
  scale_linetype_manual(values=2, guide = "legend") +
  facet_wrap(~CU_f, scales = "free_y", labeller=CU_labeller_FR) +
  scale_x_continuous(expand = expansion(mult = c(0, .01))) +
  labs(y = "Géniteurs (en milliers)", x="Année", col="", fill="") +
  theme_sleek() +
  theme(legend.position = "bottom",
        strip.text = element_text(size=10),
        legend.text = element_text(size=10)) +
  scale_color_manual(values=HCR_cols, aesthetics = c("fill", "color"),
                     labels = c("IMEG" = "OEGP ",
                                "Moratorium" = "Moratoire ",
                                "No fishing" = "Aucune pêche"
                     ))
ggsave(here("csasdown/figure/french/spw_project.PNG"),
       height=650*2, width=810*2, units="px", dpi=240)



# Figure 22:

# Figure 23:performance metrics and status multipanel (all HCR excl. fixed ER, all PMs) ----
#metrics
perf.metrics <- perf.metrics |> left_join(HCR_lookup, by="HCR") |>
  mutate(HCR_name = factor(HCR_name, levels=HCR_lookup$HCR_name))

pm_plot <- perf.metrics |>
  filter(!(HCR %in% HCR_grps[["fixed"]])) |>
  filter(!(HCR %in% "realistic")) |>
  filter(!(metric %in% c("n.above.upr", "n.between.bench", "n.below.lwr", "n.above.reb", "n.extinct"))) |>
  mutate(metric_name = case_when(metric == "ER" ~ "Taux d’exploitation",
                                 metric == "cdn.harvest" ~ "Prise canadienne",
                                 metric == "pr.closed" ~ "Prop. d'années de fermeture de la pêche",
                                 metric == "escapement" ~ "Géniteurs",
                                 metric == "harvest" ~ "Pêche",
                                 .default = str_to_sentence(metric)))|>
  mutate(metric_name = factor(metric_name, levels=c("Géniteurs", "Pêche", "Prise canadienne", "Prop. d'années de fermeture de la pêche","Taux d’exploitation"))) |>
  ggplot() +
  geom_col(aes(x=HCR_name, y = median, fill=HCR_name)) +
  geom_segment(aes(x=HCR_name,
                   xend=HCR_name,
                   y=q_25, yend=q_75), col="grey30") +
  scale_fill_manual(values=HCR_cols) +
  facet_wrap(~metric_name, scales = "free_y", nrow=4) +
  theme_sleek() +
  scale_y_continuous() +
  theme(legend.position = c(0.8,0.15),
        axis.text.x = element_blank(),
        legend.title = element_blank()) +
  guides(fill=guide_legend(ncol=2, byrow=T)) +
  labs(x="", y="") +
  scale_color_manual(values=HCR_cols, aesthetics = c("fill", "color"),
                     labels = c("IMEG" = "OEGP ",
                                "Moratorium" = "Moratoire ",
                                "No fishing" = "Aucune pêche",
                                "Moratorium cap" = "Plafond du moratoire",
                                "PA Alternative" = "Solution de rechange de l’AP"))


# status
perf.status <- perf.metrics |>
  filter(metric %in% c("n.above.upr", "n.between.bench", "n.below.lwr", "n.above.reb", "n.extinct")) |>
  mutate(status = factor(gsub("^n ", "", gsub("\\.", " ", metric)),
                         levels=c("above reb", "above upr", "between bench", "below lwr", "extinct")))

status_plot <- perf.status |>
  filter(!(HCR %in% HCR_grps[["fixed"]])) |>
  ggplot(aes(x = HCR_name, y = mean, fill = status)) +
  geom_col() +
  scale_fill_discrete(type = c("pink3", "forestgreen", "darkorange", "darkred", "black"),
                      labels= c("above reb" = "Au-dessus de rét.",
                                "above upr" = "Au-dessus de sup.",
                                "between bench" = "Entre réf.",
                                "below lwr" = "Sous inf.",
                                "extinct" = "Disparue")) +
  scale_y_continuous(breaks = c(2,4,6,8)) +
  labs(x="Règle de contrôle des prises ", y = "Nombre moyen d’UC \n (1 000 simulations)", fill="État") +
  theme_sleek() +
  scale_x_discrete(
    labels = c(
      "IMEG" = "OEGP",
      "IMEG cap" = "Plafond de l’OEGP",
      "Moratorium" = "Moratoire ",
      "No fishing" = "Aucune pêche",
      "Moratorium cap" = "Plafond  moratoire",
      "PA Alternative" = "Solution  rechange"
    )
  )




cowplot::plot_grid(pm_plot, status_plot, nrow=2, labels="auto", rel_heights = c(1.5,1))

ggsave(here("csasdown/figure/french/perf_metrics_status.PNG"), height=900*2,
       width=800*2, units="px", dpi=240,bg = "white")

# Figure 22: fixed ER trade-off multipanel - CU ----

spwn_v_ER <- S.fwd %>% filter(HCR %in% HCR_grps[["fixed"]]) %>%
  group_by(HCR, CU_f) %>%
  summarize(mean_spwn = mean(S.50)) %>%
  mutate(ER = as.numeric(gsub("\\D", "", HCR))) %>%
  ggplot() +
  geom_point(aes(y=ER, x=mean_spwn/1000, col=CU_f), shape='circle', size=2, alpha=0.7) +
  scale_colour_viridis_d(labels= CU_prettyFrench) + scale_y_continuous(breaks = seq(0,100,20)) +
  theme_sleek() +
  theme(legend.position="none",
        axis.title = element_text(size=10)) +
  labs(x="Géniteurs (en milliers) ", y="Taux d’exploitation", col="Conservation Unit")

harv_v_ER <-
  H.fwd |> filter(HCR %in% HCR_grps[["fixed"]]) |>
  summarize(mean_harv = mean(H.50), .by=c(HCR, CU_f)) |>
  mutate(ER = as.numeric(gsub("\\D", "", HCR))) |>
  left_join(CU_name_lookup) |>
  arrange(ER) |>
  ggplot() +
  geom_point(aes(y=ER, x=mean_harv/1000, col=CU_pretty), shape='circle', size=2, alpha=0.7) +
  scale_colour_viridis_d(labels= CU_prettyFrench) +
  scale_y_continuous(breaks = seq(0,100,20)) +
  theme_sleek() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(size=10),
        axis.title = element_text(size=10)) +
  labs(x="Prélèvements UC (en milliers)", y="Taux d’exploitation", col="Unité de conservation")

status_ER <- perf.status %>% filter(HCR %in% HCR_grps[["fixed"]]) %>%
  mutate(ER = as.numeric(gsub("\\D", "", HCR))) %>%
  filter(ER != 100) %>%
  ggplot(aes(x=mean, y=factor(ER), fill=status)) +
  geom_col() +
  geom_vline(data=data.frame(x=seq(1:9)), aes(xintercept=x), col="white", linewidth=0.05) +
  scale_fill_discrete(type = c("pink3", "forestgreen", "darkorange", "darkred", "black"),
                      labels= c("above reb" = "Au-dessus de rét.",
                                "above upr" = "Au-dessus de sup.",
                                "between bench" = "Entre réf.",
                                "below lwr" = "Sous inf.",
                                "extinct" = "Disparue")) +
  scale_x_continuous(breaks = c(2,4,6,8)) +
  scale_y_discrete(breaks = seq(0,90,20)) +
  labs(y = "Taux d’exploitation", fill="", x="Nombre moyen d’UC (plus de 1 000 simulations)") +
  theme_sleek() +
  theme(legend.margin = margin(l=20, r=20),
        legend.text = element_text(size=10),
        axis.title = element_text(size=10))

b <- cowplot::plot_grid(spwn_v_ER, harv_v_ER, nrow=1, rel_widths=c(0.55,1), labels=c("b","c"), label_x = c(0,-0.015))
cowplot::plot_grid(status_ER, b, nrow=2, rel_heights=c(1,1), labels=c(NULL,"a"))

ggsave(here("csasdown/figure/french/fixed_ER_tradeoffs.PNG"), height=600*2,
       width=800*2, units="px", dpi=240)

# Figure A.1: GSI samples
# source gsi_wrangle.R first
g <- ggplot() +
  geom_vline(xintercept=c(180,210,240), color="light grey",lwd=0.25,lty=2) +
  geom_bar(data = bc, aes(x=as.numeric(julian), y=prop2),stat = "identity") +
  geom_bar(data = gsi2, aes(x=as.numeric(julian), y=(julian_prop2)*-1),
           fill="red", alpha=0.85, stat = "identity") +
  scale_x_continuous(limits=c(175,250), breaks = c(180,210,240)) +
  xlab("Jour de l’année") +
  ylab("Taille des échantillons de la remonte et de l’ISG") +
  facet_wrap(~year, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        strip.text = element_text(size=9),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        aspect.ratio=1,
        panel.spacing.x=unit(0.1, "lines"),
        panel.spacing.y=unit(0.3, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank()) +
  geom_text(data = gsi_count,
            mapping = aes(x = 237, y = -0.5, label = year_count, hjust = 1, vjust = 2),
            size=3)

png(here("csasdown/figure/french/gsi-run-samples.PNG"), width = 6, height = 8, units = "in", res = 600)
print(g)
dev.off()

# Figure B.1: fishwheel catchability
RR_pars <- rpt[["sdrpt"]]

fw_catch <- as.data.frame(RR_pars) |>
  filter(par=="lnqE_tg") |>
  mutate(mid = exp(val),
         lwr = exp(lCI),
         upr = exp(uCI),
         year = seq(1984,2006)) |>
  filter(year > 1984,
         year < 2005) |>
  select(year,mid,lwr,upr)

ggplot(fw_catch, aes(x = year, y = mid)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,position=position_dodge(0.9)) +
  theme_sleek() +
  labs(x = "Année", y = "Capturabilité des tourniquets à poissons")
ggsave(here("csasdown/figure/french/fishwheel-catchability.PNG"), height=4.25, width=8)

# Figure B.2-B.3: RR fits
# load run-reconstruction model fit
load(here("analysis/R/run-reconstructions/fittedMod/rpt.Rdata"))

plotFitI_FR(rpt, folder=here("csasdown/figure/french/"))

# Figure C.1: benchmark comparison
ref_points <- read.csv(here("analysis/data/generated/bench_par_table.csv"))
dem_ref_points <- read.csv(here("analysis/data/generated/demographic_parameters.csv"))

simple <- ref_points |>
  filter(bench.par %in% c("Sgen","Smsr.20", "Smsr.40","Smsy","Seq","Smsr")) |>
  mutate(median = X50.,
         lwr=X10.,
         upr=X90.,
         par=bench.par,
         model="spawners") |>
  select(CU,par,median,lwr,upr,model)

simple.1 <- simple |>
  filter(par == "Smsy") |>
  mutate(par = "Smsy.80",
         median = median*0.8,
         lwr=lwr*0.8,
         upr=upr*0.8) |>
  select(CU,par,median,lwr,upr,model)

demo.1 <- dem_ref_points |>
  filter(period == "recent",
         par == "Smsr") |>
  mutate(par = "Smsr.egg-mass",
         model="egg-mass",
         lwr=lower,
         upr=upper) |>
  select(CU,par,median,lwr,upr,model)

demo.2 <- demo.1 |>
  mutate(par = "Smsr.20.egg-mass",
         median = median*0.2,
         lwr=lwr*0.2,
         upr=upr*0.2) |>
  select(CU,par,median,lwr,upr,model)

demo.3 <- demo.1 |>
  mutate(par = "Smsr.40.egg-mass",
         median = median*0.4,
         lwr=lwr*0.4,
         upr=upr*0.4) |>
  select(CU,par,median,lwr,upr,model)

demo <- rbind(demo.1,demo.2,demo.3)

pars <- rbind(simple,simple.1,demo)

a <- ggplot(pars |>filter(
  par %in% c("Sgen","Smsr.20","Smsr.20.egg-mass")), aes(x = CU, y = median, fill = par)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin = lwr, ymax = upr,col = par), width = 0,position=position_dodge(0.9)) +
  theme_sleek() +
  scale_fill_manual(values = scales::hue_pal()(3),
                    labels=c(expression("G"["gén"]), expression("20%G"["NMR"]),
                             expression("20%G"["NMR, mo"])),
                    aesthetics=c("fill", "colour")) +
  theme(legend.position = c(0.8,0.825),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title=element_blank()) +
  labs(x = "Unité de conservation", y = "Géniteurs")


pars2 <- pars |>filter(
  par %in% c("Smsy.80","Smsr.40","Smsr.40.egg-mass"))
pars2$par2 <- factor(pars2$par,levels=c("Smsy.80","Smsr.40","Smsr.40.egg-mass"))

b <- ggplot(pars2, aes(x = CU, y = median, fill = par2)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin = lwr, ymax = upr,col = par2), width = 0,position=position_dodge(0.9)) +
  theme_sleek()  +
  scale_fill_manual(values = scales::hue_pal()(3),
                    labels=c(expression("80%G"["RMD"]), expression("40%G"["NMR"]),
                             expression("40%G"["NMR, em"])),
                    aesthetics=c("fill", "colour")) +
  theme(legend.position = c(0.8,0.825),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title=element_blank()) +
  labs(x = "Unité de conservation", y = "Géniteurs")

c <- pars |> filter(par %in% c("Seq","Smsr","Smsr.egg-mass")) |>
  ggplot(aes(x = CU, y = median, fill = par)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin = lwr, ymax = upr,col = par), width = 0, position=position_dodge(0.9)) +
  scale_fill_manual(values = scales::hue_pal()(3),
                    labels=c(expression("G"["eq"]), expression("G"["NMR"]),
                             expression("G"["NMR, mo"])),
                    aesthetics=c("fill", "colour")) +
  theme_sleek() +
  theme(legend.position = c(0.8,0.825),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.title=element_blank()) +
  labs(x = "Unité de conservation", y = "Géniteurs") +
  scale_x_discrete(
    labels = c(
      "Big.Salmon" = "Big Salmon",
      "MiddleYukonR.andtribs." = "Moyen Yukon et affluents",
      "NorthernYukonR.andtribs." = "Yukon Nord et affluents",
      "UpperYukonR." = "Cours supérieur du Yukon",
      "Whiteandtribs." = "White et affluents",
      "YukonR.Teslinheadwaters" = "Eaux d’amont du Yukon-de la Treslin"
    ))

cowplot::plot_grid(a, b, c, labels="auto", ncol=1,rel_heights=c(0.6,0.6,1))

ggsave(here("csasdown/figure/french/bench-compare.PNG"), width = 675*2, height = 900*2,
       units="px", dpi=240)

# Figure D.1: age comps by CU
eagle_age_sex_gen <- read.csv(here("analysis/data/raw/ASL_Eagle_2005-2024_geneticIDs.csv"))
gsi <- read.csv(here("analysis/data/raw/border-gsi-table-2024-update-full.csv"))

age <-eagle_age_sex_gen |>
  mutate(fish=Genetic.Sample.Number,
         year=sampleYear) |>
  filter(species == "Chinook",
         year>2008) |>
  select(year, fish, sexID, totalAge)

gsi_hProb <- gsi |>
  filter(prob>0.5,
         year>2008) |>
  select(year, fish, CU, prob)

gsi_age <- age |>
  left_join(gsi_hProb, by = c("year", "fish")) |>
  mutate(age = case_when(totalAge == 1.1 ~ 3,
                         totalAge == 1.2 ~ 4,
                         totalAge == 1.3 ~ 5,
                         totalAge == 1.4 ~ 6,
                         totalAge == 1.5 ~ 7,
                         totalAge == 2.2 ~ 5,
                         totalAge == 2.3 ~ 6,
                         totalAge == 2.4 ~ 7)) |>
  filter(age != 3)


cu_age_sex <- gsi_age |>
  group_by(year,CU,sexID) |>
  count(age) |>
  drop_na()

cu_age <- gsi_age |>
  group_by(year,CU) |>
  count(age) |>
  drop_na()|>
  mutate(year_count = sum(n),
         prop = n/year_count) |>
  drop_na()

ggplot(cu_age |> filter(!year %in% c(2010,2012,2013)), aes(x = CU, y = prop, fill=as.factor(age))) +
  geom_bar( stat = "identity") +
  theme_sleek() +
  facet_wrap(~year) +
  labs(fill = "Classe d’âge ") +
  theme(legend.position = c(0.7,0.06),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Unité de conservation", y = "Proportion")+
  scale_x_discrete(
    labels = c(
      "MidYR" = "MoyYuk",
      "Norden" = "Norden",
      "NYR" = "YukN",
      "UpperYR" = "YukSup"
    ))

my.ggsave(here("csasdown/figure/french/age-cu-by-yrs.PNG"), height=4.75, width=7)

# Figure D.3: harvest comps by location ----

# harvest by AK district and year
AK_dist_harv <- read.csv(here("analysis/data/raw/YkCk_Harvest_byDistrTypeStockAge.csv"))

# CDN harvest by district and year
CDN_AK_dist_harv <- AK_dist_harv |>
  filter(Stock == "Upper",
         District != 7) |>
  group_by(Year, District, Fishery) |>
  summarise(total_harvest = sum(Total)) |>
  filter(Year %in% c(2005,2006,2007,2008,2009,2010))


# harvest genetics by AK district and year
AK_dist_harv_genetics <- read.csv(here("analysis/data/raw/AK-harvest-genetics.csv")) |>
  group_by(Year, District, Fishery, Stock) |>
  summarise(avg_contr = mean(Contribution))|>
  na.omit()

AK_dist_harv_genetics_total_contr <- read.csv(here("analysis/data/raw/AK-harvest-genetics.csv")) |>
  group_by(Year, District, Fishery, Stock) |>
  summarise(avg_contr = mean(Contribution)) |>
  group_by(Year, District, Fishery) |>
  summarise(total_contr = sum(avg_contr)) |>
  na.omit()

AK_harv_genetics <- AK_dist_harv_genetics |>
  left_join(AK_dist_harv_genetics_total_contr, by=c("Year", "District", "Fishery")) |>
  mutate(contr = avg_contr/total_contr) |>
  select(Year, District, Fishery, Stock, contr) |>
  left_join(CDN_AK_dist_harv, by=c("Year", "District", "Fishery")) |>
  mutate(harvest = contr*total_harvest) |>
  group_by(Year, Stock) |>
  summarize(stock_harvest = sum(harvest)) |>
  mutate(harv_per_contr = stock_harvest/sum(stock_harvest))

# border comps by CU and year
CU_border_comps <- read.csv(here("analysis/data/generated/CU-border-comps.csv")) |>
  mutate(Stock = case_when(
    CU_f == "NorthernYukonR.andtribs." ~ "Frontière",
    CU_f == "Whiteandtribs." ~ "Frontière",
    CU_f == "Pelly" ~ "Pelly",
    CU_f == "Stewart" ~ "Pelly",
    CU_f == "Nordenskiold" ~ "Carmacks",
    CU_f == "MiddleYukonR.andtribs." ~ "Carmacks",
    CU_f == "YukonR.Teslinheadwaters" ~ "Carmacks",
    CU_f == "Big.Salmon" ~ "Carmacks",
    CU_f == "UpperYukonR." ~ "Takhini")) |>
  rename(Year=year) |>
  group_by(Year,Stock) |>
  summarize(RR_per_contr=(sum(contr)/100)) |>
  filter(Year %in% c(2005,2006,2007,2008,2009,2010))

fish_select_explore <- AK_harv_genetics |>
  left_join(CU_border_comps, by=c("Year","Stock")) |>
  select(!stock_harvest) |>
  rename(Harvest = harv_per_contr) |>
  rename(Border = RR_per_contr) |>
  pivot_longer(!c(Year,Stock), names_to = "Source", values_to = "Contribution" ) |>
  mutate(Stock_F = case_when(Stock == "Border" ~ "Frontière",
                             .default = Stock))

ggplot(fish_select_explore, aes(x = Stock_F, y = Contribution, fill = Source)) +
  geom_bar(position="dodge", stat = "identity") +
  theme_sleek() +
  labs(x = "Groupe d’UC ", y = "Contribution") +
  facet_wrap(~Year) +
  scale_fill_discrete(labels= c("Border" = "Frontière",
                                "Harvest" = "Pêche"))


my.ggsave(here("csasdown/figure/french/harbest_vs_border_composition.PNG"), height=4.25, width=8)

