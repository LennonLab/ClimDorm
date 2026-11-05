
library(ggplot2)
library(patchwork)
library(dplyr)
library(tibble)
library(scales)

# Time in years
hour <- 1/(24*365.25)
day <- 1/365.25
week <- 7*day
month <- 1/12

endurance <- tribble(
  ~taxon, ~min_yr, ~max_yr,
  "Bacteria", 1/60*hour, 1e6,
  "Plant seeds", day, 2000,
  "Daphnia ephippia", week, 700,
  "Bdelloid rotifers", week, 24000,
  "Yucca moth", week, 30,
  "Frogs", week, 2,
  "African lungfish", week, 7,
  "Birds", hour, week,
  "Mammals", hour, 1,
  "Diatoms", NA_real_, 7000,
  "Dinoflagellates", NA_real_, 100
)

adversity <- tribble(
  ~event, ~min_yr, ~max_yr, ~recurrence,
  "Night", hour, 12*hour, "Daily",
  "Winter / dry season", 2*month, 6*month, "Yearly",
  "ENSO", 6*month, 2, "Every 2–7 years",
  "Volcanic winter", 2, 4, "Stochastic",
  "Impact winter (K–Pg)", 2, 30, "Stochastic",
  "Millennial events (YD, D–O)", 1000, 3000, "Millennial"
)

write.csv(endurance, "endurance_data.csv", row.names=FALSE, na="")
write.csv(adversity, "adversity_data.csv", row.names=FALSE, na="")

axis_breaks <- 10^seq(-6, 7, by=1)
axis_labels <- function(x) {
  parse(text=paste0("10^", round(log10(x))))
}

common <- theme_minimal(base_size=11) +
  theme(
    panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank(),
    panel.grid.major.x=element_line(color="#e6e8eb", linewidth=.3),
    plot.title=element_text(face="bold", size=13),
    axis.title.y=element_blank(),
    axis.text.y=element_text(color="#30343b", size=9),
    plot.margin=margin(10, 18, 6, 12)
  )

# Dormancy endurance
endurance <- endurance %>%
  mutate(taxon=factor(taxon, levels=rev(taxon)))

p1 <- ggplot(endurance, aes(y=taxon)) +
  annotate(
    "rect", xmin=1, xmax=10, ymin=-Inf, ymax=Inf,
    fill="#dcefe8", alpha=.65
  ) +
  geom_segment(
    data=filter(endurance, !is.na(min_yr)),
    aes(x=min_yr, xend=max_yr, yend=taxon),
    linewidth=4, color="#2876d0", lineend="round"
  ) +
  geom_point(
    data=filter(endurance, is.na(min_yr)),
    aes(x=max_yr),
    color="#2876d0", size=2.7, shape=1, stroke=1.2
  ) +
  scale_x_log10(
    limits=c(1e-6, 1e7),
    breaks=axis_breaks,
    labels=axis_labels
  ) +
  labs(
    title="How long can organisms persist? — dormancy endurance",
    x=NULL
  ) +
  common +
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )

# Adverse duration
adversity <- adversity %>%
  mutate(event=factor(event, levels=rev(event)))

p2 <- ggplot(adversity, aes(y=event)) +
  annotate(
    "rect", xmin=1, xmax=10, ymin=-Inf, ymax=Inf,
    fill="#dcefe8", alpha=.65
  ) +
  geom_segment(
    aes(x=min_yr, xend=max_yr, yend=event),
    linewidth=4, color="#ec704c", lineend="round"
  ) +
  scale_x_log10(
    limits=c(1e-6, 1e7),
    breaks=axis_breaks,
    labels=axis_labels
  ) +
  labs(
    title="How long does the bad spell last? — adverse-phase duration",
    x="Duration (years; logarithmic scale)"
  ) +
  common

# Combined figure
fig <- (p1 / p2) +
  plot_layout(heights=c(1.4, 1)) +
  plot_annotation(
    title="Clipping has a budget: endurance vs adversity on one time axis",
    caption="Approximate ranges; open circles indicate unknown lower bounds."
  ) &
  theme(
    plot.background=element_rect(fill="white", color=NA)
  )

ggsave("dormancy_vs_adversity.pdf", fig, width=12, height=9)
ggsave("dormancy_vs_adversity.png", fig, width=12, height=9, dpi=300)

print(fig)

