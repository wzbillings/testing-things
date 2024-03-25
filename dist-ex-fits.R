###
# Simulating data for murphy visit slides
# need to move to BQ
# Zane
# 2024-03-24
###

set.seed(13123419)
N <- 100
dat_id <-
	tibble::tibble(
		id = 1:100,
		beta_0 = 3,
		beta_1 = -2,
		b0 = rnorm(N, 0, 0.5),
		b1 = rnorm(N, 0, 0.25),
	)

dat <-
	dat_id |>
	tidyr::expand_grid(
		d = seq(0, 1, 0.1)
	) |>
	dplyr::mutate(
		mu = (b0 + beta_0) + (b1 + beta_1) * d,
		z_star = rnorm(dplyr::n(), mu, 0.5),
		y_star = 5 * 2 ^ z_star,
		z = ifelse(z_star < 1, 1, floor(z_star)),
		y = 5 * 2 ^ z
	)
summary(dat)

plot(dat$d, dat$z_star)

fit_hier <- lme4::lmer(
	formula = z_star ~ 1 + d + (1 + d | id),
	data = dat
)

summary(fit_hier)

require(broom.mixed)
preds_hier <- broom::augment(fit_hier, newdata = dat)

ranefs_hier <- lme4::ranef(fit_hier)$id
plot(ranefs_hier$`(Intercept)`, dat_id$b0)
plot(ranefs_hier$d, dat_id$b1)

marg_hier <- broom::augment(
	fit_hier, newdata = dat, re.form = NA,
) |>
	dplyr::distinct(d, .fitted)

library(ggplot2)
theme_set(hgp::theme_ms())
ggplot() +
	geom_point(
		data = dat,
		aes(x = d, y = z),
		shape = 21,
		size = 1,
		position = position_jitter(width = 0.01, height = 0.1, seed = 98098)
	) +
	geom_line(
		data = preds_hier,
		mapping = aes(x = d, y = .fitted, group = id),
		linewidth = 0.5,
		alpha = 0.5,
		color = "darkgray"
	) +
	geom_line(
		data = marg_hier,
		mapping = aes(x = d, y = .fitted),
		linewidth = 1.25,
		alpha = 1,
		color = "red"
	) +
	labs(
		x = "Antigenic distance",
		y = "HAI titer"
	)

ggsave(
	here::here("hier.png"),
	width = 12,
	height = 10
)

fit_gam <- mgcv::gam(
	z ~ s(d, bs = "tp", k = 4),
	data = dat
)

plot(fit_gam)

preds_gam <- broom::augment(fit_gam, newdata = dat, unconditional = TRUE) |>
	dplyr::distinct(d, .fitted, .se.fit) |>
	dplyr::mutate(
		lwr = .fitted - 1.96 * .se.fit,
		upr = .fitted + 1.96 * .se.fit
	)

ggplot() +
	geom_point(
		data = dat,
		aes(x = d, y = z),
		shape = 21,
		size = 1,
		position = position_jitter(width = 0.01, height = 0.1, seed = 98098)
	) +
	geom_smooth(
		data = dat,
		aes(x = d, y = z),
		method = "lm",
		formula = y ~ x,
		alpha = 0.5
	) +
	geom_ribbon(
		data = preds_gam,
		mapping = aes(x = d, ymin = lwr, ymax = upr),
		fill = "gray",
		alpha = 0.5
	) +

	geom_line(
		data = preds_gam,
		mapping = aes(x = d, y = .fitted),
		linewidth = 1.25,
		alpha = 1,
		color = "red"
	) +
	labs(
		x = "Antigenic distance",
		y = "HAI titer"
	)

ggsave(
	here::here("gam.png"),
	width = 12,
	height = 10
)
