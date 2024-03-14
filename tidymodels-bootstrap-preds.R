###
# Bootstrap CIs for tidymodels predictions
# Zane
# Thu Mar 14 09:28:44 2024
###

# First we need to get some data.
peng <- palmerpenguins::penguins |>
	tidyr::drop_na()

# Next set up a tidymodels fit
library(tidymodels)
peng_recp <- recipes::recipe(
	body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm + sex +
		species + island,
	data = peng
	) |>
	recipes::step_dummy(recipes::all_nominal_predictors())

prepped_data <-
	peng_recp |>
	recipes::prep() |>
	recipes::juice()

peng_spec <-
	# Decision tree with random hyperparameter choices that I just picked
	parsnip::decision_tree(
		tree_depth = 5L,
		min_n = 10L,
		cost_complexity = 0.01
	) |>
	# Use rpart with a continuous outcome (body mass)
	parsnip::set_mode("regression") |>
	parsnip::set_engine("rpart")

# Put it together into a workflow
peng_wf <-
	workflows::workflow() |>
	workflows::add_recipe(peng_recp) |>
	workflows::add_model(peng_spec)

# First basic fit
basic_fit <-
	peng_wf |>
	parsnip::fit(peng)

# Generate bootstraps
peng_boots <-
	peng |>
	rsample::bootstraps(times = 1000)

# Fit the model to each bootstrap analysis set
# peng_boots_res <-
# 	peng_wf |>
# 	tune::fit_resamples(
# 		resamples = peng_boots,
# 		metrics = yardstick::metric_set(yardstick::rmse),
# 		control = tune::control_resamples(
# 			verbose = TRUE,
# 			save_pred = TRUE,
# 			save_workflow = TRUE
# 		)
# 	)

# Apparently there's no way to make that function save all of the fitted
# workflows so we have to get those ourselves.
peng_boots_fits <- purrr::map(
	1:nrow(peng_boots),
	\(i) parsnip::fit(peng_wf, data = rsample::analysis(peng_boots$splits[[i]])),
	.progress = "Fitting to bootstraps"
)

# Now we use the original data (this could also be just the training
# data depending on how we construct resamples) and get the predictions using
# each of the bootstrap fits.
peng_boots_preds <- purrr::map(
	peng_boots_fits,
	\(fit) augment(fit, new_data = peng) |>
		# We need the row IDs for grouping later so add them now
		dplyr::mutate(row_id = dplyr::row_number()),
	.progress = "Getting predictions on full data set"
) |>
	# Bind them together
	rlang::set_names(peng_boots$id) |>
	purrr::list_rbind(names_to = "bootstrap")

# Now we group by row_id and get the percentiles over the bootstraps to get a
# bootstrap CI for each prediction
peng_boots_cis <-
	peng_boots_preds |>
	dplyr::group_by(row_id) |>
	dplyr::summarize(ggdist::mean_qi(.pred)) |>
	dplyr::select(
		lwr = ymin,
		upr = ymax
	)

# Now get the predictions for the point estimates on the full data set and then
# join those CIs back
preds_with_cis <-
	basic_fit |>
	augment(peng) |>
	dplyr::bind_cols(peng_boots_cis)

# Make the plot
ggplot2::theme_set(hgp::theme_ms())
preds_with_cis |>
	ggplot2::ggplot() +
	ggplot2::aes(
		x = body_mass_g,
		y = .pred,
		ymin = lwr,
		ymax = upr
	) +
	ggplot2::geom_abline(
		slope = 1, intercept = 0,
		color = "red", linetype = "dashed"
	) +
	ggplot2::geom_pointrange() +
	ggplot2::labs(
		x = "Observed",
		y = "Predicted (95% bootstrap CI; B = 1000)"
	)
