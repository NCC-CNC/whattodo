function newParameterSetting(manager, x) {
  return new ParameterSetting(
    manager,
    x.id,
    x.name,
    x.status,
    x.value,
    x.min_value,
    x.max_value,
    x.step_value,
    x.hide,
    x.units,
    x.reference_value,
    x.reference_units
  );
}

function newThemeSetting(manager, x) {
  return new ThemeSetting(
    manager,
    x.id,
    x.name,
    x.feature_name,
    x.feature_id,
    x.feature_total_amount,
    x.feature_current_held,
    x.feature_min_goal,
    x.feature_max_goal,
    x.feature_goal,
    x.feature_step_goal,
    x.units || "units"
  );
}

function newWeightSetting(manager, x) {
  return new WeightSetting(
    manager,
    x.id,
    x.name,
    x.min_factor,
    x.max_factor,
    x.factor,
    x.step_factor,
  );
}
