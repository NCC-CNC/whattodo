class ThemeSetting {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    feature_name,
    feature_id,
    feature_total_amount,
    feature_current_held,
    feature_min_goal,
    feature_max_goal,
    feature_goal,
    feature_limit_goal,
    feature_step_goal,
    units
  ) {
    // class fields
    this.id = id;
    this.elementId = "setting-" + id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".theme-setting-template")
        .content,
      true);
    this.name_el = this.el.querySelector(".name-label");
    this.goal_el = this.el.querySelector(".noUiSlider-widget");
    this.reset_el = this.el.querySelector(".reset-button");
    this.current_label_el = this.el.querySelector(".current-label");
    this.current_bar_el = this.el.querySelector(".current-bar");
    this.total = feature_total_amount;
    this.units = units;

    // local variables
    let that = this;
    let goal_label_el = this.el.querySelector(".slider-label");
    let goal_symbol_el = this.el.querySelector(".slider-symbol");

    // attach id to element
    this.el.querySelector(".solution-setting").id = this.elementId;

    // set initial values
    /// name
    this.name_el.innerText = name;
    /// goal
    noUiSlider.create(this.goal_el, {
      start: feature_goal,
      step: feature_step_goal,
      connect: "lower",
      range: {
        "min": feature_min_goal,
        "max": feature_max_goal
      }
    });
    /// current label
    this.current_label_el.innerText =
      current_label_text(
        feature_current_held, this.total, "Current", this.units);
    /// current bar width
    style_current_bar(this.current_bar_el, feature_current_held);

    // set listeners to update user interface
    if (HTMLWidgets.shinyMode) {
      /// enforce minimum limit
      this.goal_el.noUiSlider.on("change", function (values, handle) {
        if (values[handle] < that.limit) {
          that.goal_el.noUiSlider.set(that.limit);
        }
      });
      /// update goal label
      this.goal_el.noUiSlider.on("update", function (values, handle) {
        goal_label_el.innerText = goal_label_text(
          values[handle], that.total, "Goal", that.units);
      });
      /// reset button
      this.reset_el.addEventListener("click", function () {
        that.goal_el.noUiSlider.set(feature_goal);
      })
    }

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// goal
      this.goal_el.noUiSlider.on("update", function(values, handle) {
        let v = parseFloat(values[handle]);
        if (v >= feature_limit_goal) {
          Shiny.setInputValue(manager, {
            id: id,
            setting: "feature_goal",
            value: v,
            type: "theme"
          });
        }
      });
    }
  }

  /* update methods */
  updateSetting(setting, value) {
    if (setting === "name") {
      this.updateName(value);
    } else if (setting === "feature_goal") {
      this.updateFeatureGoal(value);
    } else if (setting === "feature_current") {
      this.updateFeatureCurrent(value);
    }
  }

  updateName(value) {
    this.name_el.innerText = value;
  }

  updateFeatureGoal(value) {
    this.goal_el.noUiSlider.set(value);
  }

  updateFeatureCurrent(value) {
    // current label
    this.current_label_el.innerText =
      current_label_text(value, this.total, "Current", this.units);
    // current bar width
    style_current_bar(this.current_bar_el, value);
  }

  /* render method */
  render() {
    return this.el;
  }

};
