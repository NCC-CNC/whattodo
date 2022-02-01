class WeightSetting {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    min_factor,
    max_factor,
    factor,
    step_factor
  ) {
    // class fields
    this.id = id;
    this.elementId = "setting-" + id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".weight-setting-template")
        .content,
      true);
    this.name_el = this.el.querySelector(".name-label");
    this.factor_el = this.el.querySelector(".noUiSlider-widget");

    // local variables
    let that = this;

    // attach id to element
    this.el.querySelector(".solution-setting").id = this.elementId;

    // set initial values
    /// name
    this.name_el.innerText = name;
    /// factor
    noUiSlider.create(this.factor_el, {
      start: factor,
      step: step_factor,
      connect: "lower",
      tooltips: true,
      format: wNumb({decimals: 0}),
      range: {
        "min": min_factor,
        "max": max_factor
      }
    });

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// factor
      this.factor_el.noUiSlider.on("update", function (values, handle) {
        Shiny.setInputValue(manager, {
          id: id,
          setting: "factor",
          value: parseFloat(values[handle]),
          type: "weight"
        });
      });
    }
  }

  /* update methods */
  updateSetting(setting, value) {
    if (setting === "name") {
      this.updateName(value);
    } else if (setting === "factor") {
      this.updateFactor(value);
    }
  }

  /* update name */
  updateName(value) {
    this.name_el.innerText = value;
  }

  updateFactor(value) {
    this.factor_el.noUiSlider.set(value);
  }

  /* render method */
  render() {
    return this.el;
  }

};
