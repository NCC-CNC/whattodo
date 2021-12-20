class ParameterSetting {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    status,
    value,
    min_value,
    max_value,
    step_value,
    hide,
    units
  ) {
    // class fields
    this.id = id;
    this.elementId = "setting-" + id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".parameter-setting-template")
        .content,
      true);
    this.name_el = this.el.querySelector(".name-label");
    this.ref_el = this.el.querySelector(".reference-label");
    this.status_el = this.el.querySelector(".status-checkbox");
    this.value_el = this.el.querySelector(".noUiSlider-widget");
    this.value_container_el = this.el.querySelector(".parameter-slider");
    this.previous_value = value;
    this.hide = hide;

    // local variables
    let that = this;

    // attach id to element
    this.el.querySelector(".solution-setting").id = this.elementId;

    // set initial values
    /// name
    this.name_el.innerText = name;
    /// value
    let slider_format = wNumb({decimals: 0, suffix: units});
    noUiSlider.create(this.value_el, {
      start: value,
      step: step_value,
      connect: "lower",
      tooltips: true,
      format: slider_format,
      range: {
        "min": min_value,
        "max": max_value
      }
    });
    /// status
    this.updateStatus(status);
    /// ref label
    this.ref_format = parameter_format(min_value, max_value);
    this.ref_el.innerText = this.ref_format.to(value);

    /// hide slider if needed
    if (status) {
      this.value_container_el.style.display = "block";
      this.ref_el.style.display = "inline";
    } else {
      this.value_container_el.style.display = "none";
      this.ref_el.style.display = "none";
    }

    // set listeners to update user interface
    /// enable/disable widget on click
    if (HTMLWidgets.shinyMode) {
      this.status_el.addEventListener("change", function () {
        //// set switch value
        let checked = this.checked;
        //// update slider
        if (checked) {
          that.value_el.noUiSlider.set(that.previous_value);
          that.ref_el.innerText =
            that.ref_format.to(slider_format.from(that.previous_value));
        } else {
          that.previous_value = that.value_el.noUiSlider.get();
          that.value_el.noUiSlider.set(min_value);
          that.ref_el.innerText = that.ref_format.to(min_value);
        }
        /// hide slider if needed
        if (that.hide) {
          if (checked) {
            that.value_container_el.style.display = "block";
            that.ref_el.style.display = "inline";
          } else {
            that.value_container_el.style.display = "none";
            that.ref_el.style.display = "none";
          }
        }
        //// update HTML styles
        let els =
          document.getElementById(that.elementId).querySelectorAll(
            ".disable-if-inactive");
        if (checked) {
          els.forEach((x) => x.removeAttribute("disabled"));
        } else {
          els.forEach((x) => x.setAttribute("disabled", ""));
        }
      });
    }

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// value
      this.value_el.noUiSlider.on("update", function (values, handle) {
        let v = slider_format.from(values[handle]);
        that.ref_el.innerText = that.ref_format.to(v);
        Shiny.setInputValue(manager, {
          id: id,
          setting: "value",
          value: v,
          type: "parameter"
        });
      });
      /// status
      this.status_el.addEventListener("change", function () {
        let checked = this.checked;
        Shiny.setInputValue(manager, {
          id: id,
          setting: "status",
          value: checked,
          type: "parameter"
        });
      });
    }
  }

  /* update methods */
  updateSetting(setting, value) {
    if (setting === "name") {
      this.updateName(value);
    } else if (setting === "status") {
      this.updateStatus(value);
    } else if (setting === "range") {
      this.updateRange(value);
    } else if (setting === "value") {
      this.updateValue(value);
    }
  }

  updateName(value) {
    this.name_el.innerText = value;
  }

  updateStatus(value) {
    // update HTML elements if needed
    if (this.status_el.checked !== value) {
      /// update switch
      this.status_el.checked = value;
      /// update slider
      if (value) {
        this.value_el.noUiSlider.set(this.previous_value);
        this.ref_el.innerText =
          this.ref_format.to(slider_format.from.from(this.previous_value));
      } else {
        this.previous_value = this.value_el.noUiSlider.get();
        this.value_el.noUiSlider.set(min_value);
        this.ref_el.innerText = this.ref_format.to(min_value);
      }
      /// hide slider if needed
      if (this.hide) {
        if (checked) {
          this.value_container_el.style.display = "block";
          this.ref_el.style.display = "inline";
        } else {
          this.value_container_el.style.display = "none";
          this.ref_el.style.display = "none";
        }
      }
    }
    // update HTML element styles
    let els = this.el.querySelectorAll(
        ".disable-if-inactive, .disable-if-inactive.icon i");
    if (value) {
      els.forEach((x) => x.removeAttribute("disabled"));
    } else {
      els.forEach((x) => x.setAttribute("disabled", ""));
    }
  }

  updateValue(value) {
    this.previous_value = value;
    this.value_el.noUiSlider.set(value);
    this.ref_el.innerText = this.ref_format.to(value);
  }

  updateRange(value) {
    if (value[0] > this.value_el.noUiSlider.get()) {
      this.value_el.noUiSlider.set(value[0]);
    }
    if (value[1] < this.value_el.noUiSlider.get()) {
      this.value_el.noUiSlider.set(value[1]);
    }
    this.ref_format = parameter_format(value[0], value[1]);
    this.value_el.noUiSlider.updateOptions({
      range: {
        "min": value[0],
        "max": value[1]
      }
    });
  }

  /* render method */
  render() {
    return this.el;
  }

};
