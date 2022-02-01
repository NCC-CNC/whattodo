// display text indicating the current amount of a feature
function current_label_text(prop, total, prefix, units) {
  let v1 = Math.round(prop * 100.0);
  let v2 = auto_round(prop * total);
  return `${prefix}: ${v1}% (${v2} ${units})`;
}

// display text indicating the goal for a feature
function goal_label_text(prop, total, prefix, units) {
  let v1 = Math.round(prop * 100.0);
  let v2 = auto_round(prop * total);
  return `${prefix}: ${v1}% (${v2} ${units})`;
}

// style current bar
function style_current_bar(d, prop, threshold = 0) {
  let w = prop;
  w = Math.max(w, threshold);
  d.style.width = (w * 100) + "%";
}

// parameter text
function parameter_format(min_value, max_value) {
  let num_format = wNumb({decimals: 2, thousand: ","});
  return {
    to: function (value) {
      let x1 = value - min_value;
      let x2 = max_value - min_value;
      let v = (x1 / x2) * 100;
      if ((v < 0) | Number.isNaN(v)) {
        v = 0;
      }
      return "(" + num_format.to(v) + "%)";
    }
  }
}
