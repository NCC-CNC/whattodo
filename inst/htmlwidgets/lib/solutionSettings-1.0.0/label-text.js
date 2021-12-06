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
