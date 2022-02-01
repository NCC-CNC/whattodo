class Solution {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    parameters,
    statistics,
    theme_results,
    solution_color
  ) {
    // set fields
    this.id = id;
    this.name = name;
    this.solution_color = solution_color

    // parameters results
    this.parameters_el = document.createElement("div");
    this.parameters_el.classList.add("parameters-results");
    this.parameters_el.appendChild(
      newParameters(manager, parameters).render()
    );

    // statistics results
    this.statistics_el = document.createElement("div");
    this.statistics_el.classList.add("statistics-results");
    this.statistics_el.appendChild(
      newStatistics(manager, statistics).render()
    );

    // theme results
    this.themes_el = document.createElement("div");
    this.themes_el.classList.add("themes-results");
    theme_results.forEach((x) => {
      this.themes_el.appendChild(
        newThemeResults(manager, x, solution_color).render());
    });

  }

  /* render method */
  render_parameters(el) {
    el.appendChild(this.parameters_el);
  }

  render_statistics(el) {
    el.appendChild(this.statistics_el);
  }

  render_themes(el) {
    el.appendChild(this.themes_el);
  }

}
