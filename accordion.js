document.addEventListener("DOMContentLoaded", function () {
  var accordions = document.querySelectorAll(".accordion-section");
  accordions.forEach(function (details) {
    // Accordion behavior: close others when one opens
    details.addEventListener("toggle", function () {
      if (details.open) {
        accordions.forEach(function (other) {
          if (other !== details && other.open) {
            other.open = false;
          }
        });
      }
    });

    // Display question count in summary
    var summary = details.querySelector("summary");
    if (summary) {
      var count = details.querySelectorAll(":scope > details").length;
      var span = document.createElement("span");
      span.className = "question-count";
      span.textContent = " (" + count + " question" + (count !== 1 ? "s" : "") + ")";
      summary.appendChild(span);
    }
  });
});
