document.addEventListener("DOMContentLoaded", function () {
  // Assign unique radio group names per quiz container
  var quizzes = document.querySelectorAll(".quiz-container");
  quizzes.forEach(function (quiz, i) {
    var radios = quiz.querySelectorAll('input[type="radio"]');
    var name = "quiz-" + i;
    radios.forEach(function (radio) {
      radio.setAttribute("name", name);
    });
  });

  // Handle radio button clicks for correct/wrong feedback
  document.addEventListener("change", function (e) {
    if (e.target.type !== "radio") return;
    var quiz = e.target.closest(".quiz-container");
    if (!quiz) return;

    // Clear previous feedback
    quiz.querySelectorAll(".quiz-option").forEach(function (label) {
      label.classList.remove("quiz-correct", "quiz-incorrect");
    });

    var label = e.target.closest(".quiz-option");
    if (!label) return;

    if (e.target.hasAttribute("data-correct")) {
      label.classList.add("quiz-correct");
    } else {
      label.classList.add("quiz-incorrect");
      // Also highlight the correct answer
      var correct = quiz.querySelector('input[data-correct="true"]');
      if (correct) {
        var correctLabel = correct.closest(".quiz-option");
        if (correctLabel) correctLabel.classList.add("quiz-correct");
      }
    }
  });
});
