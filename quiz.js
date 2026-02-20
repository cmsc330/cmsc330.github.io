document.addEventListener("DOMContentLoaded", function () {
    document.querySelectorAll("details").forEach(function (details) {
        var summary = details.querySelector(":scope > summary");
        if (!summary || !summary.querySelector(".quiz-container")) return;

        // Prevent clicking the summary area from toggling details
        summary.addEventListener("click", function (e) {
            e.preventDefault();
        });

        // Stop option clicks from bubbling to summary (so they aren't blocked by the above)
        summary.querySelectorAll("label.quiz-option").forEach(function (label) {
            label.addEventListener("click", function (e) {
                e.stopPropagation();
            });
        });
    });

    document.addEventListener("change", function (e) {
        if (e.target.type !== "radio") return;
        var quiz = e.target.closest(".quiz-container");
        if (!quiz) return;

        quiz.querySelectorAll(".quiz-option").forEach(function (label) {
            label.classList.remove("quiz-correct", "quiz-incorrect");
        });

        var label = e.target.closest(".quiz-option");
        if (!label) return;

        if (e.target.hasAttribute("data-correct")) {
            label.classList.add("quiz-correct");
        } else {
            label.classList.add("quiz-incorrect");
            var correct = quiz.querySelector('input[data-correct="true"]');
            if (correct) {
                var correctLabel = correct.closest(".quiz-option");
                if (correctLabel) correctLabel.classList.add("quiz-correct");
            }
        }

        var details = quiz.closest("details");
        if (details) details.open = true;
    });
});