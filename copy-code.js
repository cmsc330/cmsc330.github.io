document.addEventListener("DOMContentLoaded", function () {
  // Escaped so the glyphs survive however this file gets decoded.
  var COPY_GLYPH = "\uD83D\uDCCB"; // clipboard
  var DONE_GLYPH = "\u2713"; // check mark

  // Everything before a line that is exactly "[Output]" is the input the
  // student would type; ocaml-repl / rust-repl append the evaluated result
  // after that marker. Blocks without the marker (shell transcripts, file
  // listings, fancy-* snippets) are copied whole.
  function inputOnly(text) {
    var lines = text.split("\n");
    for (var i = 0; i < lines.length; i++) {
      if (lines[i].trim() === "[Output]") {
        lines = lines.slice(0, i);
        break;
      }
    }
    while (lines.length && lines[lines.length - 1].trim() === "") {
      lines.pop();
    }
    return lines.join("\n");
  }

  // navigator.clipboard is unavailable when the build output is previewed
  // over file://, so fall back to the hidden-textarea trick.
  function legacyCopy(text) {
    var ta = document.createElement("textarea");
    ta.value = text;
    ta.setAttribute("readonly", "");
    ta.style.position = "fixed";
    ta.style.top = "-1000px";
    ta.style.opacity = "0";
    document.body.appendChild(ta);
    ta.select();
    try {
      document.execCommand("copy");
    } catch (e) {
      /* nothing more we can do */
    }
    document.body.removeChild(ta);
  }

  function copyText(text) {
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).catch(function () {
        legacyCopy(text);
      });
    } else {
      legacyCopy(text);
    }
  }

  function makeButton(code) {
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "copy-code-btn";
    btn.title = "Copy code";
    btn.setAttribute("aria-label", "Copy code");
    btn.textContent = COPY_GLYPH;

    var timer = null;
    btn.addEventListener("click", function (e) {
      // Keep the click from toggling an enclosing <details>
      // (fancy-collapsible / accordion-section).
      e.preventDefault();
      e.stopPropagation();

      copyText(inputOnly(code.textContent));

      btn.textContent = DONE_GLYPH;
      btn.classList.add("copied");
      if (timer) {
        clearTimeout(timer);
      }
      timer = setTimeout(function () {
        btn.textContent = COPY_GLYPH;
        btn.classList.remove("copied");
        timer = null;
      }, 1200);
    });

    return btn;
  }

  var boxes = document.querySelectorAll("div.fancy-box");
  Array.prototype.forEach.call(boxes, function (box) {
    // fancy-quote reuses .fancy-box for prose; only code gets a button.
    var code = box.querySelector("pre code");
    if (!code) {
      return;
    }

    var btn = makeButton(code);
    var filebox = box.closest ? box.closest(".Rfilebox") : null;
    var title = filebox ? filebox.querySelector("p.Rfiletitle") : null;

    if (title) {
      // .Rfiletitle is already absolutely positioned in the top-right corner
      // of the (position: relative) .Rfilebox, and sits outside the box's
      // horizontal scroll container.
      title.appendChild(document.createTextNode(" "));
      title.appendChild(btn);
    } else {
      // Bare fancy-box (fancy-ocaml / fancy-c / fancy-rust): the box itself is
      // overflow-x: auto, so anchor the button to a wrapper instead.
      var wrap = document.createElement("div");
      wrap.className = "copy-wrap";
      box.parentNode.insertBefore(wrap, box);
      wrap.appendChild(box);
      wrap.appendChild(btn);
    }
  });
});
