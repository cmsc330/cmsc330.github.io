// DFA exercise: binary strings with an odd number of 0s and an odd number of 1s.
// Rendered by js/dfa-builder.js; embedded with @dfa-exercise in notes/05-fsa.scrbl.
(window.DFA_PROBLEMS = window.DFA_PROBLEMS || {})["odd-zeros-odd-ones"] = {
  alphabet: ["0", "1"],
  inLanguage: (s) => {
    const zeros = [...s].filter((c) => c === "0").length;
    return zeros % 2 === 1 && (s.length - zeros) % 2 === 1;
  },

  // State names: parity of 0s, then parity of 1s (e = even, o = odd).
  reference: {
    start: "ee",
    finals: new Set(["oo"]),
    delta: {
      ee: { 0: "oe", 1: "eo" },
      oe: { 0: "ee", 1: "oo" },
      eo: { 0: "oo", 1: "ee" },
      oo: { 0: "eo", 1: "oe" },
    },
  },
  minimalStates: 4,

  // A drawing of the reference DFA, in the builder's save format.
  solution: {
    nodeRadius: 32,
    nodes: [
      { x: 140, y: 90, text: "ee", isAcceptState: false },
      { x: 420, y: 90, text: "oe", isAcceptState: false },
      { x: 140, y: 330, text: "eo", isAcceptState: false },
      { x: 420, y: 330, text: "oo", isAcceptState: true },
    ],
    links: [
      { type: "StartLink", node: 0, deltaX: -75, deltaY: 0 },
      { type: "Link", nodeA: 0, nodeB: 1, text: "0", parallelPart: 0.5, perpendicularPart: -25, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 1, nodeB: 0, text: "0", parallelPart: 0.5, perpendicularPart: -25, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 2, nodeB: 3, text: "0", parallelPart: 0.5, perpendicularPart: -25, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 3, nodeB: 2, text: "0", parallelPart: 0.5, perpendicularPart: -25, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 0, nodeB: 2, text: "1", parallelPart: 0.5, perpendicularPart: -25, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 2, nodeB: 0, text: "1", parallelPart: 0.5, perpendicularPart: -25, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 1, nodeB: 3, text: "1", parallelPart: 0.5, perpendicularPart: -25, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 3, nodeB: 1, text: "1", parallelPart: 0.5, perpendicularPart: -25, lineAngleAdjust: 0 },
    ],
  },

  storageKey: "cmsc330-dfa-odd-zeros-odd-ones",
  languageSummary: "the binary strings with an odd number of 0s and an odd number of 1s",
  solutionNotes: {
    title: "One solution (the minimal DFA, 4 states)",
    intro: "Each state remembers two things: whether we have seen an even or odd number of 0s, and of 1s (first letter = 0s, second = 1s):",
    states: [
      ["ee", "even 0s, even 1s (the start)"],
      ["oe", "odd 0s, even 1s"],
      ["eo", "even 0s, odd 1s"],
      ["oo", "odd 0s, odd 1s. Accept"],
    ],
    outro: "A 0 flips the first letter (move left–right); a 1 flips the second (move up–down).",
  },
};
