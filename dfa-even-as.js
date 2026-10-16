// DFA exercise: strings over {a, b} with an even number of a's (any number of b's).
// Rendered by js/dfa-builder.js; embedded with @dfa-exercise in notes/05-fsa.scrbl.
(window.DFA_PROBLEMS = window.DFA_PROBLEMS || {})["even-as"] = {
  alphabet: ["a", "b"],
  inLanguage: (s) => [...s].filter((c) => c === "a").length % 2 === 0,

  reference: {
    start: "E",
    finals: new Set(["E"]),
    delta: {
      E: { a: "O", b: "E" },
      O: { a: "E", b: "O" },
    },
  },
  minimalStates: 2,

  // A drawing of the reference DFA, in the builder's save format.
  solution: {
    nodeRadius: 32,
    nodes: [
      { x: 160, y: 170, text: "E", isAcceptState: true },
      { x: 400, y: 170, text: "O", isAcceptState: false },
    ],
    links: [
      { type: "StartLink", node: 0, deltaX: 0, deltaY: -80 },
      { type: "SelfLink", node: 0, text: "b", anchorAngle: Math.PI },
      { type: "SelfLink", node: 1, text: "b", anchorAngle: 0 },
      { type: "Link", nodeA: 0, nodeB: 1, text: "a", parallelPart: 0.5, perpendicularPart: -40, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 1, nodeB: 0, text: "a", parallelPart: 0.5, perpendicularPart: -40, lineAngleAdjust: 0 },
    ],
  },

  storageKey: "cmsc330-dfa-even-as",
  languageSummary: "the strings over {a, b} with an even number of a's",
  solutionNotes: {
    title: "One solution (the minimal DFA, 2 states)",
    intro: "Each state remembers whether we have seen an even or odd number of a's so far:",
    states: [
      ["E", "an even number of a's. This is the start (zero a's is even), and it accepts"],
      ["O", "an odd number of a's"],
    ],
    outro: "Reading an a flips between E and O; reading a b changes nothing.",
  },
};
