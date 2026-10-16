// DFA exercise: binary strings with an odd number of 1s.
// Rendered by js/dfa-builder.js; embedded with @dfa-exercise in notes/05-fsa.scrbl.
(window.DFA_PROBLEMS = window.DFA_PROBLEMS || {})["odd-ones"] = {
  alphabet: ["0", "1"],
  inLanguage: (s) => [...s].filter((c) => c === "1").length % 2 === 1,

  reference: {
    start: "E",
    finals: new Set(["O"]),
    delta: {
      E: { 0: "E", 1: "O" },
      O: { 0: "O", 1: "E" },
    },
  },
  minimalStates: 2,

  // A drawing of the reference DFA, in the builder's save format.
  solution: {
    nodeRadius: 32,
    nodes: [
      { x: 160, y: 170, text: "E", isAcceptState: false },
      { x: 400, y: 170, text: "O", isAcceptState: true },
    ],
    links: [
      { type: "StartLink", node: 0, deltaX: 0, deltaY: -80 },
      { type: "SelfLink", node: 0, text: "0", anchorAngle: Math.PI },
      { type: "SelfLink", node: 1, text: "0", anchorAngle: 0 },
      { type: "Link", nodeA: 0, nodeB: 1, text: "1", parallelPart: 0.5, perpendicularPart: -40, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 1, nodeB: 0, text: "1", parallelPart: 0.5, perpendicularPart: -40, lineAngleAdjust: 0 },
    ],
  },

  storageKey: "cmsc330-dfa-odd-ones",
  languageSummary: "the binary strings with an odd number of 1s",
  solutionNotes: {
    title: "One solution (the minimal DFA, 2 states)",
    intro: "Each state remembers whether we have seen an even or odd number of 1s so far:",
    states: [
      ["E", "an even number of 1s (the start: zero 1s is even)"],
      ["O", "an odd number of 1s. Accept"],
    ],
    outro: "Reading a 1 flips between E and O; reading a 0 changes nothing.",
  },
};
