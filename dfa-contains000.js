// DFA exercise: binary strings containing the substring 000.
// Rendered by js/dfa-builder.js; embedded with @dfa-exercise in notes/05-fsa.scrbl.
(window.DFA_PROBLEMS = window.DFA_PROBLEMS || {}).contains000 = {
  alphabet: ["0", "1"],
  inLanguage: (s) => s.includes("000"),

  reference: {
    start: "0",
    finals: new Set(["3"]),
    delta: {
      0: { 0: "1", 1: "0" },
      1: { 0: "2", 1: "0" },
      2: { 0: "3", 1: "0" },
      3: { 0: "3", 1: "3" },
    },
  },
  minimalStates: 4,

  // A drawing of the reference DFA, in the builder's save format.
  solution: {
    nodeRadius: 32,
    nodes: [
      { x: 80, y: 170, text: "0", isAcceptState: false },
      { x: 230, y: 170, text: "1", isAcceptState: false },
      { x: 380, y: 170, text: "2", isAcceptState: false },
      { x: 530, y: 170, text: "3", isAcceptState: true },
    ],
    links: [
      { type: "StartLink", node: 0, deltaX: -75, deltaY: 0 },
      { type: "Link", nodeA: 0, nodeB: 1, text: "0", parallelPart: 0.5, perpendicularPart: 0, lineAngleAdjust: Math.PI },
      { type: "Link", nodeA: 1, nodeB: 2, text: "0", parallelPart: 0.5, perpendicularPart: 0, lineAngleAdjust: Math.PI },
      { type: "Link", nodeA: 2, nodeB: 3, text: "0", parallelPart: 0.5, perpendicularPart: 0, lineAngleAdjust: Math.PI },
      { type: "SelfLink", node: 0, text: "1", anchorAngle: -Math.PI / 2 },
      { type: "SelfLink", node: 3, text: "0,1", anchorAngle: -Math.PI / 2 },
      { type: "Link", nodeA: 1, nodeB: 0, text: "1", parallelPart: 0.5, perpendicularPart: -45, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 2, nodeB: 0, text: "1", parallelPart: 0.5, perpendicularPart: -95, lineAngleAdjust: 0 },
    ],
  },

  storageKey: "cmsc330-dfa-contains000",
  languageSummary: "the binary strings that contain 000",
  solutionNotes: {
    title: "One solution (the minimal DFA, 4 states)",
    intro: "Each state remembers how many 0s in a row we have just seen:",
    states: [
      ["0", "no trailing 0s yet (the start, or we just read a 1)"],
      ["1", "the input so far ends in exactly one 0"],
      ["2", "it ends in exactly two 0s"],
      ["3", "we have seen 000. Accept, and stay here whatever comes next"],
    ],
    outro: "A 1 in states 0–2 breaks the run, so it sends us back to state 0.",
  },
};
