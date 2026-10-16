// DFA exercise: binary strings that end with 00 followed by 11 (end in 0011).
// Rendered by js/dfa-builder.js; embedded with @dfa-exercise in notes/05-fsa.scrbl.
(window.DFA_PROBLEMS = window.DFA_PROBLEMS || {}).ends0011 = {
  alphabet: ["0", "1"],
  inLanguage: (s) => s.endsWith("0011"),

  reference: {
    start: "q0",
    finals: new Set(["q4"]),
    delta: {
      q0: { 0: "q1", 1: "q0" },
      q1: { 0: "q2", 1: "q0" },
      q2: { 0: "q2", 1: "q3" },
      q3: { 0: "q1", 1: "q4" },
      q4: { 0: "q1", 1: "q0" },
    },
  },
  minimalStates: 5,

  // A drawing of the reference DFA, in the builder's save format.
  solution: {
    nodeRadius: 32,
    nodes: [
      { x: 80, y: 200, text: "q0", isAcceptState: false },
      { x: 220, y: 200, text: "q1", isAcceptState: false },
      { x: 360, y: 200, text: "q2", isAcceptState: false },
      { x: 500, y: 200, text: "q3", isAcceptState: false },
      { x: 640, y: 200, text: "q4", isAcceptState: true },
    ],
    links: [
      { type: "StartLink", node: 0, deltaX: -75, deltaY: 0 },
      { type: "SelfLink", node: 0, text: "1", anchorAngle: -Math.PI / 2 },
      { type: "Link", nodeA: 0, nodeB: 1, text: "0", parallelPart: 0.5, perpendicularPart: -30, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 1, nodeB: 0, text: "1", parallelPart: 0.5, perpendicularPart: -30, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 1, nodeB: 2, text: "0", parallelPart: 0.5, perpendicularPart: 0, lineAngleAdjust: Math.PI },
      { type: "SelfLink", node: 2, text: "0", anchorAngle: -Math.PI / 2 },
      { type: "Link", nodeA: 2, nodeB: 3, text: "1", parallelPart: 0.5, perpendicularPart: 0, lineAngleAdjust: Math.PI },
      { type: "Link", nodeA: 3, nodeB: 1, text: "0", parallelPart: 0.5, perpendicularPart: -70, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 3, nodeB: 4, text: "1", parallelPart: 0.5, perpendicularPart: 0, lineAngleAdjust: Math.PI },
      { type: "Link", nodeA: 4, nodeB: 1, text: "0", parallelPart: 0.5, perpendicularPart: 145, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 4, nodeB: 0, text: "1", parallelPart: 0.5, perpendicularPart: -140, lineAngleAdjust: 0 },
    ],
  },

  storageKey: "cmsc330-dfa-ends0011",
  languageSummary: "the binary strings that end with 0011",
  solutionNotes: {
    title: "One solution (the minimal DFA, 5 states)",
    intro: "Each state remembers how much of 0011 the input currently ends with:",
    states: [
      ["q0", "nothing useful (the start, or we just read a 1 that broke the pattern)"],
      ["q1", "the input ends in a single 0"],
      ["q2", "it ends in 00"],
      ["q3", "it ends in 001"],
      ["q4", "it ends in 0011. Accept"],
    ],
    outro: "This is the same machine as “contains 0011”, except q4 is no longer a trap: the pattern must be at the very end, so after q4 a 0 (…00110) goes to q1 and a 1 (…00111) goes to q0.",
  },
};
