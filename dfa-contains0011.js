// DFA exercise: binary strings containing 00 followed by 11 (the substring 0011).
// Rendered by js/dfa-builder.js; embedded with @dfa-exercise in notes/05-fsa.scrbl.
(window.DFA_PROBLEMS = window.DFA_PROBLEMS || {}).contains0011 = {
  alphabet: ["0", "1"],
  inLanguage: (s) => s.includes("0011"),

  reference: {
    start: "q0",
    finals: new Set(["q4"]),
    delta: {
      q0: { 0: "q1", 1: "q0" },
      q1: { 0: "q2", 1: "q0" },
      q2: { 0: "q2", 1: "q3" },
      q3: { 0: "q1", 1: "q4" },
      q4: { 0: "q4", 1: "q4" },
    },
  },
  minimalStates: 5,

  // A drawing of the reference DFA, in the builder's save format.
  solution: {
    nodeRadius: 32,
    nodes: [
      { x: 80, y: 180, text: "q0", isAcceptState: false },
      { x: 220, y: 180, text: "q1", isAcceptState: false },
      { x: 360, y: 180, text: "q2", isAcceptState: false },
      { x: 500, y: 180, text: "q3", isAcceptState: false },
      { x: 640, y: 180, text: "q4", isAcceptState: true },
    ],
    links: [
      { type: "StartLink", node: 0, deltaX: -75, deltaY: 0 },
      { type: "SelfLink", node: 0, text: "1", anchorAngle: -Math.PI / 2 },
      { type: "Link", nodeA: 0, nodeB: 1, text: "0", parallelPart: 0.5, perpendicularPart: -30, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 1, nodeB: 0, text: "1", parallelPart: 0.5, perpendicularPart: -30, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 1, nodeB: 2, text: "0", parallelPart: 0.5, perpendicularPart: 0, lineAngleAdjust: Math.PI },
      { type: "SelfLink", node: 2, text: "0", anchorAngle: -Math.PI / 2 },
      { type: "Link", nodeA: 2, nodeB: 3, text: "1", parallelPart: 0.5, perpendicularPart: 0, lineAngleAdjust: Math.PI },
      { type: "Link", nodeA: 3, nodeB: 1, text: "0", parallelPart: 0.5, perpendicularPart: -80, lineAngleAdjust: 0 },
      { type: "Link", nodeA: 3, nodeB: 4, text: "1", parallelPart: 0.5, perpendicularPart: 0, lineAngleAdjust: Math.PI },
      { type: "SelfLink", node: 4, text: "0,1", anchorAngle: -Math.PI / 2 },
    ],
  },

  storageKey: "cmsc330-dfa-contains0011",
  languageSummary: "the binary strings that contain 0011",
  solutionNotes: {
    title: "One solution (the minimal DFA, 5 states)",
    intro: "Each state remembers how much of 0011 we have matched at the end of the input so far:",
    states: [
      ["q0", "nothing useful yet (the start, or we just read a 1 that broke the pattern)"],
      ["q1", "the input ends in a single 0"],
      ["q2", "it ends in 00. More 0s keep us here: the last two symbols are still 00"],
      ["q3", "it ends in 001"],
      ["q4", "we have seen 0011. Accept, and stay here whatever comes next"],
    ],
    outro: "From q3, a 0 gives …0010, which ends in a single 0, so we go back to q1, not q0.",
  },
};
