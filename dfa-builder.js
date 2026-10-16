// Interactive DFA builder, shared by every DFA exercise in the notes.
//
// Each exercise is a <div class="dfa-builder" data-problem="NAME"> (built from
// html/dfa-builder.html by dfa-exercise in fancyverb.rkt) plus a problem file
// js/dfa-NAME.js that registers its definition:
//
//   (window.DFA_PROBLEMS = window.DFA_PROBLEMS || {}).NAME = {
//     alphabet, inLanguage, reference, minimalStates, solution,
//     storageKey, languageSummary, solutionNotes,
//   };
//
// Elements inside a builder are found by data-role, so several builders can
// share one page. Scribble loads scripts in <head>, so we wait for the DOM.
(() => {
  "use strict";

  // ---------------------------------------------------------------------------
  // Geometry and drawing primitives (adapted from Evan Wallace's FSM Designer)
  // ---------------------------------------------------------------------------

  const snapToPadding = 6;
  const hitTargetPadding = 6;
  const smallStateSize = 24;
  const largeStateSize = 32;

  function drawText(c, text, x, y, angleOrNull, isSelected, nodeRadius, showCaret) {
    c.font = '20px "Times New Roman", serif';
    let width = c.measureText(text).width;

    // Shrink long names so they stay inside the state
    if (width > nodeRadius + 16) {
      let px = 20 - Math.floor((width - nodeRadius) / 8);
      if (px < 10) px = 10;
      c.font = px + 'px "Times New Roman", serif';
      width = c.measureText(text).width;
    }

    x -= width / 2;

    // Position the text intelligently if given an angle
    if (angleOrNull != null) {
      const cos = Math.cos(angleOrNull);
      const sin = Math.sin(angleOrNull);
      const cornerPointX = (width / 2 + 5) * (cos > 0 ? 1 : -1);
      const cornerPointY = (10 + 5) * (sin > 0 ? 1 : -1);
      const slide =
        sin * Math.pow(Math.abs(sin), 40) * cornerPointX -
        cos * Math.pow(Math.abs(cos), 10) * cornerPointY;
      x += cornerPointX - sin * slide;
      y += cornerPointY + cos * slide;
    }

    x = Math.round(x);
    y = Math.round(y);
    c.fillText(text, x, y + 6);
    if (isSelected && showCaret) {
      x += width;
      c.beginPath();
      c.moveTo(x, y - 10);
      c.lineTo(x, y + 10);
      c.stroke();
    }
  }

  function drawArrow(c, x, y, angle, big = false) {
    const dx = Math.cos(angle);
    const dy = Math.sin(angle);
    const m = big ? 1.6 : 1;
    c.beginPath();
    c.moveTo(x, y);
    c.lineTo(x - 10 * m * dx + 5 * m * dy, y - 10 * m * dy - 5 * m * dx);
    c.lineTo(x - 10 * m * dx - 5 * m * dy, y - 10 * m * dy + 5 * m * dx);
    c.fill();
  }

  function det(a, b, c, d, e, f, g, h, i) {
    return a * e * i + b * f * g + c * d * h - a * f * h - b * d * i - c * e * g;
  }

  function circleFromThreePoints(x1, y1, x2, y2, x3, y3) {
    const a = det(x1, y1, 1, x2, y2, 1, x3, y3, 1);
    const bx = -det(x1 * x1 + y1 * y1, y1, 1, x2 * x2 + y2 * y2, y2, 1, x3 * x3 + y3 * y3, y3, 1);
    const by = det(x1 * x1 + y1 * y1, x1, 1, x2 * x2 + y2 * y2, x2, 1, x3 * x3 + y3 * y3, x3, 1);
    const c = -det(x1 * x1 + y1 * y1, x1, y1, x2 * x2 + y2 * y2, x2, y2, x3 * x3 + y3 * y3, x3, y3);
    return {
      x: -bx / (2 * a),
      y: -by / (2 * a),
      radius: Math.sqrt(bx * bx + by * by - 4 * a * c) / (2 * Math.abs(a)),
    };
  }

  function segmentContains(s, x, y) {
    const dx = s.endX - s.startX;
    const dy = s.endY - s.startY;
    const length = Math.sqrt(dx * dx + dy * dy);
    const percent = (dx * (x - s.startX) + dy * (y - s.startY)) / (length * length);
    const distance = (dx * (y - s.startY) - dy * (x - s.startX)) / length;
    return percent > 0 && percent < 1 && Math.abs(distance) < hitTargetPadding;
  }

  class Node {
    constructor(x, y) {
      this.x = x;
      this.y = y;
      this.mouseOffsetX = 0;
      this.mouseOffsetY = 0;
      this.isAcceptState = false;
      this.text = "";
    }
    setMouseStart(x, y) {
      this.mouseOffsetX = this.x - x;
      this.mouseOffsetY = this.y - y;
    }
    setAnchorPoint(x, y) {
      this.x = x + this.mouseOffsetX;
      this.y = y + this.mouseOffsetY;
    }
    draw(c, isSelected, r, showCaret) {
      c.beginPath();
      c.arc(this.x, this.y, r, 0, 2 * Math.PI, false);
      c.stroke();
      drawText(c, this.text, this.x, this.y, null, isSelected, r, showCaret);
      if (this.isAcceptState) {
        c.beginPath();
        c.arc(this.x, this.y, r - 6, 0, 2 * Math.PI, false);
        c.stroke();
      }
    }
    closestPointOnCircle(x, y, r) {
      const dx = x - this.x;
      const dy = y - this.y;
      const scale = Math.sqrt(dx * dx + dy * dy) || 1;
      return { x: this.x + (dx * r) / scale, y: this.y + (dy * r) / scale };
    }
    containsPoint(x, y, r) {
      const R = r + hitTargetPadding;
      return (x - this.x) ** 2 + (y - this.y) ** 2 < R * R;
    }
  }

  class Link {
    constructor(a, b) {
      this.nodeA = a;
      this.nodeB = b;
      this.text = "";
      this.lineAngleAdjust = 0; // added to the text angle when the link is straight
      this.parallelPart = 0.5; // fraction of the way from nodeA to nodeB
      this.perpendicularPart = 0; // pixels away from the line between nodeA and nodeB
    }
    getAnchorPoint() {
      const dx = this.nodeB.x - this.nodeA.x;
      const dy = this.nodeB.y - this.nodeA.y;
      const scale = Math.sqrt(dx * dx + dy * dy);
      return {
        x: this.nodeA.x + dx * this.parallelPart - (dy * this.perpendicularPart) / scale,
        y: this.nodeA.y + dy * this.parallelPart + (dx * this.perpendicularPart) / scale,
      };
    }
    setAnchorPoint(x, y) {
      const dx = this.nodeB.x - this.nodeA.x;
      const dy = this.nodeB.y - this.nodeA.y;
      const scale = Math.sqrt(dx * dx + dy * dy);
      this.parallelPart = (dx * (x - this.nodeA.x) + dy * (y - this.nodeA.y)) / (scale * scale);
      this.perpendicularPart = (dx * (y - this.nodeA.y) - dy * (x - this.nodeA.x)) / scale;
      // snap to a straight line
      if (this.parallelPart > 0 && this.parallelPart < 1 && Math.abs(this.perpendicularPart) < snapToPadding) {
        this.lineAngleAdjust = (this.perpendicularPart < 0) * Math.PI;
        this.perpendicularPart = 0;
      }
    }
    getEndPointsAndCircle(r) {
      if (this.perpendicularPart === 0) {
        const midX = (this.nodeA.x + this.nodeB.x) / 2;
        const midY = (this.nodeA.y + this.nodeB.y) / 2;
        const start = this.nodeA.closestPointOnCircle(midX, midY, r);
        const end = this.nodeB.closestPointOnCircle(midX, midY, r);
        return { hasCircle: false, startX: start.x, startY: start.y, endX: end.x, endY: end.y };
      }
      const anchor = this.getAnchorPoint();
      const circle = circleFromThreePoints(this.nodeA.x, this.nodeA.y, this.nodeB.x, this.nodeB.y, anchor.x, anchor.y);
      const isReversed = this.perpendicularPart > 0;
      const reverseScale = isReversed ? 1 : -1;
      const startAngle = Math.atan2(this.nodeA.y - circle.y, this.nodeA.x - circle.x) - (reverseScale * r) / circle.radius;
      const endAngle = Math.atan2(this.nodeB.y - circle.y, this.nodeB.x - circle.x) + (reverseScale * r) / circle.radius;
      return {
        hasCircle: true,
        startX: circle.x + circle.radius * Math.cos(startAngle),
        startY: circle.y + circle.radius * Math.sin(startAngle),
        endX: circle.x + circle.radius * Math.cos(endAngle),
        endY: circle.y + circle.radius * Math.sin(endAngle),
        startAngle,
        endAngle,
        circleX: circle.x,
        circleY: circle.y,
        circleRadius: circle.radius,
        reverseScale,
        isReversed,
      };
    }
    draw(c, isSelected, r, showCaret) {
      const s = this.getEndPointsAndCircle(r);
      c.beginPath();
      if (s.hasCircle) {
        c.arc(s.circleX, s.circleY, s.circleRadius, s.startAngle, s.endAngle, s.isReversed);
      } else {
        c.moveTo(s.startX, s.startY);
        c.lineTo(s.endX, s.endY);
      }
      c.stroke();

      if (s.hasCircle) {
        drawArrow(c, s.endX, s.endY, s.endAngle - s.reverseScale * (Math.PI / 2));
        let startAngle = s.startAngle;
        let endAngle = s.endAngle;
        if (endAngle < startAngle) endAngle += Math.PI * 2;
        const textAngle = (startAngle + endAngle) / 2 + s.isReversed * Math.PI;
        const textX = s.circleX + s.circleRadius * Math.cos(textAngle);
        const textY = s.circleY + s.circleRadius * Math.sin(textAngle);
        drawText(c, this.text, textX, textY, textAngle, isSelected, r, showCaret);
      } else {
        drawArrow(c, s.endX, s.endY, Math.atan2(s.endY - s.startY, s.endX - s.startX));
        const textX = (s.startX + s.endX) / 2;
        const textY = (s.startY + s.endY) / 2;
        const textAngle = Math.atan2(s.endX - s.startX, s.startY - s.endY);
        drawText(c, this.text, textX, textY, textAngle + this.lineAngleAdjust, isSelected, r, showCaret);
      }
    }
    containsPoint(x, y, r) {
      const s = this.getEndPointsAndCircle(r);
      if (!s.hasCircle) return segmentContains(s, x, y);
      const dx = x - s.circleX;
      const dy = y - s.circleY;
      const distance = Math.sqrt(dx * dx + dy * dy) - s.circleRadius;
      if (Math.abs(distance) >= hitTargetPadding) return false;
      let angle = Math.atan2(dy, dx);
      let startAngle = s.startAngle;
      let endAngle = s.endAngle;
      if (s.isReversed) [startAngle, endAngle] = [endAngle, startAngle];
      if (endAngle < startAngle) endAngle += Math.PI * 2;
      if (angle < startAngle) angle += Math.PI * 2;
      else if (angle > endAngle) angle -= Math.PI * 2;
      return angle > startAngle && angle < endAngle;
    }
  }

  class SelfLink {
    constructor(node, mouse) {
      this.node = node;
      this.anchorAngle = 0;
      this.mouseOffsetAngle = 0;
      this.text = "";
      if (mouse) this.setAnchorPoint(mouse.x, mouse.y);
    }
    setMouseStart(x, y) {
      this.mouseOffsetAngle = this.anchorAngle - Math.atan2(y - this.node.y, x - this.node.x);
    }
    setAnchorPoint(x, y) {
      this.anchorAngle = Math.atan2(y - this.node.y, x - this.node.x) + this.mouseOffsetAngle;
      // snap to 90 degrees
      const snap = Math.round(this.anchorAngle / (Math.PI / 2)) * (Math.PI / 2);
      if (Math.abs(this.anchorAngle - snap) < 0.1) this.anchorAngle = snap;
      // keep in the range -pi to pi so containsPoint() always works
      if (this.anchorAngle < -Math.PI) this.anchorAngle += 2 * Math.PI;
      if (this.anchorAngle > Math.PI) this.anchorAngle -= 2 * Math.PI;
    }
    getEndPointsAndCircle(r) {
      const circleX = this.node.x + 1.5 * r * Math.cos(this.anchorAngle);
      const circleY = this.node.y + 1.5 * r * Math.sin(this.anchorAngle);
      const circleRadius = 0.75 * r;
      const startAngle = this.anchorAngle - Math.PI * 0.8;
      const endAngle = this.anchorAngle + Math.PI * 0.8;
      return {
        endX: circleX + circleRadius * Math.cos(endAngle),
        endY: circleY + circleRadius * Math.sin(endAngle),
        startAngle,
        endAngle,
        circleX,
        circleY,
        circleRadius,
      };
    }
    draw(c, isSelected, r, showCaret) {
      const s = this.getEndPointsAndCircle(r);
      c.beginPath();
      c.arc(s.circleX, s.circleY, s.circleRadius, s.startAngle, s.endAngle, false);
      c.stroke();
      const textX = s.circleX + s.circleRadius * Math.cos(this.anchorAngle);
      const textY = s.circleY + s.circleRadius * Math.sin(this.anchorAngle);
      drawText(c, this.text, textX, textY, this.anchorAngle, isSelected, r, showCaret);
      drawArrow(c, s.endX, s.endY, s.endAngle + Math.PI * 0.4);
    }
    containsPoint(x, y, r) {
      const s = this.getEndPointsAndCircle(r);
      const distance = Math.hypot(x - s.circleX, y - s.circleY) - s.circleRadius;
      return Math.abs(distance) < hitTargetPadding;
    }
  }

  class StartLink {
    constructor(node, start) {
      this.node = node;
      this.deltaX = 0;
      this.deltaY = 0;
      if (start) this.setAnchorPoint(start.x, start.y);
    }
    setAnchorPoint(x, y) {
      this.deltaX = x - this.node.x;
      this.deltaY = y - this.node.y;
      if (Math.abs(this.deltaX) < snapToPadding) this.deltaX = 0;
      if (Math.abs(this.deltaY) < snapToPadding) this.deltaY = 0;
    }
    getEndPoints(r) {
      const startX = this.node.x + this.deltaX;
      const startY = this.node.y + this.deltaY;
      const end = this.node.closestPointOnCircle(startX, startY, r);
      return { startX, startY, endX: end.x, endY: end.y };
    }
    draw(c, _isSelected, r) {
      const s = this.getEndPoints(r);
      c.lineWidth = 3;
      c.beginPath();
      c.moveTo(s.startX, s.startY);
      c.lineTo(s.endX, s.endY);
      c.stroke();
      drawArrow(c, s.endX, s.endY, Math.atan2(-this.deltaY, -this.deltaX), true);
    }
    containsPoint(x, y, r) {
      return segmentContains(this.getEndPoints(r), x, y);
    }
  }

  class TemporaryLink {
    constructor(from, to) {
      this.from = from;
      this.to = to;
    }
    draw(c) {
      c.beginPath();
      c.moveTo(this.to.x, this.to.y);
      c.lineTo(this.from.x, this.from.y);
      c.stroke();
      drawArrow(c, this.to.x, this.to.y, Math.atan2(this.to.y - this.from.y, this.to.x - this.from.x));
    }
  }

  // ---------------------------------------------------------------------------
  // The interactive builder
  // ---------------------------------------------------------------------------

  class FSMBuilder {
    constructor(canvas, { alphabet, hintEl, onChange }) {
      this.canvas = canvas;
      this.ctx = canvas.getContext("2d");
      this.alphabet = alphabet;
      this.hintEl = hintEl;
      this.onChange = onChange || (() => {});

      this.nodes = [];
      this.links = [];
      this.startStates = new Set();
      this.selected = null;
      this.currentLink = null;
      this.moving = false;
      this.pan = null;
      this.origin = null;
      this.lastTap = null;

      this.highlight = new Set(); // objects flagged by the checker
      this.highlightStart = null;
      this.flash = null; // temporary hint message

      this.nodeRadius = largeStateSize;
      this.view = { x: 0, y: 0, s: 1 };
      this.cssW = 0;
      this.cssH = 0;
      this.dpr = 1;
      this.sized = false;

      this.transitionMode = false;
      this.readOnly = false;
      this.caretVisible = true;
      this.caretTimer = null;

      this.bindEvents();
    }

    // --- events -------------------------------------------------------------

    bindEvents() {
      const cv = this.canvas;
      cv.addEventListener("pointerdown", (e) => this.onPointerDown(e));
      cv.addEventListener("pointermove", (e) => this.onPointerMove(e));
      cv.addEventListener("pointerup", (e) => this.onPointerUp(e));
      cv.addEventListener("pointercancel", (e) => this.onPointerUp(e));
      cv.addEventListener("contextmenu", (e) => this.onContextMenu(e));
      cv.addEventListener("keydown", (e) => this.onKeyDown(e));
      cv.addEventListener("focus", () => this.resetCaret());
      cv.addEventListener("blur", () => this.draw());
      cv.addEventListener("wheel", (e) => {
        // Only zoom with Ctrl/⌘ (or a trackpad pinch) so the notes page still scrolls normally
        if (!(e.ctrlKey || e.metaKey) || e.deltaY === 0) return;
        e.preventDefault();
        const m = this.pointerPos(e);
        this.zoomAt(m.x, m.y, e.deltaY < 0 ? 1.1 : 1 / 1.1);
      }, { passive: false });

      new ResizeObserver(() => this.resize()).observe(cv.parentElement);
      matchMedia("(prefers-color-scheme: dark)").addEventListener("change", () => this.draw());
    }

    linkMode(e) {
      return e.shiftKey || this.transitionMode;
    }

    onPointerDown(e) {
      if (e.button !== 0) return;
      this.canvas.focus({ preventScroll: true });
      const m = this.pointerPos(e);
      const w = this.toWorld(m.x, m.y);
      e.preventDefault();

      if (this.readOnly) {
        this.startPan(m, e);
        return;
      }

      const linkMode = this.linkMode(e);

      // Our own double-click detection, so double-tap works on touch screens too
      if (!linkMode) {
        const t = performance.now();
        const lt = this.lastTap;
        if (lt && t - lt.t < 350 && Math.hypot(m.x - lt.x, m.y - lt.y) < 12) {
          this.lastTap = null;
          this.onDoubleClick(w);
          return;
        }
        this.lastTap = { t, x: m.x, y: m.y };
      }

      this.selected = this.objectAt(w.x, w.y);
      this.moving = false;
      this.origin = w;

      if (this.selected) {
        if (linkMode && this.selected instanceof Node) {
          this.currentLink = new SelfLink(this.selected, w);
        } else {
          this.moving = true;
          if (this.selected.setMouseStart) this.selected.setMouseStart(w.x, w.y);
        }
        this.resetCaret();
      } else if (linkMode) {
        this.currentLink = new TemporaryLink(w, w);
      } else {
        this.startPan(m, e);
      }
      this.canvas.setPointerCapture(e.pointerId);
      this.draw();
    }

    startPan(m, e) {
      this.pan = { sx: m.x, sy: m.y, ox: this.view.x, oy: this.view.y };
      this.canvas.setPointerCapture(e.pointerId);
      this.canvas.style.cursor = "grabbing";
      this.draw();
    }

    onDoubleClick(w) {
      const obj = this.objectAt(w.x, w.y);
      if (obj === null) {
        const node = new Node(w.x, w.y);
        node.text = this.nextStateName();
        this.nodes.push(node);
        this.selected = node;
        this.resetCaret();
        this.changed();
      } else if (obj instanceof Node) {
        obj.isAcceptState = !obj.isAcceptState;
        this.selected = obj;
        this.changed();
      }
    }

    onPointerMove(e) {
      const m = this.pointerPos(e);

      if (this.pan) {
        this.view.x = this.pan.ox + (m.x - this.pan.sx);
        this.view.y = this.pan.oy + (m.y - this.pan.sy);
        this.draw();
        return;
      }
      if (this.readOnly) return;

      const w = this.toWorld(m.x, m.y);
      const linkMode = this.linkMode(e);

      if (this.currentLink) {
        if (this.currentLink instanceof Link && !linkMode) {
          // Shift released mid-drag: bend the new transition instead
          this.currentLink.setAnchorPoint(w.x, w.y);
        } else {
          let target = this.objectAt(w.x, w.y);
          if (!(target instanceof Node)) target = null;

          if (this.selected === null) {
            if (target) {
              if (this.startStates.size === 0) {
                this.currentLink = new StartLink(target, this.origin);
              } else {
                this.highlightStart = [...this.startStates][0];
                this.setFlash("A DFA has exactly one start state. Delete the current start arrow first.");
              }
            } else {
              this.currentLink = new TemporaryLink(this.origin, w);
            }
          } else if (target === this.selected) {
            this.currentLink = new SelfLink(this.selected, w);
          } else if (target) {
            this.currentLink = new Link(this.selected, target);
          } else {
            this.currentLink = new TemporaryLink(this.selected.closestPointOnCircle(w.x, w.y, this.nodeRadius), w);
          }
        }
        this.draw();
        this.highlightStart = null;
      }

      if (this.moving && this.selected) {
        this.selected.setAnchorPoint(w.x, w.y);
        if (this.selected instanceof Node) this.snapNode(this.selected);
        this.draw();
      }
    }

    onPointerUp(e) {
      if (this.canvas.hasPointerCapture(e.pointerId)) this.canvas.releasePointerCapture(e.pointerId);
      if (this.pan) {
        this.pan = null;
        this.canvas.style.cursor = "";
        this.draw();
        return;
      }
      const wasMoving = this.moving;
      this.moving = false;

      if (this.currentLink) {
        if (!(this.currentLink instanceof TemporaryLink)) {
          this.selected = this.currentLink;
          this.links.push(this.currentLink);
          if (this.currentLink instanceof StartLink) this.startStates.add(this.currentLink.node);
          this.resetCaret();
          this.currentLink = null;
          this.changed();
          return;
        }
        this.currentLink = null;
        this.draw();
      } else if (wasMoving) {
        this.onChange(); // positions moved; save without clearing the checker's highlights
      }
    }

    onContextMenu(e) {
      e.preventDefault();
      if (this.readOnly) return;
      const m = this.pointerPos(e);
      const w = this.toWorld(m.x, m.y);
      const obj = this.objectAt(w.x, w.y);
      if (obj) {
        this.selected = obj;
        this.deleteSelected();
      }
    }

    onKeyDown(e) {
      if (this.readOnly) return;
      const sel = this.selected;
      const editable = sel !== null && "text" in sel;

      if (e.key === "Backspace") {
        e.preventDefault();
        if (editable) {
          sel.text = sel.text.slice(0, -1);
          this.resetCaret();
          this.changed();
        }
      } else if (e.key === "Delete") {
        e.preventDefault();
        this.deleteSelected();
      } else if (e.key === "Escape") {
        this.selected = null;
        this.draw();
      } else if (e.key.length === 1 && editable && !e.metaKey && !e.ctrlKey && !e.altKey) {
        const ch = e.key;
        const allowed =
          sel instanceof Node
            ? ch >= " " && ch <= "~" && !`'"\\<>%`.includes(ch)
            : this.alphabet.includes(ch) || ch === ",";
        if (allowed) {
          e.preventDefault();
          sel.text += ch;
          this.resetCaret();
          this.changed();
        } else if (ch === " ") {
          e.preventDefault(); // don't scroll the page
        }
      }
    }

    // --- editing helpers ----------------------------------------------------

    changed() {
      this.highlight.clear();
      this.draw();
      this.onChange();
    }

    setFlash(msg) {
      this.flash = msg;
      clearTimeout(this.flashTimer);
      this.flashTimer = setTimeout(() => {
        this.flash = null;
        this.updateHint();
      }, 2600);
    }

    nextStateName() {
      const used = new Set(this.nodes.map((n) => n.text.trim()));
      let i = 0;
      while (used.has(String(i))) i++;
      return String(i);
    }

    addStateNearCenter() {
      const c = this.toWorld(this.cssW / 2, this.cssH / 2);
      const r = this.nodeRadius;
      let x = c.x;
      let y = c.y;
      let tries = 0;
      while (this.nodes.some((n) => Math.hypot(n.x - x, n.y - y) < 2.2 * r) && tries++ < 40) {
        x += 2.6 * r;
        if (tries % 5 === 0) {
          x = c.x;
          y += 2.6 * r;
        }
      }
      const node = new Node(x, y);
      node.text = this.nextStateName();
      this.nodes.push(node);
      this.selected = node;
      this.canvas.focus({ preventScroll: true });
      this.resetCaret();
      this.changed();
    }

    toggleAccepting() {
      if (this.selected instanceof Node) {
        this.selected.isAcceptState = !this.selected.isAcceptState;
        this.changed();
      }
    }

    deleteSelected() {
      const sel = this.selected;
      if (!sel) return;
      this.nodes = this.nodes.filter((n) => n !== sel);
      this.links = this.links.filter((l) => {
        const hit = l === sel || l.node === sel || l.nodeA === sel || l.nodeB === sel;
        if (hit && l instanceof StartLink) this.startStates.delete(l.node);
        return !hit;
      });
      this.selected = null;
      this.currentLink = null;
      this.changed();
    }

    clear() {
      this.nodes = [];
      this.links = [];
      this.startStates.clear();
      this.selected = null;
      this.changed();
      this.fit();
    }

    objectAt(x, y) {
      for (const n of this.nodes) if (n.containsPoint(x, y, this.nodeRadius)) return n;
      for (const l of this.links) if (l.containsPoint(x, y, this.nodeRadius)) return l;
      return null;
    }

    snapNode(node) {
      for (const other of this.nodes) {
        if (other === node) continue;
        if (Math.abs(node.x - other.x) < snapToPadding) node.x = other.x;
        if (Math.abs(node.y - other.y) < snapToPadding) node.y = other.y;
      }
    }

    resetCaret() {
      clearInterval(this.caretTimer);
      this.caretTimer = setInterval(() => {
        if (document.activeElement !== this.canvas) return;
        this.caretVisible = !this.caretVisible;
        this.draw();
      }, 500);
      this.caretVisible = true;
    }

    // --- view ---------------------------------------------------------------

    pointerPos(e) {
      const b = this.canvas.getBoundingClientRect();
      return { x: e.clientX - b.left, y: e.clientY - b.top };
    }

    toWorld(sx, sy) {
      return { x: (sx - this.view.x) / this.view.s, y: (sy - this.view.y) / this.view.s };
    }

    zoomAt(sx, sy, factor) {
      const before = this.toWorld(sx, sy);
      this.view.s = Math.max(0.2, Math.min(4, this.view.s * factor));
      this.view.x = sx - before.x * this.view.s;
      this.view.y = sy - before.y * this.view.s;
      this.draw();
    }

    zoomCenter(factor) {
      this.zoomAt(this.cssW / 2, this.cssH / 2, factor);
    }

    fit() {
      if (this.nodes.length === 0) {
        this.view = { x: 0, y: 0, s: 1 };
        this.draw();
        return;
      }
      let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
      for (const n of this.nodes) {
        minX = Math.min(minX, n.x);
        minY = Math.min(minY, n.y);
        maxX = Math.max(maxX, n.x);
        maxY = Math.max(maxY, n.y);
      }
      // padding leaves room for self-loops and the start arrow
      const pad = this.nodeRadius * 2.8;
      const availW = Math.max(10, this.cssW - 2 * pad);
      const availH = Math.max(10, this.cssH - 2 * pad);
      const s = Math.min(1, availW / Math.max(1, maxX - minX), availH / Math.max(1, maxY - minY));
      this.view.s = s;
      this.view.x = this.cssW / 2 - ((minX + maxX) / 2) * s;
      this.view.y = this.cssH / 2 - ((minY + maxY) / 2) * s;
      this.draw();
    }

    toggleStateSize() {
      this.nodeRadius = this.nodeRadius === smallStateSize ? largeStateSize : smallStateSize;
      this.draw();
      this.onChange();
    }

    resize() {
      const rect = this.canvas.getBoundingClientRect();
      this.dpr = window.devicePixelRatio || 1;
      this.cssW = rect.width;
      this.cssH = rect.height;
      this.canvas.width = Math.round(rect.width * this.dpr);
      this.canvas.height = Math.round(rect.height * this.dpr);
      if (!this.sized) {
        this.sized = true;
        this.fit();
      } else {
        this.draw();
      }
    }

    // --- rendering ----------------------------------------------------------

    draw() {
      const c = this.ctx;
      const css = getComputedStyle(this.canvas);
      const ink = css.getPropertyValue("--canvas-ink").trim() || "#000";
      const selColor = css.getPropertyValue("--canvas-sel").trim() || "blue";
      const errColor = css.getPropertyValue("--canvas-err").trim() || "red";
      const showCaret = this.caretVisible && document.activeElement === this.canvas && !this.readOnly;
      const r = this.nodeRadius;

      c.setTransform(1, 0, 0, 1, 0, 0);
      c.clearRect(0, 0, this.canvas.width, this.canvas.height);
      c.setTransform(this.dpr, 0, 0, this.dpr, 0, 0);
      c.translate(this.view.x, this.view.y);
      c.scale(this.view.s, this.view.s);

      const paint = (obj) => {
        const isSelected = obj === this.selected && !this.readOnly;
        let color = ink;
        if (this.highlight.has(obj)) color = errColor;
        if (obj instanceof StartLink && this.highlightStart === obj.node) color = errColor;
        if (isSelected) color = selColor;
        c.lineWidth = isSelected ? 2 : 1.5;
        c.fillStyle = c.strokeStyle = color;
        obj.draw(c, isSelected, r, showCaret);
      };
      this.nodes.forEach(paint);
      this.links.forEach(paint);

      if (this.currentLink) {
        c.lineWidth = 1.5;
        c.fillStyle = c.strokeStyle = selColor;
        this.currentLink.draw(c, true, r, false);
      }

      if (this.nodes.length === 0) {
        c.setTransform(this.dpr, 0, 0, this.dpr, 0, 0);
        c.fillStyle = css.getPropertyValue("--muted").trim() || "#888";
        c.font = "15px system-ui, sans-serif";
        c.textAlign = "center";
        c.fillText("Double-click anywhere to add your first state", this.cssW / 2, this.cssH / 2);
        c.textAlign = "start";
      }

      this.updateHint();
    }

    updateHint() {
      if (!this.hintEl) return;
      const sel = this.selected;
      let msg;
      let warn = false;
      if (this.readOnly) {
        msg = "Showing a solution (read-only). Drag to pan. Click “Back to my DFA” to keep working.";
      } else if (this.flash) {
        msg = this.flash;
        warn = true;
      } else if (sel instanceof Node) {
        const name = sel.text.trim() ? `State “${sel.text}”` : "Unnamed state";
        msg = `${name} selected. Type to rename · double-click to toggle accepting · Delete to remove.`;
      } else if (sel instanceof StartLink) {
        msg = "Start arrow selected. Drag to reposition · Delete to remove.";
      } else if (sel instanceof Link || sel instanceof SelfLink) {
        msg = `Transition selected. Type ${this.alphabet.join(", ")} or a comma to label it · drag to bend · Delete to remove.`;
      } else if (this.transitionMode) {
        msg = "Transition mode: drag between states to add a transition; click a state for a self-loop; drag from empty space onto a state for the start arrow.";
      } else {
        msg = "Double-click empty space to add a state · Shift-drag between states to add a transition.";
      }
      this.hintEl.textContent = msg;
      this.hintEl.classList.toggle("warn", warn);
    }

    // --- save format (same shape as the PrairieLearn element's backup JSON) --

    serialize() {
      const idx = (n) => this.nodes.indexOf(n);
      return {
        nodeRadius: this.nodeRadius,
        nodes: this.nodes.map((n) => ({ x: n.x, y: n.y, text: n.text, isAcceptState: n.isAcceptState })),
        links: this.links.map((l) => {
          if (l instanceof SelfLink) return { type: "SelfLink", node: idx(l.node), text: l.text, anchorAngle: l.anchorAngle };
          if (l instanceof StartLink) return { type: "StartLink", node: idx(l.node), deltaX: l.deltaX, deltaY: l.deltaY };
          return {
            type: "Link",
            nodeA: idx(l.nodeA),
            nodeB: idx(l.nodeB),
            text: l.text,
            lineAngleAdjust: l.lineAngleAdjust,
            parallelPart: l.parallelPart,
            perpendicularPart: l.perpendicularPart,
          };
        }),
      };
    }

    load(data) {
      this.nodes = [];
      this.links = [];
      this.startStates.clear();
      this.selected = null;
      this.currentLink = null;
      this.highlight.clear();
      if (!data) {
        this.fit();
        return;
      }
      if (data.nodeRadius === smallStateSize || data.nodeRadius === largeStateSize) this.nodeRadius = data.nodeRadius;
      for (const b of data.nodes || []) {
        const n = new Node(b.x, b.y);
        n.text = String(b.text ?? "");
        n.isAcceptState = !!b.isAcceptState;
        this.nodes.push(n);
      }
      for (const b of data.links || []) {
        let link = null;
        if (b.type === "SelfLink" && this.nodes[b.node]) {
          link = new SelfLink(this.nodes[b.node]);
          link.anchorAngle = b.anchorAngle;
          link.text = String(b.text ?? "");
        } else if (b.type === "StartLink" && this.nodes[b.node] && this.startStates.size === 0) {
          link = new StartLink(this.nodes[b.node]);
          link.deltaX = b.deltaX;
          link.deltaY = b.deltaY;
          this.startStates.add(link.node);
        } else if (b.type === "Link" && this.nodes[b.nodeA] && this.nodes[b.nodeB]) {
          link = new Link(this.nodes[b.nodeA], this.nodes[b.nodeB]);
          link.parallelPart = b.parallelPart;
          link.perpendicularPart = b.perpendicularPart;
          link.lineAngleAdjust = b.lineAngleAdjust || 0;
          link.text = String(b.text ?? "");
        }
        if (link) this.links.push(link);
      }
      if (this.sized) this.fit();
    }
  }

  // ---------------------------------------------------------------------------
  // Turning the drawing into a DFA, and grading it
  // ---------------------------------------------------------------------------

  const DUMP = null; // the hidden dead state

  function stateLabel(n) {
    return n.text.trim() ? `State “${n.text.trim()}”` : "An unnamed state";
  }

  function extractDFA(b, useDump, alphabet) {
    const errors = [];
    const bad = new Set();

    if (b.nodes.length === 0) return { errors: ["Your DFA has no states yet."], bad };

    const byName = new Map();
    const unnamed = [];
    for (const n of b.nodes) {
      const name = n.text.trim();
      if (!name) unnamed.push(n);
      else byName.set(name, [...(byName.get(name) || []), n]);
    }
    if (unnamed.length) {
      errors.push(unnamed.length === 1 ? "A state has no name." : `${unnamed.length} states have no name.`);
      unnamed.forEach((n) => bad.add(n));
    }
    for (const [name, list] of byName) {
      if (list.length > 1) {
        errors.push(`State names must be unique: “${name}” is used ${list.length} times.`);
        list.forEach((n) => bad.add(n));
      }
    }

    const starts = b.links.filter((l) => l instanceof StartLink);
    if (starts.length === 0) {
      errors.push("There is no start state. Shift-drag from empty space onto a state to add the start arrow.");
    }

    const out = new Map(); // node -> Map(symbol -> { to, link })
    for (const l of b.links) {
      if (l instanceof StartLink) continue;
      const from = l instanceof SelfLink ? l.node : l.nodeA;
      const to = l instanceof SelfLink ? l.node : l.nodeB;
      const symbols = l.text.split(",").map((s) => s.trim()).filter(Boolean);
      if (symbols.length === 0) {
        errors.push(`${stateLabel(from)} has a transition with no label.`);
        bad.add(l);
        continue;
      }
      if (!out.has(from)) out.set(from, new Map());
      const m = out.get(from);
      for (const s of symbols) {
        if (!alphabet.includes(s)) {
          errors.push(`“${s}” is not a symbol of {${alphabet.join(", ")}}. Separate symbols with commas.`);
          bad.add(l);
        } else if (m.has(s)) {
          errors.push(`${stateLabel(from)} has more than one transition on ${s}.`);
          bad.add(l);
          bad.add(m.get(s).link);
        } else {
          m.set(s, { to, link: l });
        }
      }
    }

    let anyMissing = false;
    for (const n of b.nodes) {
      const missing = alphabet.filter((a) => !out.get(n)?.has(a));
      if (missing.length) {
        anyMissing = true;
        if (!useDump) {
          errors.push(`${stateLabel(n)} has no transition on ${missing.join(" or ")}.`);
          bad.add(n);
        }
      }
    }
    if (anyMissing && !useDump) {
      errors.push("Add the missing transitions, or tick “Send missing transitions to a dump state”.");
    }

    if (errors.length) return { errors: [...new Set(errors)], bad };

    const delta = {};
    for (const n of b.nodes) {
      const row = {};
      for (const [sym, { to }] of out.get(n) || []) row[sym] = to.text.trim();
      delta[n.text.trim()] = row;
    }
    return {
      dfa: {
        start: starts[0].node.text.trim(),
        finals: new Set(b.nodes.filter((n) => n.isAcceptState).map((n) => n.text.trim())),
        delta,
      },
      stateCount: b.nodes.length + (anyMissing ? 1 : 0),
      errors: [],
      bad,
    };
  }

  const step = (d, q, a) => (q === DUMP ? DUMP : d.delta[q]?.[a] ?? DUMP);
  const accepts = (d, q) => q !== DUMP && d.finals.has(q);

  // Exact equivalence: explore every reachable pair of states in the product automaton
  function equivalent(A, B, alphabet) {
    const seen = new Set();
    const queue = [[A.start, B.start]];
    while (queue.length) {
      const [p, q] = queue.shift();
      const key = JSON.stringify([p, q]);
      if (seen.has(key)) continue;
      seen.add(key);
      if (accepts(A, p) !== accepts(B, q)) return false;
      for (const a of alphabet) queue.push([step(A, p, a), step(B, q, a)]);
    }
    return true;
  }

  // Shortest-first counterexamples, like the PrairieLearn grader's feedback
  function counterexamples(student, ref, alphabet, maxLen = 10, maxEach = 5) {
    const falsePos = [];
    const falseNeg = [];
    let level = [["", student.start, ref.start]];
    for (let len = 0; len <= maxLen && level.length; len++) {
      const next = [];
      for (const [w, p, q] of level) {
        const sa = accepts(student, p);
        const ra = accepts(ref, q);
        if (sa && !ra && falsePos.length < maxEach) falsePos.push(w);
        if (!sa && ra && falseNeg.length < maxEach) falseNeg.push(w);
        for (const a of alphabet) next.push([w + a, step(student, p, a), step(ref, q, a)]);
      }
      if (falsePos.length >= maxEach && falseNeg.length >= maxEach) break;
      level = next;
    }
    return { falsePos, falseNeg, maxLen };
  }


  // ---------------------------------------------------------------------------
  // Page wiring: one call per builder on the page
  // ---------------------------------------------------------------------------

  function el(tag, props = {}, ...children) {
    const e = document.createElement(tag);
    Object.assign(e, props);
    for (const ch of children) e.append(ch);
    return e;
  }

  function initBuilder(root, P) {
    const $ = (role) => root.querySelector(`[data-role="${role}"]`);

    const feedbackEl = $("feedback");
    const dumpBox = $("dump");

    function save() {
      if (builder.readOnly) return;
      try {
        localStorage.setItem(P.storageKey, JSON.stringify({ fsm: builder.serialize(), dump: dumpBox.checked }));
      } catch (_) { /* storage unavailable: work just isn't remembered */ }
    }

    function loadSaved() {
      try {
        const raw = localStorage.getItem(P.storageKey);
        return raw ? JSON.parse(raw) : null;
      } catch (_) {
        return null;
      }
    }

    const builder = new FSMBuilder($("fsm"), {
      alphabet: P.alphabet,
      hintEl: $("hint"),
      onChange: () => {
        save();
        updateButtons();
      },
    });

    const saved = loadSaved();
    if (saved) {
      builder.load(saved.fsm);
      dumpBox.checked = !!saved.dump;
    }

    function updateButtons() {
      const ro = builder.readOnly;
      $("btn-accept").disabled = ro || !(builder.selected instanceof Node);
      $("btn-delete").disabled = ro || builder.selected === null;
      for (const id of ["btn-add", "btn-mode", "btn-size", "btn-clear", "btn-check"]) $(id).disabled = ro;
      dumpBox.disabled = ro;
    }

    // Keep selection-dependent buttons in sync after pointer interactions
    $("fsm").addEventListener("pointerup", updateButtons);
    $("fsm").addEventListener("keydown", () => setTimeout(updateButtons));

    $("btn-add").addEventListener("click", () => builder.addStateNearCenter());
    $("btn-mode").addEventListener("click", (e) => {
      builder.transitionMode = !builder.transitionMode;
      e.currentTarget.setAttribute("aria-pressed", String(builder.transitionMode));
      builder.draw();
    });
    $("btn-accept").addEventListener("click", () => builder.toggleAccepting());
    $("btn-delete").addEventListener("click", () => {
      builder.deleteSelected();
      updateButtons();
    });
    $("btn-zoom-in").addEventListener("click", () => builder.zoomCenter(1.2));
    $("btn-zoom-out").addEventListener("click", () => builder.zoomCenter(1 / 1.2));
    $("btn-fit").addEventListener("click", () => builder.fit());
    $("btn-size").addEventListener("click", () => builder.toggleStateSize());
    dumpBox.addEventListener("change", save);

    // Two-step clear (window.confirm can be blocked when the page is embedded)
    const clearBtn = $("btn-clear");
    let clearArmed = null;
    clearBtn.addEventListener("click", () => {
      if (clearArmed) {
        clearTimeout(clearArmed);
        clearArmed = null;
        clearBtn.textContent = "Clear";
        builder.clear();
        feedbackEl.textContent = "";
        feedbackEl.className = "feedback";
        updateButtons();
      } else {
        clearBtn.textContent = "Click again to clear";
        clearArmed = setTimeout(() => {
          clearArmed = null;
          clearBtn.textContent = "Clear";
        }, 3000);
      }
    });

    // Solution view: temporarily swap in the reference drawing, read-only
    let stashed = null;
    $("btn-solution").addEventListener("click", (e) => {
      const btn = e.currentTarget;
      if (!builder.readOnly) {
        stashed = builder.serialize();
        builder.readOnly = true;
        builder.load(P.solution);
        btn.textContent = "Back to my DFA";
        showSolutionNotes();
      } else {
        builder.readOnly = false;
        builder.load(stashed);
        stashed = null;
        btn.textContent = "Show solution";
        feedbackEl.textContent = "";
        feedbackEl.className = "feedback";
      }
      builder.fit();
      updateButtons();
    });

    function showSolutionNotes() {
      const s = P.solutionNotes;
      feedbackEl.className = "feedback";
      feedbackEl.replaceChildren(
        el("p", { className: "verdict", textContent: s.title }),
        el("p", { textContent: s.intro }),
        el("ul", {}, ...s.states.map(([name, text]) => el("li", {}, el("b", { textContent: name }), " — " + text))),
        s.outro ? el("p", { textContent: s.outro }) : "",
      );
    }

    $("btn-check").addEventListener("click", () => {
      const res = extractDFA(builder, dumpBox.checked, P.alphabet);
      builder.highlight = res.bad;
      builder.draw();

      if (res.errors.length) {
        feedbackEl.className = "feedback bad";
        feedbackEl.replaceChildren(
          el("p", { className: "verdict", textContent: "This isn't a valid DFA yet" }),
          el("ul", {}, ...res.errors.map((m) => el("li", { textContent: m }))),
          res.bad.size ? el("p", { className: "muted", textContent: "The problem states and transitions are shown in red." }) : "",
        );
        return;
      }

      if (equivalent(res.dfa, P.reference, P.alphabet)) {
        const n = res.stateCount;
        const sizeNote =
          n === P.minimalStates
            ? `It uses ${n} states, which is the minimum possible.`
            : `It uses ${n} states. The smallest DFA for this language has ${P.minimalStates}. Can you merge any?`;
        feedbackEl.className = "feedback ok";
        feedbackEl.replaceChildren(
          el("p", { className: "verdict", textContent: "Correct!" }),
          el("p", { textContent: `Your DFA accepts exactly ${P.languageSummary}. ` + sizeNote }),
        );
        return;
      }

      const { falsePos, falseNeg, maxLen } = counterexamples(res.dfa, P.reference, P.alphabet);
      const chips = (list) =>
        el("ul", { className: "chips" }, ...list.map((w) => el("li", { textContent: w === "" ? "ε" : w })));
      const parts = [el("p", { className: "verdict", textContent: "Not quite" })];
      if (falsePos.length) {
        parts.push(el("p", { textContent: "Your DFA accepts these strings, which are not in the language:" }), chips(falsePos));
      }
      if (falseNeg.length) {
        parts.push(el("p", { textContent: "These strings are in the language, but your DFA rejects them:" }), chips(falseNeg));
      }
      if (!falsePos.length && !falseNeg.length) {
        parts.push(el("p", { textContent: `There are no counterexamples of length ${maxLen} or less, but your DFA still differs on some longer string.` }));
      }
      parts.push(el("p", { className: "muted", textContent: "Tip: trace one of these strings below to see where your DFA goes wrong." }));
      feedbackEl.className = "feedback bad";
      feedbackEl.replaceChildren(...parts);
    });

    // String tracer
    $("trace-form").addEventListener("submit", (e) => {
      e.preventDefault();
      const out = $("trace");
      const raw = $("trace-input").value.trim();
      const w = raw === "ε" ? "" : raw;
      if ([...w].some((ch) => !P.alphabet.includes(ch))) {
        out.replaceChildren(el("span", { className: "no", textContent: `Only the symbols ${P.alphabet.join(" and ")} are allowed.` }));
        return;
      }
      const res = extractDFA(builder, dumpBox.checked, P.alphabet);
      if (res.errors.length) {
        out.replaceChildren(el("span", { className: "muted", textContent: "Fix your DFA first. Click “Check my DFA” to see what's wrong." }));
        return;
      }
      const d = res.dfa;
      const name = (q) => (q === DUMP ? "∅" : q);
      let q = d.start;
      let path = name(q);
      for (const a of w) {
        q = step(d, q, a);
        path += ` —${a}→ ${name(q)}`;
      }
      const acc = accepts(d, q);
      const should = P.inLanguage(w);
      out.replaceChildren(
        el("div", { textContent: `"${w === "" ? "ε" : w}":  ${path}` }),
        el("div", {},
          "Your DFA ",
          el("span", { className: acc ? "yes" : "no", textContent: acc ? "accepts" : "rejects" }),
          " it. ",
          acc === should
            ? el("span", { className: "muted", textContent: "That's right." })
            : el("span", { className: "no", textContent: `It should be ${should ? "accepted" : "rejected"}.` }),
        ),
      );
    });

    updateButtons();
  }

  document.addEventListener("DOMContentLoaded", () => {
    const problems = window.DFA_PROBLEMS || {};
    for (const root of document.querySelectorAll(".dfa-builder[data-problem]")) {
      const P = problems[root.dataset.problem];
      if (P) initBuilder(root, P);
      else console.error(`dfa-builder: no problem named "${root.dataset.problem}"`);
    }
  });
})();
