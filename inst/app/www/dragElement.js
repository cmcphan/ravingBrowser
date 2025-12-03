// Adapted from https://www.w3schools.com/howto/howto_js_draggable.asp
function getLine() {
  return document.getElementById("plot_1-alignment_bar");
}

function getBarButton() {
  return document.getElementById("toolbar_1-toggle_alignment_bar")
}

function disableLine() {
  line = getLine();
  button = getBarButton();
  line.onmousedown = null;
  line.style.visibility = "hidden";
  button.removeEventListener("click", disableLine);
  button.addEventListener("click", dragLine)
}

function dragLine() {
  var xInitial = 0, xNew = 0;
  line = getLine();
  button = getBarButton();
  line.onmousedown = dragMouseDown;
  line.style.visibility = "visible";
  button.removeEventListener("click", dragLine);
  button.addEventListener("click", disableLine);

  function dragMouseDown(e) {
    e.preventDefault();
    // get the mouse cursor position at startup:
    xInitial = e.clientX;
    document.onmouseup = closeDragLine;
    // call a function whenever the cursor moves:
    document.onmousemove = lineDrag;
  }

  function lineDrag(e) {
    e.preventDefault();
    // calculate the new cursor position:
    xNew = xInitial - e.clientX;
    xInitial = e.clientX;
    xMax = getPlotWidth() * 0.95; // From plotBrush.js
    // set the element's new position:
    xSet = line.offsetLeft - xNew;
    if (xSet < 0) {
      line.style.left = 0 + "px";
    } else if (xSet > xMax) {
      line.style.left = xMax + "px";
    } else {
      line.style.left = xSet + "px";
    }
  }

  function closeDragLine() {
    // stop moving when mouse button is released:
    document.onmouseup = null;
    document.onmousemove = null;
  }
}
