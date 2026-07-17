minPos = 0;
maxPos = 0;
coordMin = 0;
coordMax = 0;

// Adapted from https://www.w3schools.com/howto/howto_js_draggable.asp
function getLine() {
  return document.getElementById("plot_1-alignment_bar");
}

function getBarButton() {
  return document.getElementById("toolbar_1-toggle_alignment_bar");
}

function getText() {
  return document.getElementById("plot_1-alignment_bar_text");
}

Shiny.addCustomMessageHandler("plot_coords", function(coords) {
  coordMin = coords[0];
  coordMax = coords[1];
  line = getLine();
  text = getText();
  text.innerHTML = convertCoordSingle(line.offsetLeft);
});

// Function to convert the current coordinate of the alignment bar
//  into a genomic position on the current plot
function convertCoordSingle(p) {
  // Width of just the plot panel. Function from plotBrush.js
  var width = getPlotWidth();
  minPos = minLeft - 26;
  maxPos = width - minRight + 3;
  var pos = (p-minPos) / (maxPos-minPos);
  var coord = Math.floor(coordMin + pos*(coordMax-coordMin));
  return coord;
}

function disableLine() {
  line = getLine();
  button = getBarButton();
  text = getText();
  line.onmousedown = null;
  line.style.visibility = "hidden";
  text.style.visibility = "hidden";
  button.removeEventListener("click", disableLine);
  button.addEventListener("click", dragLine);
  button.classList.remove("button_toggle_on");
  button.blur();
}

function dragLine() {
  var xInitial = 0, xNew = 0;
  line = getLine();
  button = getBarButton();
  text = getText();
  line.onmousedown = dragMouseDown;
  line.style.visibility = "visible";
  text.innerHTML = convertCoordSingle(line.offsetLeft);
  text.style.visibility = "visible";
  button.removeEventListener("click", dragLine);
  button.addEventListener("click", disableLine);
  button.classList.add("button_toggle_on");
  button.blur();

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
    // set the element's new position:
    xSet = line.offsetLeft - xNew;
    if (xSet <= minPos) {
      line.style.left = minPos + "px";
      text.innerHTML = convertCoordSingle(minPos);
    } else if (xSet >= maxPos) {
      line.style.left = maxPos + "px";
      text.innerHTML = convertCoordSingle(maxPos);
    } else {
      line.style.left = xSet + "px";
      text.innerHTML = convertCoordSingle(xSet);
    }
  }

  function closeDragLine() {
    // stop moving when mouse button is released:
    document.onmouseup = null;
    document.onmousemove = null;
  }
}
