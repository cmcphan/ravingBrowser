var pre_panel = 0;
var post_panel = 0;
var plot_draws = 0; // Number of times a plot has been drawn
var minLeft = 0;
var minRight = 0;
// Keep track of where the div edges currently are
var xLeft = 0;
var xRight = 0;

function getBrush() {
  return document.getElementById("plot_1-brush");
}

// This is the toggle button in the toolbar
function getBrushButton() {
  return document.getElementById("toolbar_1-toggle_brush");
}

// This is the button inside the brush itself
function getBrushButtonInternal() {
  return document.getElementById("plot_1-brush_zoom");
}

function getPlotWidth() {
  return document.getElementById("plot_1-patchwork").offsetWidth;
}

function getPlot() {
  return document.getElementById("plot_1-patchwork");
}

// Function to convert the left and right coordinates of the brush
//  into proportions relative to the plot panel width
function convertCoords(l, r) {
  // Plot has not been drawn yet so we shouldn't return any values
  if (plot_draws == 0) {
    return null;
  }
  // Width of just the plot panel
  var width = getPlotWidth();
  var maxLeft = width - post_panel - 15;
  var left = (l - 15 - pre_panel) / maxLeft;
  // Essentially converting right coord to an equivalent left coord here
  var right = (width - r) / maxLeft;
  // Clicking and
  //  not dragging out the brush results in a slight overlap in these calcs where 
  //  the left coord is slightly higher than the right one. Dragging even a single
  //  pixel resolves this problem. This is used as a check in the observeEvent in 
  //  in the app itself to determine when the brush is too narrow.
  return [left, right];
}

Shiny.addCustomMessageHandler("panel_widths", function(widths) {
  // Divide by 2 since the source image seems to always be double 
  //  the dimensions of the plot canvas, so adjust the pixel widths accordingly
  pre_panel = widths[0]/2;
  post_panel = widths[1]/2;
  plot_draws = plot_draws + 1;
  // Check that the brush is not now outside of the plot panel
  minLeft = pre_panel + 15;
  minRight = post_panel + 15;
  var brush = getBrush();
  if (xLeft < minLeft) {
    brush.style.left = minLeft + "px";
    xLeft = minLeft;
  }
  if (xRight < minRight) {
    brush.style.right = minRight + "px";
    xRight = minRight;
  }
  Shiny.setInputValue("plot_1-brush_coords", convertCoords(xLeft, xRight));
});

function disableBrush() {
  var brush = getBrush();
  var button = getBrushButton();
  var plot = getPlot();
  var brushButton = getBrushButtonInternal();
  plot.onmousedown = null;
  plot.style.cursor = "default";
  brush.style.visibility = "hidden";
  button.removeEventListener("click", disableBrush);
  button.addEventListener("click", brushElement);
  button.classList.remove("button_toggle_on");
  button.blur();
  brushButton.style.visibility = "hidden";
}

function brushElement() {
  var xInitial = 0, xNew = 0;
  var brush = getBrush();
  var button = getBrushButton();
  var plot = getPlot();
  var brushButton = getBrushButtonInternal();
  var rLimit = 0;
  var plotWidth = 0;
  plot.onmousedown = brushMouseDown;
  plot.style.cursor = "grab";
  button.removeEventListener("click", brushElement);
  button.addEventListener("click", disableBrush);
  button.classList.add("button_toggle_on");
  button.blur();

  function brushMouseDown(e) {
    e.preventDefault();
    brush.style.cursor = "grabbing";
    plot.style.cursor = "grabbing";
    // get the mouse cursor position on click:
    xInitial = e.clientX;
    plotWidth = getPlotWidth();
    xLeft = e.offsetX + 15; // Correcting for padding
    var maxLeft = plotWidth - post_panel + 11;
    if (xLeft < minLeft){
      xLeft = minLeft;
      brush.style.left = xLeft + "px";
    } else if (xLeft > maxLeft) {
      xLeft = maxLeft;
    }
    brush.style.left = xLeft + "px";
    xRight = plotWidth - xLeft + 26;
    brush.style.right = xRight + "px";
    brush.style.visibility = "visible";
    brushMode = "r";
    rLimit = plotWidth - xLeft + 26;
    brushButton.style.visibility = "hidden";

    document.onmouseup = closeBrush;
    // call a function whenever the cursor moves:
    // These are called on the document so that regardless of where the cursor
    //  is when the mouse button is released or the mouse is moved the effect will
    //  apply. Setting this on the brush element itself means that if the cursor is
    //  dragged outside of the brush it will no longer work.
    document.onmousemove = elementBrush;
  }

  function elementBrush(e) {
    e.preventDefault();
    // calculate the new cursor position:
    xNew = xInitial - e.clientX;
    xInitial = e.clientX;
    
    function moveLeft() {
      newLeft = xLeft - xNew;
      if (newLeft <= minLeft) {
        rLimit = plotWidth - minLeft + 26;
        brush.style.left = minLeft + "px";
      } else if ((xNew < 0) & (newLeft > (plotWidth - xRight + 26))){
        xLeft = plotWidth - xRight + 26;
        brush.style.left = xLeft + "px";
        rLimit = plotWidth - xLeft + 26;
        brushMode = "r";
        moveRight();
      }
      else {
        xLeft = newLeft;
        rLimit = plotWidth - xLeft + 26;
        brush.style.left = xLeft + "px";
      }
    }

    function moveRight() {
      newRight = xRight + xNew;
      if (newRight <= minRight){
        brush.style.right = minRight + "px";
      } else if ((xNew > 0) & (newRight > rLimit)) {
        xRight = rLimit;
        brush.style.right = rLimit + "px";
        brushMode = "l";
        moveLeft();
      } else {
        xRight = newRight;
        brush.style.right = xRight + "px";
      }
    }

    if (brushMode == "r") {
      moveRight();
    } else if (brushMode == "l") {
      moveLeft();
    }
  }

  function closeBrush() {
    // stop moving when mouse button is released:
    document.onmouseup = null;
    document.onmousemove = null;
    brush.style.cursor = "move";
    plot.style.cursor = "grab";
    brush.onmousedown = dragBrushMouseDown;
    Shiny.setInputValue("plot_1-brush_coords", convertCoords(xLeft, xRight));
    brushButton.style.visibility = "visible";
  }
}

function dragBrushMouseDown(e) {
  e.preventDefault();
  var xInitial = 0, xNew = 0;
  xInitial = e.clientX;
  var brush = getBrush();
  var plotWidth = getPlotWidth();
  var brushButton = getBrushButtonInternal();
  brushWidth = brush.offsetWidth;
  document.onmouseup = closeDragBrush;
  document.onmousemove = brushDrag;

  function brushDrag(e) {
    e.preventDefault();
    brushButton.style.visibility = "hidden";
    xNew = xInitial - e.clientX;
    xInitial = e.clientX;
    newLeft = xLeft - xNew;
    newRight = xRight + xNew;
    if (xNew > 0) {
      if (newLeft <= minLeft) {
        xLeft = minLeft;
        xRight = plotWidth - minLeft + 30 - brushWidth;
      } else {
        xLeft = newLeft;
        xRight = newRight;
      }
    } else if (xNew < 0) {
      if (newRight <= minRight) {
        xLeft = plotWidth - minRight + 30 - brushWidth;
        xRight = minRight;
      } else {
        xLeft = newLeft;
        xRight = newRight;
      }
    }
    brush.style.left = xLeft + "px";
    brush.style.right = xRight + "px";
  }

  function closeDragBrush() {
    document.onmouseup = null;
    document.onmousemove = null;
    Shiny.setInputValue("plot_1-brush_coords", convertCoords(xLeft, xRight));
    brushButton.style.visibility = "visible";
  }
}

// This is used by the toolbar buttons to hide the brush when a
//  different operation is triggered
function hideBrush() {
  var brush = getBrush();
  var brushButton = getBrushButtonInternal();
  brush.style.visibility = "hidden";
  brushButton.style.visibility = "hidden";
}