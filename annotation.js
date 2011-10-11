$(document).ready(function() {
  var caseName = getUrlVars()['case_name'];
  if (caseName === undefined) {
    alert("Please specify a case_name GET param");
  } else {
    loadCase(caseName);
  }
});

function loadCase(caseName) {
  document.getElementById('photo').src = '/input/' + caseName + '.jpeg';

  $.ajax({
    type:'GET',
    url:'/input/' + caseName + '.json',
    dataType: 'text',
    success: function(data) {
      var boxesJSON = JSON.parse(data);
      for (var i = 0; i < boxesJSON.length; i++) {
        var boxJSON = boxesJSON[i];
        var newBox = createBox(global.nextBoxNum,
          boxJSON.left, boxJSON.top, boxJSON.width, boxJSON.height,
          boxJSON.notes);
        for (var j = 0; j < boxJSON.points.length; j++) {
          var point = boxJSON.points[j];
          createPoint(newBox, point.x, point.y, point.type, point.staffY);
        }
      }
    },
    error: function(jqXHR, textStatus, errorThrown) {
      console.warn(jqXHR);
      console.warn(textStatus);
      console.warn(errorThrown);
    }
  });
}

var global = {
  newBoxMode:false,
  nextBoxNum:0,
  boxes:[],
  currentBoxNum:null,
  newPointMode:false
}

var TABLE_Y_TO_STAFF_Y = -8;

function updateButtonGlows() {
  document.getElementById('newBoxButton').style.border =
    (global.newBoxMode) ? '3px #0f0 solid' : '';
  document.getElementById('newPointButton').style.border =
    (global.newPointMode) ? '3px #0f0 solid' : '';
}

function updatePointerCursor() {
  var photo = document.getElementById('photo')
  var corner = document.getElementById('corner');
  if (global.newBoxMode) {
    photo.onmousemove = function() {
      var e = e ? e : window.event;
      corner.style.display = 'block';
      corner.style.left = e.clientX + document.body.scrollLeft + 'px';
      corner.style.top = e.clientY + document.body.scrollTop + 'px';
    }
  }
  else {
    photo.onmousemove = null;
    corner.style.display = 'none';
  }
}

function toggleNewBoxMode() {
  if (global.newBoxMode)
    global.newBoxMode = false;
  else {
    global.newBoxMode = true;
    global.newPointMode = false;
  }
  updatePointerCursor();
  updateButtonGlows();
  return false;
}

function toggleNewPointMode() {
  if (global.newPointMode)
    global.newPointMode = false;
  else {
    global.newPointMode = true;
    global.newBoxMode = false;
  }
  updatePointerCursor();
  updateButtonGlows();
}

function updateBoxesJSON() {
  var boxJSON = document.getElementById('box_json');
  boxJSON.value = JSON.stringify(global.boxes);
}

function createBox(num, left, top, width, height, notes) {
  var box = document.createElement('div');
  box.setAttribute('id', 'box' + num);
  box.setAttribute('style', 'position:absolute;' +
    'left:' + left + 'px' + ';' +
    'top:' + top + 'px' + ';' +
    'width:' + width + 'px;' +
    'height:' + height + 'px;' +
    'border:3px black solid');
  var surroundingDiv = document.getElementById('surrounding_div');
  surroundingDiv.appendChild(box);
  var box = document.getElementById('box' + global.nextBoxNum);
  global.nextBoxNum += 1; // update early so loops include new box
  box.onmousedown = function(e) {
    if (photo.onmousedown) photo.onmousedown(e);
  }
  box.onmousemove = function(e) {
    if (photo.onmousemove) photo.onmousemove(e);
  };
  box.onmouseup = function(e) {
    if (photo.onmouseup) photo.onmouseup(e);
  };

  var newBox = {num:num, left:left, top:top, width:width, height:height,
     notes:notes, points:[]};
  global.boxes.push(newBox);
  updateBoxesJSON();
  return newBox;
}

function findBoxIncludingPoint(x, y) {
  var foundBox = null;
  for (var i = 0; i < global.nextBoxNum; i++) {
    var box = global.boxes[i];
    if (box) {
      var x0 = box.left;
      var y0 = box.top;
      var x1 = x0 + box.width;
      var y1 = y0 + box.height;
      if (box && x >= x0 && y >= y0 && x <= x1 && y <= y1) {
        foundBox = box;
        break;
      }
    }
  }
  return foundBox;
}

function createPoint(box, x, y, pointType, staffY) {
  var div = document.createElement('div');
  var surroundingDiv = document.getElementById('surrounding_div');
  div.setAttribute('class', 'point');
  div.setAttribute('style',
    'position:absolute;left:' + x + 'px;top:' + y + 'px');

  var text = document.createTextNode();
  text.nodeValue = pointType;
  div.appendChild(text);

  surroundingDiv.appendChild(div);

  box.points.push({type:pointType, x:x, y:y, staffY:staffY});
  updateBoxesJSON();
}

function handleClick(x, y) {
  if (global.newBoxMode) {
    var photo = document.getElementById('photo');
    createBox(global.nextBoxNum, x, y, 50, 50, []);
    var corner = document.getElementById('corner');
    corner.style.display = 'none';
  }
  else if (global.newPointMode) {
    var foundBox = findBoxIncludingPoint(x, y);
    if (foundBox) {
      var pointType = $('input:radio[name=point_type]:checked').val();
      var staffY = parseInt($('input:radio[name=staff_y]:checked').val());
      createPoint(foundBox, x, y, pointType, staffY);
    }
  }
  else {
    var foundBox = findBoxIncludingPoint(x, y);
    if (foundBox) {
      var foundBoxDiv = document.getElementById('box' + foundBox.num);
      if (global.currentBoxNum == foundBox.num) {
        foundBoxDiv.style.border = '3px black solid';
        global.currentBoxNum = null;
      }
      else {
        if (global.currentBoxNum !== null) {
          var oldCurrentBox =
            document.getElementById('box' + global.currentBoxNum);
          oldCurrentBox.style.border = '3px black solid';
        }
        foundBoxDiv.style.border = '3px #0f0 solid';
        global.currentBoxNum = foundBox.num;
      }
    }
  }
}

function handleDrag(x, y) {
  if (global.newBoxMode) {
    var boxNum = global.nextBoxNum - 1;
    var box = document.getElementById('box' + boxNum);
    box.style.width  = (x - parseInt(box.style.left)) + 'px';
    box.style.height = (y - parseInt(box.style.top)) + 'px';
    global.boxes[boxNum].width = x - global.boxes[boxNum].left;
    global.boxes[boxNum].height = y - global.boxes[boxNum].top;
  }
}

function handleOnMouseDown() {
  var e = e ? e : window.event;
  //if (e.targetTouches)
  //  return; // let the touchstart handler take care of it
  var photo = document.getElementById('photo');
  handleClick(e.clientX + document.body.scrollLeft - 12,
    e.clientY + document.body.scrollTop - 15);
  photo.onmousemove = function(e) {
    var e = e ? e : window.event;
    handleDrag(e.clientX + document.body.scrollLeft,
      e.clientY + document.body.scrollTop);
  };
  photo.onmouseup = function(e) {
    if (global.newBoxMode) {
      var photo = document.getElementById('photo');
      photo.onmousemove = null;
      photo.onmouseup = null;
      global.addMode = false;
      updateBoxesJSON();
    }
  };
  //var box = document.getElementById('box1');
  //box.onmousemove = photo.onmousemove;
  //box.onmouseup = photo.onmouseup;
  e.preventDefault(); // prevent Chrome's default of dragging the image around
}

// Read a page's GET URL variables and return them as an associative array.
function getUrlVars() {
  var vars = [], hash;
  var hashes = window.location.href.slice(
    window.location.href.indexOf('?') + 1).split('&');
  for(var i = 0; i < hashes.length; i++) {
    hash = hashes[i].split('=');
    vars.push(hash[0]);
    vars[hash[0]] = hash[1];
  }
  return vars;
}
