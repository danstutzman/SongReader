$(document).ready(function() {
  $.ajax({
    type:'GET',
    url:'/boxes3.json',
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
          createPoint(newBox, point.x, point.y, point.type);
        }
      }
    },
    error: function(jqXHR, textStatus, errorThrown) {
      console.warn(jqXHR);
      console.warn(textStatus);
      console.warn(errorThrown);
    }
  });
});

var global = {
  newBoxMode:false,
  nextBoxNum:0,
  boxes:[],
  currentBoxNum:null,
  newBoxMode:false,
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

function createPoint(box, x, y, pointType) {
  var div = document.createElement('div');
  var surroundingDiv = document.getElementById('surrounding_div');
  div.setAttribute('class', 'point');
  div.setAttribute('style',
    'position:absolute;left:' + x + 'px;top:' + y + 'px');

  var text = document.createTextNode();
  text.nodeValue = pointType;
  div.appendChild(text);

  surroundingDiv.appendChild(div);

  box.points.push({type:pointType, x:x, y:y});
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
      var pointType = document.getElementById('point_type').value;
      createPoint(foundBox, x, y, pointType);
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
    updateStaffFromCurrentBox();
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

function getTDHasNote(td) {
  var text = td.childNodes[0];
  return text != null && text.nodeValue == 'O';
}

function setTDHasNote(td, newHasNote) {
  var text = td.childNodes[0];
  if (!text) {
    text = document.createTextNode('');
    td.appendChild(text);
  }
  text.nodeValue = newHasNote ? 'O' : '';
}

function updateCurrentBoxJSONFromStaff() {
  if (global.currentBoxNum !== null) {
    var notes = [];
    var staff = $('#staff')[0];
    var trs = staff.getElementsByTagName('tr');
    for (var i = 0; i < trs.length; i++) {
      var tr = trs[i];
      var tds = tr.childNodes;
      for (var j = 0; j < tds.length; j++) {
        var td = tds[j];
        var hasNote =
          td.childNodes[0] && td.childNodes[0].nodeValue == 'O';
        if (j >= notes.length)
          notes.push([]);
        if (hasNote)
          notes[j].push(i + TABLE_Y_TO_STAFF_Y);
      }
    }

    var currentBox = global.boxes[global.currentBoxNum];
    currentBox.notes = notes;
  }
  updateBoxesJSON();
}

function updateStaffFromCurrentBox() {
  // clear staff
  var staff = $('#staff')[0];
  var trs = staff.getElementsByTagName('tr');
  for (var i = 0; i < trs.length; i++) {
    var tr = trs[i];
    while (tr.childNodes.length > 0)
      tr.removeChild(tr.childNodes[0]);
  }

  if (global.currentBoxNum !== null) {
    var box = global.boxes[global.currentBoxNum];

    for (var x = 0; x < box.notes.length; x++) {
      var notesForX = box.notes[x];
      extendStaff();
      for (var i = 0; i < notesForX.length; i++) {
        var y = notesForX[i] - TABLE_Y_TO_STAFF_Y;
        var td = trs[y].getElementsByTagName('td')[x];
        setTDHasNote(td, true);
      }
    }
  }
}

function extendStaff() {
  var staff = $('#staff')[0];
  var trs = staff.getElementsByTagName('tr');
  for (var i = 0; i < trs.length; i++) {
    var tr = trs[i];
    var td = document.createElement('td');
    var class_;
    if (i < 4) class_ = (i % 2 == 0) ? 'ledger' : 'space';
    else if (i > 12) class_ = (i % 2 == 0) ? 'ledger' : 'space';
    else class_ = (i % 2 == 0) ? 'line' : 'space';
    td.setAttribute('class', class_);
    td.onclick = function() {
      setTDHasNote(this, !getTDHasNote(this));
      updateCurrentBoxJSONFromStaff();
    };
    tr.appendChild(td);
  }
}
