$(document).ready(function() {
  $.ajax({
    type:'GET',
    url:'/boxes.json',
    dataType: 'text',
    success: function(data) {
      var boxes = JSON.parse(data);
      for (var i = 0; i < boxes.length; i++) {
        var box = boxes[i];
        createBox(global.nextBoxNum,
          box.left, box.top, box.width, box.height, box.notes);
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
  currentBoxNum:null
}

function updateButtonGlows() {
  document.getElementById('addButton').style.border =
    (global.addMode) ? '3px #0f0 solid' : '';
  document.getElementById('deleteButton').style.border =
    (global.deleteMode) ? '3px #0f0 solid' : '';
}

function toggleAddMode() {
  if (global.addMode)
    global.addMode = false;
  else {
    global.addMode = true;
    global.deleteMode = false;
  }
  var photo = document.getElementById('photo')
  var corner = document.getElementById('corner');
  if (global.addMode) {
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
  updateButtonGlows();
  return false;
}

function toggleDeleteMode() {
  if (global.deleteMode)
    global.deleteMode = false;
  else {
    global.deleteMode = true;
    global.addMode = false;
  }
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

  global.boxes.push(
    {num:num, left:left, top:top, width:width, height:height,
     notes:notes});

  updateBoxesJSON();
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

function handleClick(x, y) {
  if (global.addMode) {
    var photo = document.getElementById('photo');
    createBox(global.nextBoxNum, x, y, 50, 50, []);
    var corner = document.getElementById('corner');
    corner.style.display = 'none';
  }
  else if (global.deleteMode) {
    var foundBox = findBoxIncludingPoint(x, y);
    if (foundBox) {
      foundBox.style.display = 'none';
      global.deleteMode = false;
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
  if (global.addMode) {
    var boxNum = global.nextBoxNum - 1;
    var box = document.getElementById('box' + boxNum);
    box.style.width  = (x - parseInt(box.style.left)) + 'px';
    box.style.height = (y - parseInt(box.style.top)) + 'px';
    global.boxes[boxNum].width = x - global.boxes[boxNum].left;
    global.boxes[boxNum].height = y - global.boxes[boxNum].height;
  }
}
function handleOnMouseDown() {
  var e = e ? e : window.event;
  //if (e.targetTouches)
  //  return; // let the touchstart handler take care of it
  var photo = document.getElementById('photo');
  handleClick(e.clientX + document.body.scrollLeft,
    e.clientY + document.body.scrollTop);
  photo.onmousemove = function(e) {
    var e = e ? e : window.event;
    handleDrag(e.clientX + document.body.scrollLeft,
      e.clientY + document.body.scrollTop);
  };
  photo.onmouseup = function(e) {
    if (global.addMode) {
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
          notes[j].push(i);
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
        var y = notesForX[i];
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
    td.setAttribute('class', (i % 2 == 1) ? 'line' : 'space');
    td.onclick = function() {
      setTDHasNote(this, !getTDHasNote(this));
      updateCurrentBoxJSONFromStaff();
    };
    tr.appendChild(td);
  }
}
