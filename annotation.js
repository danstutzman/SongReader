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
      var pointsJSON = JSON.parse(data);
      for (var j = 0; j < pointsJSON.length; j++) {
        var point = pointsJSON[j];
        createPoint(point.x, point.y, point.type, point.staffY);
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
  points:[],
  newPointMode:false
}

var TABLE_Y_TO_STAFF_Y = -8;

function updateButtonGlows() {
  document.getElementById('newPointButton').style.border =
    (global.newPointMode) ? '3px #0f0 solid' : '';
}

function toggleNewPointMode() {
  if (global.newPointMode)
    global.newPointMode = false;
  else {
    global.newPointMode = true;
    global.newBoxMode = false;
  }
  updateButtonGlows();
}

function updateBoxesJSON() {
  var boxJSON = document.getElementById('box_json');
  boxJSON.value = JSON.stringify(global.points);
}

function createPoint(x, y, pointType, staffY) {
  var div = document.createElement('div');
  var surroundingDiv = document.getElementById('surrounding_div');
  div.setAttribute('class', 'point');
  div.setAttribute('style',
    'position:absolute;left:' + x + 'px;top:' + y + 'px');

  var text = document.createTextNode();
  text.nodeValue = pointType;
  div.appendChild(text);

  surroundingDiv.appendChild(div);

  global.points.push({type:pointType, x:x, y:y, staffY:staffY});
  updateBoxesJSON();
}

function handleClick(x, y) {
  if (global.newPointMode) {
    var pointType = $('input:radio[name=point_type]:checked').val();
    var staffY = $('input:radio[name=staff_y]:checked').val();
    createPoint(x, y, pointType, staffY);
  }
}

function handleOnMouseDown() {
  var e = e ? e : window.event;
  //if (e.targetTouches)
  //  return; // let the touchstart handler take care of it
  var photo = document.getElementById('photo');
  handleClick(e.clientX + document.body.scrollLeft - 12,
    e.clientY + document.body.scrollTop - 15);
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
