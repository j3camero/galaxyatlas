
var getStarsInRadius = function(radius, pointX, pointY, pointZ, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/starsInRadius' + '?radius=' + encodeURIComponent(radius) + '&pointX=' + encodeURIComponent(pointX) + '&pointY=' + encodeURIComponent(pointY) + '&pointZ=' + encodeURIComponent(pointZ), true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}

var getVisibleStars = function(minLum, pointX, pointY, pointZ, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/visibleStars' + '?minLum=' + encodeURIComponent(minLum) + '&pointX=' + encodeURIComponent(pointX) + '&pointY=' + encodeURIComponent(pointY) + '&pointZ=' + encodeURIComponent(pointZ), true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}

var getVisibleStarsMagic = function(minLum, blurRad, pointX, pointY, pointZ, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/visibleStarsMagic' + '?minLum=' + encodeURIComponent(minLum) + '&blurRad=' + encodeURIComponent(blurRad) + '&pointX=' + encodeURIComponent(pointX) + '&pointY=' + encodeURIComponent(pointY) + '&pointZ=' + encodeURIComponent(pointZ), true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}
