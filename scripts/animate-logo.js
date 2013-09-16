var updateClock = (function() {
  var sDial = document.getElementById("secondDial");
  var mDial = document.getElementById("minuteDial");
  var hDial = document.getElementById("hourDial");

  function moveDials(hours, minutes, seconds) {
    hDeg = (360 / 12) * (hours + minutes/60);
    mDeg = (360 / 60) * minutes;
    sDeg = (360 / 60) * seconds;
    sDial.setAttribute("transform", "rotate("+sDeg+")");
    mDial.setAttribute("transform", "rotate("+mDeg+")");
    hDial.setAttribute("transform", "rotate("+hDeg+")");
  }

  function update() {
    var currentTime = new Date();
    moveDials(currentTime.getHours(), currentTime.getMinutes(), currentTime.getSeconds());
  };

  return update;
})();

// update every 50ms, just to be sure
setInterval(updateClock, 50);
