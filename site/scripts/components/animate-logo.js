var updateClock = (function() {
  var sDial = document.querySelector(".logo-second-dial");
  var mDial = document.querySelector(".logo-minute-dial");
  var hDial = document.querySelector(".logo-hour-dial");

  function moveDials(hours, minutes, seconds) {
    var hDeg = (360 / 12) * (hours + minutes/60);
    var mDeg = (360 / 60) * minutes;
    var sDeg = (360 / 60) * seconds;
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
