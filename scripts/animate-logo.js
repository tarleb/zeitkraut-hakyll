var clock = function() {
  var sDial = document.getElementsByClassName("seconds")[0];
  var mDial = document.getElementsByClassName("minutes")[0];
  var hDial = document.getElementsByClassName("hours")[0];

  function moveDials(hours, minutes, seconds) {
    hDeg = (360 / 12) * hours;
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
  update();
}

setInterval(clock, 50);
