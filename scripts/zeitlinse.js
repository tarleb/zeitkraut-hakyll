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

// convert obfuscated mail addresses into clickable "mailto:" links
jQuery("document").ready(function($) {
  // Transform logos into working clocks
  // setInterval(clock, 50);

  var mailAddressClass = "obfuscated-mail-address";
  $("."+mailAddressClass).each(function() {
    if (!$(this).hasClass("dont-touch")) {
      var address = $(this).attr("data-user") + "@" + $(this).attr("data-domain");
      $(this).html($("<a href=\"mailto:" + address + "\">" + address + "</a>", {}));
      $(this).removeClass(mailAddressClass);
    }
  });
});
