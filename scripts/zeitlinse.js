// convert obfuscated mail addresses into clickable "mailto:" links
jQuery("document").ready(function($) {
  var mailAddressClass = "obfuscated-mail-address";
  $("."+mailAddressClass).each(function() {
    if (!$(this).hasClass("dont-touch")) {
      var address = $(this).attr("data-user") + "@" + $(this).attr("data-domain");
      $(this).html($("<a href=\"mailto:" + address + "\">" + address + "</a>", {}));
      $(this).removeClass(mailAddressClass);
    }
  });
});
