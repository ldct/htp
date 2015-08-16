var sock = new WebSocket("ws://localhost:9160");
window.pos = 0;

$("#submitSource").on("click", function() {
  var source = $("#source").val();
  sock.send($("#source").val());

  sock.onmessage = function(m) {
    a = m;
    if (m.data == "recieved source") {
      $("#screen1").hide();
      $("#screen2").show();
      var screen = $("#screen2");
      var left = screen.find("#left");
      var right = screen.find("#right");

      source = source.split("\n");
      _.times(source.length, function(i) {
        $("<input></input>").appendTo(left).attr("id", "line_" + i);
        $("<button>Change Line</button>").appendTo(left).attr("id", "change_" + i);
        $("#line_"+i).val(source[i]);
        $("<br>").appendTo(left);
        $("#change_" + i).on("click", function() {
          if (i < pos)
            return;
          sock.send("r " + (i - pos) + " " + $("#line_" + i).val());
        })
      });

      $("<div></div>").appendTo(right).attr("id", "stdout").html("<h3>Stdout</h3>");
    } else if (m.data.split("\n")[0].indexOf("stdout") > -1) {
      $("#stdout").html("<h3>Stdout</h3>" + m.data.split("\n").slice(1).join("<br>"));
    } else if (m.data.split("\n")[0].indexOf("linesRemaining") > -1) {
      var num = parseInt(m.data.split("\n")[1]);
      window.pos = num;
      $("#pos").remove();
      $("<div id='pos'>----</div>").insertAfter("#change_" + (num - 1));
    } else if (m.data.split("\n")[0].indexOf("prog") > -1) {
      var text = m.data.split("\n").slice(1);
      _.times(source.length, function(i) {
        $("#line_" + i).val(text[i]);
      });
    }
  }
});

$("#stepForward").on("click", function() {
  sock.send("f");
});

$("#stepBackward").on("click", function() {
  sock.send("b");
});
