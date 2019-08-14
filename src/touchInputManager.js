export default function touchInputManager(app) {
  new TouchInputManager(app);
}

function TouchInputManager(app) {
  this.events = {};
  this.app = app;

  if (window.navigator.msPointerEnabled) {
    //Internet Explorer 10 style
    this.eventTouchstart = "MSPointerDown";
    this.eventTouchmove = "MSPointerMove";
    this.eventTouchend = "MSPointerUp";
  } else {
    this.eventTouchstart = "touchstart";
    this.eventTouchmove = "touchmove";
    this.eventTouchend = "touchend";
  }

  this.listen();
}

TouchInputManager.prototype.listen = function () {
  var self = this;

  // Respond to swipe events
  var touchStartClientX, touchStartClientY;
  var startTime;
  var gameContainer = document.getElementsByClassName("game-container")[0];

  gameContainer.addEventListener(this.eventTouchstart, function (event) {
    if ((!window.navigator.msPointerEnabled && event.touches.length > 1) ||
      event.targetTouches.length > 1) {
      return; // Ignore if touching with more than 1 finger
    }
    startTime = +new Date();
    if (window.navigator.msPointerEnabled) {
      touchStartClientX = event.pageX;
      touchStartClientY = event.pageY;
    } else {
      touchStartClientX = event.touches[0].clientX;
      touchStartClientY = event.touches[0].clientY;
    }

    event.preventDefault();
  });

  gameContainer.addEventListener(this.eventTouchmove, function (event) {
    var touchMoveClientX, touchMoveClientY;
    var now = +new Date();
    var diffTime = now - startTime;
    if (window.navigator.msPointerEnabled) {
      touchMoveClientX = event.pageX;
      touchMoveClientY = event.pageY;
    } else {
      touchMoveClientX = event.changedTouches[0].clientX;
      touchMoveClientY = event.changedTouches[0].clientY;
    }

    var dx = touchMoveClientX - touchStartClientX;
    var absDx = Math.abs(dx);

    var dy = touchMoveClientY - touchStartClientY;
    var absDy = Math.abs(dy);
    if (diffTime >= 100) {
      if (Math.max(absDx, absDy) > 30) {
        var direction = absDx > absDy ?
          (dx > 0 ? "ArrowRight" : "ArrowLeft") :
          (dy > 0 ? "ArrowDown" : "ArrowUp");
        self.app.ports.swipeDirectionArrow.send({ key: direction, movex: absDx, movey: absDy });
        touchStartClientY = touchMoveClientY;
        touchStartClientX = touchMoveClientX;
      }
    }
    event.preventDefault();
  });

  gameContainer.addEventListener(this.eventTouchend, function (event) {
    if ((!window.navigator.msPointerEnabled && event.touches.length > 0) ||
      event.targetTouches.length > 0) {
      return; // Ignore if still touching with one or more fingers
    }

    var touchEndClientX, touchEndClientY;
    var now = +new Date();
    var diffTime = now - startTime;
    if (window.navigator.msPointerEnabled) {
      touchEndClientX = event.pageX;
      touchEndClientY = event.pageY;
    } else {
      touchEndClientX = event.changedTouches[0].clientX;
      touchEndClientY = event.changedTouches[0].clientY;
    }

    var dx = touchEndClientX - touchStartClientX;
    var absDx = Math.abs(dx);

    var dy = touchEndClientY - touchStartClientY;
    var absDy = Math.abs(dy);
    if (diffTime < 100) {
      if (Math.max(absDx, absDy) > 10) {
        var direction = absDx > absDy ?
          (dx > 0 ? "ArrowRight" : "ArrowLeft") :
          (dy > 0 ? "SwipeDown" : "ArrowUp");
        self.app.ports.swipeDirectionArrow.send({ key: direction, movex: absDx, movey: absDy });
      } else {
        self.app.ports.swipeDirectionArrow.send({ key: "Tap", movex: 0, movey: 0 });
      }
    } else {
      
    }
  });
};
