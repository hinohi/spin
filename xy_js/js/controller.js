document.body.onkeydown = function( e ) {
    var keys = {
        37: 'left',  // arrow
        39: 'right', // arrow
        40: 'down',  // arrow
        38: 'up',// arrow
        72: 'left',  // h
        76: 'right', // l
        74: 'down',  // j
        75: 'up' // k
    };
    if ( typeof keys[ e.keyCode ] != 'undefined' ) {
        keyPress( keys[ e.keyCode ] );
        render();
    }
};
