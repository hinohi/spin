var canvas = document.getElementsByTagName( 'canvas' )[ 0 ];
var ctx = canvas.getContext( '2d' );

var W = 1200, H = 1200;
var BLOCK_W = W / COLS, BLOCK_H = H / ROWS;

// draw a single square at (x, y)
function drawBlock( x, y ) {
  ctx.fillRect( BLOCK_W * x, BLOCK_H * y, BLOCK_W  , BLOCK_H  );
}

// draws the board and the moving shape
function render() {
//  ctx.clearRect( 0, 0, W, H );
  for ( var y = 0; y < ROWS; ++y ) {
    for ( var x = 0; x < COLS; ++x ) {
      ctx.fillStyle = mycolor[ board[ y ][ x ]  ];
      drawBlock( x, y );
    }
  }
}
// render を30ミリ秒間隔で呼び出す。
setInterval( render, 30 );
