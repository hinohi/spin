var COLS = 300, ROWS = 300; // 1200を割りきれる数

var Moore=true ;
var range=1 ;
var threshold=3 ;
var stateSize=3;

var type =0;

var models = [
    [1,3,3,true],
    [2,11,3,true],
    [3,5,8,true],
    [2,9,4,true],
    [2,4,5,true],
    [5,15,6,true],
    [5,10,8,true],
    [4,9,7,true],
    [1,2,4,true],
    [2,10,3,true],
    [2,3,5,false],
    [1,3,4,true],
    [2,5,8,true],
    [1,1,14,false],
    [3,10,2,false],
    [5,23,2,false],
    [1,1,14,false],
    [2,5,3,false],
    [3,4,5,false],
    [2,2,6,false]
];

var typeNum = models.length;
var board = [];
var board2 = [];

var flag = true;
var interval;
var mycolor = [];

function setParams(type){

  var model = models[type];
  range = model[0];
  threshold = model[1];
  stateSize= model[2];
  Moore = model[3];

  clearDebugPrint();
  DebugPrint(type);
  DebugPrint(": ");
  DebugPrint(model);
//  DebugPrint(": ");
//  DebugPrint(range);
//  DebugPrint(" ");
//  DebugPrint(threshold);
//  DebugPrint(" ");
//  DebugPrint(stateSize);
//  DebugPrint(" ");
//  DebugPrint(Moore);
//  DebugPrint("\n");

// 
//  DebugPrint(threshold);
//  DebugPrint(stateSize);
//  DebugPrint(Moore);

  for(var i = 0; i<stateSize ; ++i) {
    var angl = i*360./stateSize + 200; 
    var tcolor = hsvToRGB(angl,1,1);
    var str = "'rgb(";
    str = str.concat(tcolor[0]);
    str = str.concat(",");
    str = str.concat(tcolor[1]);
    str = str.concat(",");
    str = str.concat(tcolor[2]);
    str = str.concat(")'");

    mycolor[i] = eval(str);
    //DebugPrint(str);
  }

  // random initial configuration
  for (var y=0;y<ROWS;y++){
    for(var x=0;x<COLS;x++){
      var ii = Math.floor( Math.random() * stateSize )
      board[y][x] = ii;
      //DebugPrint(ii);
    }
  }


}

function DebugPrint(str)
{
  var out = document.getElementById("debug");
  if (!out) return;
  out.value += str;
}

function clearDebugPrint(){
  var out = document.getElementById("debug");
  if (!out) return;
  out.value ="";
}

// 初期化
function init() {
  for ( var y = 0; y < ROWS; ++y ) {
    board[ y ] = [];
    for ( var x = 0; x < COLS; ++x ) {
      board[ y ][ x ] = 0;
    }
  }
  for ( var y = 0; y < ROWS; ++y ) {
    board2[ y ] = [];
    for ( var x = 0; x < COLS; ++x ) {
      board2[ y ][ x ] = 0;
    }
  }

  setParams(type);
  copyBoundary();
}

// copy boundary margin
function copyBoundary(){
  for(var y=range;y<ROWS-range;y++){
    for(var x=0;x<range;x++){
      board[y][COLS-range+x] = board[y][x+range];
    }
  }
  for(var y=range;y<ROWS-range;y++){
    for(var x=0;x<range;x++){
      board[y][x] = board[y][COLS-2*range+x];
    }
  }
  for(var y=0;y<range;y++){
    for(var x=0;x<COLS;x++){
      board[ROWS-range+y][x] = board[range+y][x];
    }
  }
  for(var y=0;y<range;y++){
    for(var x=0;x<COLS;x++){
      board[y][x] = board[ROWS-2*range+y][x];
    }
  }
}

function update(){
  for(var y=0;y<ROWS;y++){
    for(var x=0;x<COLS;x++){
      board[y][x] = board2[y][x];
    }
  }
}

function countExitedStates(x,y,s){
  // 励起状態の数を数える
  // 自分自身は、sでないことが上位ループで保証されている
  var count=0;
  if(Moore){
    for(var j=-range;j<range+1;j++){
      for(var i=-range;i<range+1;i++){
	if(board[y+j][x+i]==s){
	  count++;
	}
      }
    }
  } else { // von Neumann
    //clearDebugPrint();
    for(var j=-range;j<range+1;j++){
      for(var i=-range;i<range+1;i++){
	if(Math.abs(i)+Math.abs(j) <= range){
	  //DebugPrint(i);
	  //DebugPrint(" ");
	  //DebugPrint(j);
	  //DebugPrint("\n");
	  if(board[y+j][x+i]==s){
	    count++;
	  }
	}
      }
    }
  }
  //DebugPrint(count);
  return count;
}


function tick() {
  if(flag){
    for(var y=range;y<ROWS-range;y++){
      for(var x=range;x<COLS-range;x++){
	var nextState ;

	for(var s=0;s<stateSize-1;s++){
	  if(board[y][x]==s){
	    if(countExitedStates(x,y,s+1)>=threshold){
	      nextState = s+1;
	    } else {
	      nextState = s;
	    }
	  }
	} 
	if(board[y][x]==stateSize-1){
	  if(countExitedStates(x,y,0)>=threshold){
	    nextState = 0;
	  } else {
	    nextState = stateSize-1;
	  }
	}
	
	board2[y][x] = nextState;
      }
    }
    update();
    copyBoundary();
  }
} 


function keyPress( key ) {
  switch (key) {
  case 'left':
    init();
    break;
  case 'right':
    flag = !flag;
    break;
  case 'down':
    type = (type+1)%typeNum;
    init();
    break;
  case 'up':
    type = (type+typeNum-1)%typeNum;
    init();
    break;
  }
} 

function newGame() {
  clearInterval(interval);
  init();
  interval = setInterval( tick, 100 );
} 

newGame();
