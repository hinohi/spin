function XY() {
    var COLS = 100, ROWS = 100; // 800を割りきれる数
    var N = COLS * ROWS;
    var PI2 = 2 * Math.PI;

    var spin = [];
    var net=[];

    var T=0.5;
    var beta=1.0/T;

    var stop=false;
    var colors = [];
    var interval;

    function init() {
        spin = [];
        for (var n = 0; n < N; n++) {
            spin.push(Math.random() * PI2);
        }
        net = [];
        for (var y = 0; y < ROWS; y++) {
            for (var x = 0; x < COLS; x++) {
                net.push([
                    y*COLS + (x+1)%COLS,
                    (y+1)%ROWS*COLS + x,
                    y*COLS + (x-1+COLS)%COLS,
                    (y-1+ROWS)%ROWS*COLS + x
                ]);
            }
        }

        colors = [];
        for (var h = 0; h < 360; h++) {
            var tcolor = hsvToRGB(h, 1, 1);
            var str = "'rgb(";
            str = str.concat(tcolor[0]);
            str = str.concat(",");
            str = str.concat(tcolor[1]);
            str = str.concat(",");
            str = str.concat(tcolor[2]);
            str = str.concat(")'");
            colors.push(eval(str));
        }
    }

    function update() {
        for (var i = 0; i < N; i++) {
            var n = Math.floor(N * Math.random());
            var new_spin = Math.random() * PI2;
            // new_spin = (2*new_spin - spin[n] + PI2)%PI2;
            var E1 = -Math.cos(spin[n] - spin[net[n][0]])
                     -Math.cos(spin[n] - spin[net[n][1]])
                     -Math.cos(spin[n] - spin[net[n][2]])
                     -Math.cos(spin[n] - spin[net[n][3]]);
            var E2 = -Math.cos(new_spin - spin[net[n][0]])
                     -Math.cos(new_spin - spin[net[n][1]])
                     -Math.cos(new_spin - spin[net[n][2]])
                     -Math.cos(new_spin - spin[net[n][3]]);
            if (Math.random() * (1.0 + Math.exp(-beta*(E1-E2))) < 1.0) {
                spin[n] = new_spin;
            }
        }
    }

    function tick() {
        if (stop) return;
        update();
    } 

    function keyPress(key) {
        var div = 1.0 / 128;
        switch (key) {
        case 'left':
            init();
            break;
        case 'right':
            stop = !stop;
            break;
        case 'up':
            T += div;
            beta = 1.0 / T;
            debug_print('T='+T);
            break;
        case 'down':
            T -= div;
            if (T < div) T = div;
            beta = 1.0 / T;
            debug_print('T='+T);
            break;
        }
    }
    function key_callback(e) {
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
    }
    document.onkeydown = key_callback;



    var canvas_spin = document.getElementById('spin');
    var canvas_roll = document.getElementById('roll');
    var ctx_spin = canvas_spin.getContext('2d');
    var ctx_roll = canvas_roll.getContext('2d');

    var W = 800, H = 800;
    var BLOCK_W = W / COLS, BLOCK_H = H / ROWS;

    function drawBlock(ctx, x, y) {
        ctx.fillRect(BLOCK_W * x, BLOCK_H * y, BLOCK_W, BLOCK_H);
    }

    // draws the board and the moving shape
    function render() {
        var fact = 360 / PI2;
        for (var y = 0; y < ROWS; y++) {
            for (var x = 0; x < COLS; x++) {
                ctx_spin.fillStyle = colors[Math.floor(spin[y*COLS+x]*fact)];
                drawBlock(ctx_spin, x, y);
           }
        }
    }
    function render_roll() {
        var fact = 360 / Math.PI / 4;
        for (var y = 0; y < ROWS-1; y++) {
            for (var x = 0; x < COLS-1; x++) {
                var rol = 0.0;
                rol += Math.acos(Math.cos(spin[y*COLS+x+1] - spin[y*COLS+x]))
                rol += Math.acos(Math.cos(spin[(y+1)*COLS+x-1] - spin[y*COLS+x+1]))
                rol += Math.acos(Math.cos(spin[(y+1)*COLS+x] - spin[(y+1)*COLS+x+1]))
                rol += Math.acos(Math.cos(spin[y*COLS+x] - spin[(y+1)*COLS+x]))
                rol = (rol + PI2) * fact;
                ctx_roll.fillStyle = colors[Math.floor(rol)];
                drawBlock(ctx_roll, x, y);
           }
        }
    }

    this.newGame = function() {
        clearInterval(interval);
        init();
        interval = setInterval(tick, 10);
        setInterval(render, 30);
        setInterval(render_roll, 30);
    } 
}

function debug_print(str)
{
    var out = document.getElementById("debug");
    if (!out) return;
    out.value = str;
}

var xy = new XY();
xy.newGame();